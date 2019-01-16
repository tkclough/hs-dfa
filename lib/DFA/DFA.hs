{-# LANGUAGE FlexibleContexts, OverloadedStrings, ExistentialQuantification #-}

module DFA.DFA where

import Control.Monad.State 
import Control.Monad.Reader
import Control.Monad.Except

import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe (isNothing, fromJust)
import Data.Text (Text, pack)

import Prelude hiding (lookup, unwords)

import qualified Data.Map as M 
import qualified Data.Set as S

import Text.Megaparsec
import Text.Megaparsec.Char

-- | Representation of a deterministic finite automaton, a finite-state machine 
-- | that recognize regular languages.
data DFA q a = DFA {
    states :: Set q,            -- ^ a finite set of states
    alphabet :: Set a,          -- ^ a finite set of input symbols
    transition :: Map (q, a) q, -- ^ a transition function
    initial :: q,               -- ^ an initial state
    accept :: Set q             -- ^ a set of accept states
} deriving (Show, Eq)

-- | Representation of the current state of the DFA.
data DFAState q a = DFAState {
    currentState :: q, -- ^ the current state of the DFA
    input :: [a]       -- ^ the rest of the input to consume
} deriving (Show, Eq)

-- | Encode errors that can occur in simulation.
data DFAError q a = NoMember (q, a) 
                  | NotInAlphabet a deriving (Show, Eq)

-- | Determine whether a given DFA will accept some given input.
accepts :: (Ord a, Ord q, MonadState (DFAState q a) m, MonadReader (DFA q a) m, MonadError (DFAError q a) m) => m Bool
accepts = do 
    finished <- null <$> gets input 
    if finished 
        then inAcceptState
        else loop >> accepts
            
-- | Check if in accept state.
inAcceptState :: (Ord a, Ord q, MonadState (DFAState q a) m, MonadReader (DFA q a) m) => m Bool
inAcceptState = S.member <$> gets currentState <*> asks accept

-- | Throw an exception if the provided symbol isn't in the alphabet.
checkAlphabet :: (Ord a, Ord q, MonadReader (DFA q a) m, MonadError (DFAError q a) m) => a -> m ()
checkAlphabet x = do
  isInAlphabet <- S.member x <$> asks alphabet
  unless isInAlphabet $ throwError (NotInAlphabet x)

-- | Attempt to lookup (state, symbol) pair; return if found, or throw otherwise.
lookupOrThrow :: (Ord a, Ord q, MonadReader (DFA q a) m, MonadError (DFAError q a) m) => (q, a) -> m q
lookupOrThrow key = do
  maybeNextState <- M.lookup key <$> asks transition
  when (isNothing maybeNextState) $ throwError (NoMember key)
  return (fromJust maybeNextState)

-- | Body of accepts function; attempt to transition to another state or throw an error.
loop :: (Ord a, Ord q, MonadState (DFAState q a) m, MonadReader (DFA q a) m, MonadError (DFAError q a) m) => m ()
loop = do
  (x:xs) <- gets input
  current <- gets currentState

  checkAlphabet x -- if this symbol isn't recognized, throw

  nextState <- lookupOrThrow (current, x) -- either get the next state, or throw
  put (DFAState nextState xs) -- update the state


-- | Run a simulation on a machine and some input.
runDFA :: (Ord q, Ord a) => DFA q a -> [a] -> Either (DFAError q a) Bool
runDFA mach input = runExcept . 
                    flip runReaderT mach . 
                    flip evalStateT initState $ 
                    accepts 
    where initState = DFAState (initial mach) input
