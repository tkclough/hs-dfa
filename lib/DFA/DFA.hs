{-# LANGUAGE FlexibleContexts, OverloadedStrings, ExistentialQuantification #-}

module DFA.DFA where

import Control.Monad.State 
import Control.Monad.Reader
import Control.Monad.Except

import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe (isNothing, fromJust)
import Data.Text (Text, pack, append, unwords)
import Data.Void

import Prelude hiding (lookup, unwords)

import qualified Data.Map as M 
import qualified Data.Set as S
import qualified Data.Text.IO as T

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data DFA q a = DFA {
    states :: Set q,
    alphabet :: Set a,
    transition :: Map (q, a) q,
    initial :: q,
    accept :: Set q
} deriving (Show, Eq)

-- Q0,Q1,Q2 | 0,1 | (Q0,0)->Q0, (Q0,1)->Q1, (Q1,0)->Q2, (Q1,1)->Q2, (Q2,0)->Q0, (Q0,1)->Q1 | Q0 | Q0

data DFAState q a = DFAState {
    currentState :: q,
    input :: [a]
} deriving (Show, Eq)

data DFAError q a = NoMember (q, a) 
                  | NotInAlphabet a deriving (Show, Eq)

accepts :: (Ord a, Ord q, MonadState (DFAState q a) m, MonadReader (DFA q a) m, MonadError (DFAError q a) m) => m Bool
accepts = do 
    finished <- null <$> gets input 
    if finished 
        then S.member <$> gets currentState <*> asks accept -- we're done, check if in accepting state
        else do 
            (x:xs) <- gets input 
            current <- gets currentState

            isInAlphabet <- S.member x <$> asks alphabet
            unless isInAlphabet $ throwError (NotInAlphabet x)

            let key = (current, x)

            maybeNextState <- M.lookup key <$> asks transition
            when (isNothing maybeNextState) $ throwError (NoMember key) -- no matching entry; throw

            let nextState = fromJust maybeNextState
            put (DFAState nextState xs)

            accepts 

runDFA :: (Ord q, Ord a) => DFA q a -> [a] -> Either (DFAError q a) Bool
runDFA mach input = runExcept . flip runReaderT mach . flip evalStateT initState $ accepts 
    where initState = DFAState (initial mach) input


-- test
dfa1 str = runExcept . flip runReaderT dfa . flip runStateT (DFAState "Q0" str) $ accepts 
    where dfa = DFA (S.fromList ["Q0", "Q1", "Q2"])
                    (S.fromList [0, 1])
                    (M.fromList [(("Q0", 0), "Q1"),
                                 (("Q0", 1), "Q1"),
                                 (("Q1", 0), "Q2"),
                                 (("Q1", 1), "Q2"),
                                 (("Q2", 0), "Q0"),
                                 (("Q2", 1), "Q0")])
                    "Q0" 
                    (S.fromList ["Q0"])

           
dfa :: (Ord e, MonadParsec e Text m) => m (DFA Text Text)
dfa = DFA 
    <$> ((S.fromList <$> statesParser) <* pipe)
    <*> ((S.fromList <$> alphabetParser) <* pipe)
    <*> ((M.fromList <$> transitionParser) <* pipe)
    <*> (initialParser <* pipe)
    <*> (S.fromList <$> acceptParser)

pipe :: MonadParsec e Text m => m Char
pipe = space *> char '|' <* space

symbolParser :: MonadParsec e Text m => m Text
symbolParser = pack <$> some alphaNumChar

statesParser :: MonadParsec e Text m => m [Text]
statesParser = sepEndBy1 symbolParser (space *> char ',' <* space)

alphabetParser :: MonadParsec e Text m => m [Text]
alphabetParser = sepEndBy1 symbolParser (space *> char ',' <* space)

transitionParser :: MonadParsec e Text m => m [((Text, Text), Text)]
transitionParser = sepEndBy1 relation (space *> char ',' <* space)
    where relation = (,) 
            <$> (char '(' *> ((,) <$> symbolParser <*> (char ',' *> symbolParser)) <* char ')')
            <*> (string "->" *> symbolParser)

initialParser :: MonadParsec e Text m => m Text
initialParser = symbolParser

acceptParser :: MonadParsec e Text m => m [Text]
acceptParser = statesParser