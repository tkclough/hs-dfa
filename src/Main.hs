module Main where

import DFA.DFA 
import DFA.Parser 
import DFA.QuickCheck

import Test.QuickCheck 
import System.Environment

main :: IO ()
main = do
  [re, fd] <- getArgs
  eitherDfa <- defaultParseDFAFile fd 

  case eitherDfa of 
    Left e -> print e
    Right dfa -> verboseCheck (propRegex re dfa)