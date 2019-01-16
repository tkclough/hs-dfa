module Main where

import DFA.DFA 
import DFA.Parser 
import DFA.QuickCheck
import TM 

import Test.QuickCheck 
import System.Environment

main :: IO ()
main = do
  [re, fd] <- getArgs

  putStrLn $ "testing dfa on regexp: " ++  re
  eitherDfa <- defaultParseDFAFile fd 

  case eitherDfa of 
    Left e -> print e
    Right dfa -> verboseCheck (propRegex re dfa)