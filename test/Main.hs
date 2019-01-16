import Test.Tasty
import Test.Tasty.HUnit
import System.Exit
import Control.Exception

import qualified Data.Set as S
import qualified Data.Map as M 

import DFA.DFA
import DFA.Parser
import Data.Either (isRight)

evenZerosDFA :: DFA String Int
evenZerosDFA = DFA (S.fromList ["q0", "q1"])
                   (S.fromList [0, 1])
                   (M.fromList [(("q0", 0), "q1"),
                                (("q0", 1), "q0"),
                                (("q1", 0), "q0"),
                                (("q1", 1), "q1")])
                   "q0"
                   (S.fromList ["q0"])

dfaAccepts :: (Ord q, Ord a) => DFA q a -> [a] -> Bool
dfaAccepts mach input  = case runDFA mach input of 
    Left err -> False
    Right b -> b

dfaFails :: (Ord q, Ord a) => DFA q a -> [a] -> Bool
dfaFails mach input = case runDFA mach input of 
    Left _ -> True
    _ -> False


evenZerosTests = testGroup "Simulation Tests"
  [ testCase "Empty input" $
      dfaAccepts evenZerosDFA [] @?= True,
    testCase "One zero" $
      dfaAccepts evenZerosDFA [0, 1] @?= False,
    testCase "Four zeros" $
      dfaAccepts evenZerosDFA [1, 0, 0, 0, 0] @?= True,
    testCase "Invalid symbols" $
      dfaAccepts evenZerosDFA [2] @?= True
  ]

dfa3 :: DFA String Char
dfa3 = DFA (S.fromList ["q0", "q1"])
           (S.fromList "01")
           (M.fromList [(("q0", '0'), "q0"),
                        (("q0", '1'), "q1"),
                        (("q1", '0'), "q1"),
                        (("q1", '1'), "q1")])
           "q0"
           (S.fromList ["q1"])

parserTests = testGroup "Parser Unit Tests"
  [  testCase "dfa3.txt" $
       do eitherMach <- defaultParseDFAFile "dfa3.txt"
          assertBool "machine read properly" (isRight eitherMach)
          let mach = (\(Right x) -> x) eitherMach
          assertBool "machine matches hardcoded" (mach == dfa3)
  ]

unitTests = testGroup "Unit Tests" [evenZerosTests, parserTests]
main = defaultMain unitTests