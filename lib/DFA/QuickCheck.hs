{-# LANGUAGE OverloadedStrings #-}

module DFA.QuickCheck where 

import Test.QuickCheck
import DFA.DFA
import Data.Text
import Text.Regex
import Data.Maybe (isJust)
import Data.Char (chr, ord)

prop_MatchesSpec :: ([Text] -> Bool) -> DFA Text Text -> NonEmptyList Text -> Bool
prop_MatchesSpec pred mach input = pred (getNonEmpty input) == either (const False) id (runDFA mach (getNonEmpty input))

newtype Binary = Binary String deriving (Show, Eq)

binary :: Gen Char 
binary = (chr . (+ ord '0')) <$> choose (0 :: Int, 1)

binaryList :: Gen Binary
binaryList = Binary <$> listOf binary 


propRegex :: Ord q => String -> DFA q Char -> Binary -> Bool 
propRegex re mach (Binary input) = 
    let re' = mkRegex re 
        reRes = isJust . matchRegex re' $ input
        dfaRes = runDFA mach input in 
            reRes == either (const False) id dfaRes


instance Arbitrary Binary where 
    arbitrary = binaryList