{-# LANGUAGE OverloadedStrings #-}

module DFA.QuickCheck where 

import Test.QuickCheck
import DFA.DFA
import Data.Text

prop_MatchesSpec :: ([Text] -> Bool) -> DFA Text Text -> NonEmptyList Text -> Bool
prop_MatchesSpec pred mach input = pred (getNonEmpty input) == either (const False) id (runDFA mach (getNonEmpty input))

binary :: Gen Text 
binary = (pack . show) <$> choose (0 :: Int, 1)

binaryList :: Gen [Text]
binaryList = listOf binary 

instance Arbitrary Text where 
    arbitrary = binary