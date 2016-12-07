module TestArbitraryData where

import Test.QuickCheck
import Data(LispVal(..))

instance Arbitrary LispVal where
  arbitrary = let arbitraryList         = resize 3 $ listOf arbitrary
                  arbitraryListNotEmpty = resize 3 $ listOf1 arbitrary
                  arbitraryString       = resize 20 arbitrary
              in
              frequency [
                      (3, String     <$> arbitraryString)
                    , (3, Bool       <$> arbitrary)
                    , (3, Number     <$> arbitrary)
                    -- , Atom        <$> arbitraryString -- TODO fix random garbage chars
                    , (1, List       <$> arbitraryList)
                    , (1, DottedList <$> arbitraryListNotEmpty <*> arbitrary)
                    ]
