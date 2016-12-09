module TestPrimitives
  (primitivesTests)
where

import Data (LispVal(..))
import Evaluation.Primitives

import Test.Hspec
import Test.QuickCheck

import TestArbitraryData()
import TestPrimitives.NumberTests
import TestPrimitives.Utils


primitivesTests :: Spec
primitivesTests = describe "Primitive functions Tests" $ do
                       equalityTests
                       testNumberPrimitives


equalityTests :: Spec
equalityTests = describe "Equal? and eqv tests" $
  it "a lispval is equal to itself" $ property testSelfEquals


testSelfEquals :: LispVal -> Bool
testSelfEquals val = equal [val, val] ===> Bool True
