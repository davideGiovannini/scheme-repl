module TestPrimitives.NumberTests
  (testNumberPrimitives)
where

import Data (LispVal(..), LispFunction)
import Evaluation.Primitives

import Test.Hspec
import Test.QuickCheck

import TestArbitraryData()
import TestPrimitives.Utils((===>))


testNumberPrimitives :: SpecWith()
testNumberPrimitives = describe "Number functions with n>=2 arguments" $ do
  it "addition between numbers works"    $ property testPlus
  it "subtraction between numbers works" $ property testMinus
  it "multiplication between numbers works" $ property testMul
  it "division between numbers works" $ property testDiv
  it "modulo operator on numbers works" $ property testModulo
  it "remainder operator on numbers works" $ property testRemainder
  it "equality test between numbers works" $ property testNumEq
  it "inequality test between numbers works" $ property testNumNotEq
  it "> test between numbers works" $ property testNumGreaterThan
  it "< test between numbers works" $ property testNumLessThan
  it ">= test between numbers works" $ property testNumGreaterEq
  it "<= test between numbers works" $ property testNumLessEq

type AtLeast2Nums = Integer -> Integer -> [Integer] -> Bool

testNumToNumLispFun :: LispFunction -> (Integer -> Integer -> Integer) -> AtLeast2Nums
testNumToNumLispFun lispfun fun x y xs =
    let numbers = Number x:Number y:map Number xs
        result = foldl1 fun $ x:y:xs
    in
    lispfun numbers ===> Number result


testNumToBoolLispFun :: LispFunction -> (Integer -> Integer -> Bool) -> AtLeast2Nums
testNumToBoolLispFun lispfun fun x y xs =
    let numbers = Number x:Number y:map Number xs
        result = all (fun x) $ y:xs
    in
    lispfun numbers ===> Bool result

testPlus :: AtLeast2Nums
testPlus = testNumToNumLispFun numPlus (+)

testMinus :: AtLeast2Nums
testMinus = testNumToNumLispFun numMinus (-)

testMul :: AtLeast2Nums
testMul = testNumToNumLispFun numMul (*)

testDiv :: AtLeast2Nums
testDiv = testNumToNumLispFun numDiv div

testModulo :: AtLeast2Nums
testModulo = testNumToNumLispFun numMod mod

testRemainder :: AtLeast2Nums
testRemainder = testNumToNumLispFun numRem rem

testNumEq :: AtLeast2Nums
testNumEq = testNumToBoolLispFun numEq (==)

testNumNotEq :: AtLeast2Nums
testNumNotEq = testNumToBoolLispFun numNotEq (/=)

testNumLessThan :: AtLeast2Nums
testNumLessThan = testNumToBoolLispFun numLessT (<)

testNumGreaterThan :: AtLeast2Nums
testNumGreaterThan = testNumToBoolLispFun numGreatT (>)

testNumGreaterEq :: AtLeast2Nums
testNumGreaterEq = testNumToBoolLispFun numGE (>=)

testNumLessEq :: AtLeast2Nums
testNumLessEq = testNumToBoolLispFun numLE (<=)
