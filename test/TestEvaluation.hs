module TestEvaluation
  (evaluationTests)
where

import Parsing(readExpr)
import Evaluation
import Data (LispVal(..), ThrowsError)

import Test.Hspec
import Test.QuickCheck

import TestArbitraryData()
import Control.Monad.Except(runExcept)

evaluationTests :: SpecWith ()
evaluationTests = describe "Evaluation Tests" basicEval

readAndEval :: String -> ThrowsError LispVal
readAndEval expr = readExpr expr >>= eval

evaluatesTo :: ThrowsError LispVal -> LispVal -> Bool
evaluatesTo val value = runExcept val == Right value


basicEval :: SpecWith ()
basicEval = describe "Basic eval tests" $ do
  it "eval numbers to themselves " $ property numbersEvalToNumber
  it "eval strings to themselves " $ property stringsEvalToNumber
  it "eval booleans to themselves " $ property boolsEvalToNumber
  it "eval quoted expr to expr itself" $ property quotedExprToUnQuoted
  arithmeticTest
  it "needs more test!" $ False `shouldBe` True


numbersEvalToNumber :: Integer -> Bool
numbersEvalToNumber n =
  eval number `evaluatesTo` number
  where number = Number n


stringsEvalToNumber :: String -> Bool
stringsEvalToNumber s =
  eval string `evaluatesTo` string
  where string = String s

boolsEvalToNumber :: Bool -> Bool
boolsEvalToNumber b =
  eval bool `evaluatesTo` bool
  where bool = Bool b

quotedExprToUnQuoted :: LispVal -> Bool
quotedExprToUnQuoted expr =
  eval (List [ Atom "quote", expr]) `evaluatesTo` expr


arithmeticTest :: SpecWith ()
arithmeticTest =
  it "correctly evals: (+ 2 4 (- 5 3) (* 1 3)) -> 11" $
    readAndEval "(+ 2 4 (- 5 3) (* 1 3))" `evaluatesTo` Number 11
