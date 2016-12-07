module TestEvaluation
  (evaluationTests)
where

import Parsing(readExpr)
import Evaluation
import Data (LispVal(..))

import Test.Hspec
import Test.QuickCheck

import TestArbitraryData()

evaluationTests :: SpecWith ()
evaluationTests = describe "Evaluation Tests" basicEval


readAndEval :: String -> LispVal
readAndEval = eval . readExpr


basicEval :: SpecWith ()
basicEval = describe "Basic eval tests" $ do
  it "eval numbers to themselves " $ property numbersEvalToNumber
  it "eval strings to themselves " $ property stringsEvalToNumber
  it "eval booleans to themselves " $ property boolsEvalToNumber
  it "eval quoted expr to expr itself" $ property quotedExprToUnQuoted
  arithmeticTest


numbersEvalToNumber :: Integer -> Bool
numbersEvalToNumber n =
  eval number == number
  where number = Number n


stringsEvalToNumber :: String -> Bool
stringsEvalToNumber s =
  eval string == string
  where string = String s

boolsEvalToNumber :: Bool -> Bool
boolsEvalToNumber b =
  eval bool == bool
  where bool = Bool b

quotedExprToUnQuoted :: LispVal -> Bool
quotedExprToUnQuoted expr =
  eval (List [ Atom "quote", expr]) == expr


arithmeticTest :: SpecWith ()
arithmeticTest =
  it "correctly evals: (+ 2 4 (- 5 3) (* 1 3)) -> 11" $
    readAndEval "(+ 2 4 (- 5 3) (* 1 3))" `shouldBe` Number 11
