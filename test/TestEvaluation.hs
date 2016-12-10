module TestEvaluation
  (evaluationTests)
where

import Parsing(readExpr)
import Evaluation
import Data (LispVal(..), IOThrowsError)

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic(monadicIO, assert, pick, run)

import TestArbitraryData()
import Control.Monad.Except(runExceptT, liftIO)


evaluationTests :: SpecWith ()
evaluationTests = describe "Evaluation Tests" $ do
                       basicEval
                       arithmeticTest
                       testFunctionAndClosures

(⊢) :: LispVal -> LispVal -> IO Bool
toEval ⊢ value = ((==) <$> runExceptT evalValue) <*> (return . Right $ value)
                  where evalValue = evalEmptyEnv toEval

shouldEvaluateTo :: String -> LispVal -> Expectation
shouldEvaluateTo action result =
  runExceptT evalued `shouldReturn` Right result
  where evalued = readAndEval action
        readAndEval expr = liftThrows (readExpr expr) >>= evalEmptyEnv


evalEmptyEnv :: LispVal -> IOThrowsError LispVal
evalEmptyEnv val = do
  env <- liftIO primitiveEnv
  eval env val


propertyM :: (Arbitrary a, Show a) => (a-> IO Bool) -> Property
propertyM prop = monadicIO $ do
  testdata <- pick arbitrary
  result <- run $ prop testdata
  assert result


basicEval :: Spec
basicEval = describe "Basic eval tests" $ do
  it "eval numbers to themselves " $ propertyM numbersSelfEval
  it "eval strings to themselves " $ propertyM stringsSelfEval
  it "eval booleans to themselves " $ propertyM boolsSelfEval
  it "eval quoted expr to expr itself" $ propertyM quotedExprToUnQuoted

numbersSelfEval :: Integer -> IO Bool
numbersSelfEval n =
  number ⊢ number
  where number = Number n


stringsSelfEval :: String -> IO Bool
stringsSelfEval s =
  string ⊢ string
  where string = String s

boolsSelfEval :: Bool -> IO Bool
boolsSelfEval b =
  bool ⊢ bool
  where bool = Bool b

quotedExprToUnQuoted :: LispVal -> IO Bool
quotedExprToUnQuoted expr =
  List [Atom "quote", expr] ⊢ expr


arithmeticTest :: Spec
arithmeticTest = describe "Evaluates complex expressions:" $
  it "correctly evals: (+ 2 4 (- 5 3) (* 1 3)) -> 11" $
    "(+ 2 4 (- 5 3) (* 1 3))" `shouldEvaluateTo` Number 11



testFunctionAndClosures :: Spec
testFunctionAndClosures = describe "Declaring variables, functions and closures" $ do
  it "can declare a var" $
    "(begin (define var 45) var)" `shouldEvaluateTo` Number 45
  it "can declare a function (define (factorial x) (...)) (factorial 10)" $
    "(begin (define (factorial x) (if (= x 1) 1 (* x (factorial (- x 1))))) (factorial 10))" `shouldEvaluateTo` Number 3628800
  it "can declare a closure (counter example" $
    ("(begin (define (counter inc) (lambda (x) (set! inc (+ x inc)) inc))" ++
    " (define my-count (counter 5))" ++
    " (my-count 3) (my-count 6) (my-count 5))") `shouldEvaluateTo` Number 19
