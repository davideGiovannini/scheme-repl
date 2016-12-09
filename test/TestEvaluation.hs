module TestEvaluation
  (evaluationTests)
where

import Parsing(readExpr)
import Evaluation
import Data (LispVal(..))

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic(monadicIO, assert, pick, run)

import TestArbitraryData()
import Control.Monad.Except(runExceptT, liftIO)


evaluationTests :: SpecWith ()
evaluationTests = describe "Evaluation Tests"
                       basicEval

(⊢) :: LispVal -> LispVal -> IO Bool
toEval ⊢ value = ((==) <$> runExceptT evalValue) <*> (return . Right $ value)
                  where evalValue = evalEmptyEnv toEval

shouldEvaluateTo :: String -> LispVal -> Expectation
shouldEvaluateTo action result =
  runExceptT evalued `shouldReturn` Right result
  where evalued = readAndEval action
        readAndEval expr = do
          parsed <- liftThrows $ readExpr expr
          env <- liftIO emptyEnv
          eval env parsed


evalEmptyEnv :: LispVal -> IOThrowsError LispVal
evalEmptyEnv val = do
  env <- liftIO emptyEnv
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
  arithmeticTest

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
arithmeticTest =
  it "correctly evals: (+ 2 4 (- 5 3) (* 1 3)) -> 11" $
    "(+ 2 4 (- 5 3) (* 1 3))" `shouldEvaluateTo` Number 11
