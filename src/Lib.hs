module Lib
    (
      runRepl
    , runOne
    )
where

import System.IO

import Parsing(readExpr)
import Evaluation(eval, liftThrows, primitiveEnv, bindVars)
import Data(Env, LispVal(..))

import Control.Monad(unless)
import Control.Monad.Except

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: IO String
readPrompt = flushStr "λ:" >> getLine

readAndEval :: Env -> String -> IO ()
readAndEval env expr = do
  result <- runExceptT replAction
  case result of
    Right val -> flushStr "> " >> print val
    Left val  -> flushStr "! " >> print val
  where replAction = liftThrows (readExpr expr) >>= eval env


replFunc :: Env -> IO ()
replFunc env = do
  input <- readPrompt
  unless (input == "quit") $
    readAndEval env input >> replFunc env

runRepl :: IO ()
runRepl = primitiveEnv >>= replFunc

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveEnv >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    runIOThrows (show <$> eval env (List [Atom "load", String (head args)]))
        >>= hPutStrLn stderr
    where runIOThrows action = either show show <$> runExceptT action
