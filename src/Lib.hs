module Lib
    ( runRepl
    )
where

import System.IO

import Parsing(readExpr)
import Evaluation(eval, Env, emptyEnv, liftThrows)

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
runRepl = emptyEnv >>= replFunc
