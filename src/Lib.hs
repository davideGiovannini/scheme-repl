module Lib
    ( replFunc
    )
where

import System.IO

import Parsing(readExpr)
import Evaluation(eval)

import Control.Monad(unless)
import Control.Monad.Except

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: IO String
readPrompt = flushStr "λ:" >> getLine

readAndEval :: String -> IO ()
readAndEval expr =
  case runExcept replAction of
    Right val -> flushStr "> " >> print val
    Left val  -> flushStr "! " >> print val
  where replAction = readExpr expr >>= eval


replFunc :: IO ()
replFunc = do
  input <- readPrompt
  unless (input == "quit") $
    readAndEval input >> replFunc
