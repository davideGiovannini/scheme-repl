module Main where

import Lib
import System.Environment(getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args then runRepl else runOne args
