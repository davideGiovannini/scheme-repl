module Lib
    ( replFunc
    ) where

import Parsing(readExpr)
import Evaluation(eval)

import Control.Monad(forever)
import Control.Monad.Except

replFunc :: IO ()
replFunc = forever $ do
    putStrLn "λ:"
    input <- getLine
    let parsed = readExpr input
    let evalued = parsed >>= eval
    case runExcept evalued of
      Right right -> print right
      Left left   -> print left
