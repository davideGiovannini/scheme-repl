module Lib
    ( replFunc
    ) where

import Parsing(readExpr)
import Evaluation(eval)

import Control.Monad(forever)


(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) = flip (.)

replFunc :: IO ()
replFunc = forever $
  putStrLn "λ:" >>
  getLine >>= readExpr
    |> eval
    |> print
