module Lib
    ( someFunc
    ) where
        
import System.Environment
import Text.Parsec
import Parsing

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err  -> "No match: "    ++ show err
                   Right val -> "Found value: " ++ show val
someFunc :: IO ()
someFunc = do
  args <- getArgs
  putStrLn $ "Hello, " ++ readExpr (args !! 0)
