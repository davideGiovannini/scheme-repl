module Evaluation (eval) where

import Data(LispVal(..))


eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool   _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func: args)) = apply func $ map eval args
eval arg = arg

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (String $ "Unknown function " ++ func) ($ args) $ lookup func primitives




primitives :: [(String, [LispVal] -> LispVal)]
primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)

             , ("symbol?", isSymbol )
             , ("bool?", isBool )
             , ("string?", isString )
             , ("number?", isNumber )
             , ("list?", isList )
             ]


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0



isSymbol :: [LispVal] -> LispVal
isSymbol [Atom _] = Bool True
isSymbol _        = Bool False

isString :: [LispVal] -> LispVal
isString [String _] = Bool True
isString _        = Bool False

isBool :: [LispVal] -> LispVal
isBool [Bool _] = Bool True
isBool _        = Bool False

isNumber :: [LispVal] -> LispVal
isNumber [Number _] = Bool True
isNumber _          = Bool False


isList :: [LispVal] -> LispVal
isList [List _] = Bool True
isList _        = Bool False
