module Evaluation (eval) where

import Data(LispVal(..), ThrowsError, LispError(..))
import Control.Monad.Except(throwError)


eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool   _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func: args)) = mapM eval args >>= apply func
eval badform = throwError $ BadSpecialForm "Unrecognized special form" badform

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)




primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod",       numericBinop mod)
             , ("quotient",  numericBinop quot)
             , ("remainder", numericBinop rem)

             , ("symbol?", isSymbol )
             , ("bool?",   isBool )
             , ("string?", isString )
             , ("number?", isNumber )
             , ("list?",   isList )
             ]


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _           []  = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum



isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [Atom _] = return $ Bool True
isSymbol [_]      = return $ Bool False
isSymbol badargs  = throwError $ NumArgs 1 badargs

isString :: [LispVal] -> ThrowsError LispVal
isString [String _] = return $ Bool True
isString [_]        = return $ Bool False
isString badargs    = throwError $ NumArgs 1 badargs

isBool :: [LispVal] -> ThrowsError LispVal
isBool [Bool _] = return $ Bool True
isBool [_]      = return $ Bool False
isBool badargs  = throwError $ NumArgs 1 badargs

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [Number _] = return $ Bool True
isNumber [_]        = return $ Bool False
isNumber badargs    = throwError $ NumArgs 1 badargs


isList :: [LispVal] -> ThrowsError LispVal
isList [List _] = return $ Bool True
isList [_]      = return $ Bool False
isList badargs  = throwError $ NumArgs 1 badargs
