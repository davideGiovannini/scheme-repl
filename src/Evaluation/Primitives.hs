{-# LANGUAGE ExistentialQuantification #-}
module Evaluation.Primitives
  (
    numPlus, numMinus, numMul, numDiv
  , numMod, numQuot, numRem, numEq
  , numLessT, numGreatT, numNotEq
  , numGE, numLE, boolAnd, boolOr
  , strEq, strLessT, strGreatT
  , strLE, strGE, isSymbol, isBool
  , isString, isNumber, isList
  , head_, tail_, cons, eqv, equal
  )
where

import Control.Monad.Except(throwError, runExcept, catchError)
import Data(LispVal(..), ThrowsError, LispError(..), LispFunction)

numPlus, numMinus, numMul, numDiv :: LispFunction
numPlus   = numericBinop (+)
numMinus  = numericBinop (-)
numMul    = numericBinop (*)
numDiv    = numericBinop div

numMod, numQuot, numRem, numEq, numNotEq :: LispFunction
numMod    = numericBinop mod
numQuot   = numericBinop quot
numRem    = numericBinop rem
numEq     = numBoolBinop (==)
numNotEq  = numBoolBinop (/=)

numLessT, numGreatT, numGE, numLE :: LispFunction
numLessT  = numBoolBinop (<)
numGreatT = numBoolBinop (>)
numGE     = numBoolBinop (>=)
numLE     = numBoolBinop (<=)

boolAnd, boolOr :: LispFunction
boolAnd   = boolBoolBinop (&&)
boolOr    = boolBoolBinop (||)

strEq, strLessT, strGreatT, strLE, strGE :: LispFunction
strEq     = strBoolBinop (==)
strLessT  = strBoolBinop (<)
strGreatT = strBoolBinop (>)
strLE     = strBoolBinop (<=)
strGE     = strBoolBinop (>=)

numericBinop :: (Integer -> Integer -> Integer) -> LispFunction
numericBinop _           []  = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop  op params        = fmap (Number . foldl1 op) (mapM unpackNum params)
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool


boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> LispFunction
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ head args
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> LispFunction
numBoolBinop  = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> LispFunction
strBoolBinop  = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> LispFunction
boolBoolBinop = boolBinop unpackBool


isSymbol :: LispFunction
isSymbol [Atom _] = return $ Bool True
isSymbol [_]      = return $ Bool False
isSymbol badargs  = throwError $ NumArgs 1 badargs

isString :: LispFunction
isString [String _] = return $ Bool True
isString [_]        = return $ Bool False
isString badargs    = throwError $ NumArgs 1 badargs

isBool :: LispFunction
isBool [Bool _] = return $ Bool True
isBool [_]      = return $ Bool False
isBool badargs  = throwError $ NumArgs 1 badargs

isNumber :: LispFunction
isNumber [Number _] = return $ Bool True
isNumber [_]        = return $ Bool False
isNumber badargs    = throwError $ NumArgs 1 badargs


isList :: LispFunction
isList [List _] = return $ Bool True
isList [_]      = return $ Bool False
isList badargs  = throwError $ NumArgs 1 badargs

head_ :: LispFunction
head_ [List (x : _)]         = return x
head_ [DottedList (x : _) _] = return x
head_ [badArg]                = throwError $ TypeMismatch "pair" badArg
head_ badArgList              = throwError $ NumArgs 1 badArgList

tail_ :: LispFunction
tail_ [List (_ : xs)]         = return $ List xs
tail_ [DottedList [_] x]      = return x
tail_ [DottedList (_ : xs) x] = return $ DottedList xs x
tail_ [badArg]                = throwError $ TypeMismatch "pair" badArg
tail_ badArgList              = throwError $ NumArgs 1 badArgList


cons :: LispFunction
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList


eqv :: LispFunction
eqv [Bool arg1, Bool arg2]             = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2]         = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2]         = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2]             = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2]             = return $ Bool $ (length arg1 == length arg2) &&
                                                             all eqvPair (zip arg1 arg2)
                                             where eqvPair (x1, x2) = case runExcept (eqv [x1, x2]) of
                                                     Left _           -> False
                                                     Right (Bool val) -> val
                                                     _                -> undefined -- eqv must return only booleans
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList


data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
             `catchError` const (return False)

equal :: LispFunction
equal [arg1, arg2] = do
      primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2)
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList
