module Evaluation
  -- (eval)
where

import Data( LispVal(..)
           , ThrowsError
           , LispError(..)
           , LispFunction
           )
import Control.Monad.Except(throwError)

import Evaluation.Primitives


eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool   _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", p, c, a]) = control_if p c a
eval (List (Atom func: args)) = mapM eval args >>= apply func
eval badform = throwError $ BadSpecialForm "Unrecognized special form" badform

apply :: String -> LispFunction
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)


control_if :: LispVal -> LispVal -> LispVal -> ThrowsError LispVal
control_if predicate consequence alternative =
  do result <- eval predicate
     case result of
       Bool False -> eval alternative
       _          -> eval consequence



primitives :: [(String, LispFunction)]
primitives = [ ("+",         numPlus)
             , ("-",         numMinus)
             , ("*",         numMul)
             , ("/",         numDiv)
             , ("mod",       numMod)
             , ("quotient",  numQuot)
             , ("remainder", numRem)

             , ("symbol?",   isSymbol)
             , ("bool?",     isBool)
             , ("string?",   isString)
             , ("number?",   isNumber)
             , ("list?",     isList )
             , ("=",         numEq)
             , ("<",         numLessT)
             , (">",         numGreatT)
             , ("/=",        numNotEq)
             , (">=",        numGE)
             , ("<=",        numLE)
             , ("&&",        boolAnd)
             , ("||",        boolOr)
             , ("string=?",  strEq)
             , ("string<?",  strLessT)
             , ("string>?",  strGreatT)
             , ("string<=?", strLE)
             , ("string>=?", strGE)

             , ("head",      head_)
             , ("tail",      tail_)
             , ("cons",      cons)
             , ("eqv?",      eqv)
             , ("eq?",       eqv)
             , ("equal?",    equal)
             ]
