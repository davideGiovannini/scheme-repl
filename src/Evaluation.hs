module Evaluation
  ( eval
  , IOThrowsError
  , emptyEnv
  , Env
  , liftThrows
  , bindVars)
where

import Data( LispVal(..)
           , ThrowsError
           , LispError(..)
           , LispFunction
           )
import Control.Monad.Except(throwError, ExceptT, mapExceptT, liftIO)
import Data.Functor.Identity(runIdentity)
import Data.IORef
import Data.Maybe(isJust)

import Evaluation.Primitives


type Env = IORef [(String, IORef LispVal)]

emptyEnv :: IO Env
emptyEnv = newIORef []

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows = mapExceptT (return.runIdentity)

isBound :: Env -> String -> IO Bool
isBound envRef var = fmap (isJust . lookup var) (readIORef envRef)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . flip writeIORef value)
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings' env = fmap (++ env) (mapM addBinding bindings')
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)


eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool   _) = return val
eval env (Atom atom)    = getVar env atom
eval _   (List [Atom "quote", val]) = return val
eval env (List [Atom "if", p, c, a]) = control_if env p c a
eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
     eval env form >>= defineVar env var
eval env (List (Atom func: args)) = mapM (eval env) args >>= liftThrows . apply func


eval _ badform = throwError $ BadSpecialForm "Unrecognized special form" badform

apply :: String -> LispFunction
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)


control_if :: Env -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
control_if env predicate consequence alternative =
  do result <- eval env predicate
     case result of
       Bool False -> eval env alternative
       _          -> eval env consequence



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
             , ("not",       boolNot)

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
