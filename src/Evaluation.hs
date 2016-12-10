module Evaluation
  ( eval
  , liftThrows
  , primitiveEnv
  , bindVars)
where

import Data( LispVal(..)
           , ThrowsError
           , LispError(..)
           , LispFunction
           , IOLispFunction
           , Env
           , emptyEnv
           , IOThrowsError
           )
import Control.Monad.Except(throwError, mapExceptT, liftIO)
import Data.Functor.Identity(runIdentity)
import Data.Maybe(isJust, isNothing)
import Data.IORef

import Evaluation.Primitives



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
eval env (List [Atom "if", p, c, a]) = controlIf env p c a
eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
     eval env form >>= defineVar env var

eval env (List (Atom "define" : List (Atom var : params) : body)) =
     makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
     makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
     makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
     makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
     makeVarArgs varargs env [] body
eval env (List (Atom "begin":first:rest)) = controlBegin env (first:rest)

eval env (List (function : args)) = do
     func <- eval env function
     argVals <- mapM (eval env) args
     apply func argVals

eval _ badform = throwError $ BadSpecialForm "Unrecognized special form" badform

apply :: LispVal -> IOLispFunction
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
      if num params /= num args && isNothing varargs
         then throwError $ NumArgs (num params) args
         else liftIO (bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params) args
            num = toInteger . length
            evalBody env = last <$> mapM (eval env) body
            bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
                Nothing -> return env
apply _ _ = undefined

controlIf :: Env -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
controlIf env predicate consequence alternative =
  do result <- eval env predicate
     case result of
       Bool False -> eval env alternative
       _          -> eval env consequence

controlBegin :: Env -> IOLispFunction
controlBegin env (first:rest) = last <$> mapM (eval env) (first:rest)
controlBegin _ []             = throwError $ BadSpecialForm "Begin needs at least 1 expression" (List [])


makeFunc :: Maybe String -> Env -> [LispVal] -> IOLispFunction
makeFunc varargs env params body = return $ Func (map show params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> IOLispFunction
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> IOLispFunction
makeVarArgs = makeFunc . Just . show

primitiveEnv :: IO Env
primitiveEnv = emptyEnv >>= flip bindVars ( map makePrimitiveFunc primitives)
     where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

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
