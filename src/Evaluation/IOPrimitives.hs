module Evaluation.IOPrimitives
  (
    makePort
  , closePort
  , readProc
  , writeProc
  , load
  , readContents
  , readAll
  )
where

import Data( LispVal(..)
           , IOLispFunction
           , IOThrowsError
           , LispError(..)
           , liftThrows
           )
import Control.Monad.Except(throwError, liftIO)
import System.IO(IOMode, openFile, hClose, stdin, hGetLine, stdout, hPrint)

import Parsing(readExpr, readExprList)


makePort :: IOMode -> IOLispFunction
makePort mode [String filename] = Port <$> liftIO (openFile filename mode)
makePort _    [arg]             = throwError $ TypeMismatch "open--file expects a string" arg
makePort _    ls                = throwError $ NumArgs 1 ls

closePort :: IOLispFunction
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _           = return $ Bool False

readProc :: IOLispFunction
readProc []          = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr
readProc args        = throwError $ BadSpecialForm "bad read form" (List args)

writeProc :: IOLispFunction
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)
writeProc args             = throwError $ BadSpecialForm "bad write form" (List args)

readContents :: IOLispFunction
readContents [String filename] = String <$> liftIO (readFile filename)
readContents args              = throwError $ BadSpecialForm "bad read-contents form" (List args)


load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList


readAll :: IOLispFunction
readAll [String filename] = List <$> load filename
readAll args              = throwError $ BadSpecialForm "bad read-all form" (List args)
