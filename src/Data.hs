module Data
  ( LispVal(..)
  , LispError(..)
  , ThrowsError
  , LispFunction
  , IOLispFunction
  , Env
  , emptyEnv
  , IOThrowsError
  , liftThrows
  )
where

import Text.Parsec(ParseError)
import Control.Monad.Except
import Data.Functor.Identity(runIdentity)
import Data.IORef
import System.IO(Handle)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc LispFunction
             | Func { _params :: [String]
                    , _vararg :: Maybe String
                    , _body   :: [LispVal]
                    , _closure :: Env
                    }
             | IOFunc IOLispFunction
             | Port Handle

instance Eq LispVal where
  (Atom a)   == (Atom b)   = a == b
  (Number a) == (Number b) = a == b
  (String a) == (String b) = a == b
  (Bool a)   == (Bool b)   = a == b
  (List a)   == (List b)   = a == b
  (DottedList a a1) == (DottedList b b1) = a == b && a1 == b1
  _ == _ = False



instance Show  LispVal where
  show (String contents)        = "\"" ++ escapeString contents ++ "\""
  show (Atom name)              = name
  show (Number contents)        = show contents
  show (Bool True)              = "#t"
  show (Bool False)             = "#f"
  show (List contents)          = "(" ++ unwordsList contents ++ ")"
  show (DottedList head_ tail_) = "(" ++ unwordsList head_ ++ " . " ++ show tail_ ++ ")"
  show (PrimitiveFunc _)        = "<primitive>"
  show (Func args varargs _ _)  = "(lambda (" ++ unwords (map show args) ++
                                      maybe "" (" . "++) varargs
                                      ++ ") ...)"
  show (Port _)                 = "<IO port>"
  show (IOFunc _)               = "<IO primitive>"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

escapeString :: String -> String
escapeString = concatMap $ \char -> case char of
                                      '\n' -> "\\n"
                                      '\t' -> "\\t"
                                      '\\' -> "\\\\"
                                      '\"' -> "\\\""
                                      c    -> [c]

type LispFunction = [LispVal] -> ThrowsError LispVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
               deriving(Eq)

instance Show LispError where
  show (UnboundVar message varname)  = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func)    = message ++ ": " ++ show func
  show (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
  show (Parser parseErr)             = "Parse error at " ++ show parseErr
  show (Default message)             = "Default error: " ++ message



type ThrowsError = Except LispError

type Env = IORef [(String, IORef LispVal)]

emptyEnv :: IO Env
emptyEnv = newIORef []

type IOThrowsError = ExceptT LispError IO
type IOLispFunction = [LispVal] -> IOThrowsError LispVal

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows = mapExceptT (return.runIdentity)
