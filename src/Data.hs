module Data
  ( LispVal(..)
  , LispError(..)
  , ThrowsError
  , LispFunction
  )
where

import Text.Parsec(ParseError)
import Control.Monad.Except

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving(Eq)

instance Show  LispVal where
  show (String contents)        = "\"" ++ escapeString contents ++ "\""
  show (Atom name)              = name
  show (Number contents)        = show contents
  show (Bool True)              = "#t"
  show (Bool False)             = "#f"
  show (List contents)          = "(" ++ unwordsList contents ++ ")"
  show (DottedList head_ tail_) = "(" ++ unwordsList head_ ++ " . " ++ show tail_ ++ ")"


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
