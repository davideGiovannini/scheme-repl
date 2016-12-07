module Data (LispVal(..)) where

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
