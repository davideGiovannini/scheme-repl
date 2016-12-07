module Parsing  where

import Text.ParserCombinators.Parsec hiding (spaces)


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Char Char
             | Bool Bool
             deriving(Show, Eq)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
                _ <- char '"'
                x <- many (noneOf "\\\"" <|> (char '\\' >> char '"'))
                _ <- char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t"  -> Bool True
                         "#f"  -> Bool False
                         _     -> Atom atom

parseChar :: Parser LispVal
parseChar = do
              _ <- string "#\\"
              x <- anyChar
              return $ Char x

parseNumber :: Parser LispVal
parseNumber = do
      sign   <- optionMaybe $ oneOf "-+"
      number <- many1 digit
      return $ case sign of
        Just '-' -> Number $ read ('-':number)
        _ -> Number $ read number


parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces


parseDottedList :: Parser LispVal
parseDottedList = do
  head_ <- endBy parseExpr spaces
  tail_ <- char '.' >> spaces >> parseExpr
  return $ DottedList head_ tail_

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [ Atom "quote", x]


parseExpr :: Parser LispVal
parseExpr =  parseChar
         <|> parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do _ <- char '('
                x <- try parseList <|> parseDottedList
                _ <- char ')'
                return x
