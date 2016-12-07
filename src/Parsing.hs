module Parsing where

import Data (LispVal(..))
import Text.ParserCombinators.Parsec hiding (spaces)



symbol :: Parser Char
symbol = oneOf "!#$%&|*/:<=>?@^_~\\"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
                _ <- char '"'
                -- x <- many (noneOf "\\\"" <|> (char '\\' >> char '"'))
                x <- many  (try parseEscapedChar <|> noneOf "\\\"")
                _ <- char '"'
                return $ String x

parseEscapedChar :: Parser Char
parseEscapedChar = do
  _ <- char '\\'
  c <- anyChar
  return $ case c of
    'n'  -> '\n'
    '"'  -> '\"'
    'r'  -> '\r'
    't'  -> '\t'
    '\\' -> '\\'
    _    -> ' '



parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol <|> oneOf "+-")
              let atom = first:rest
              return $ case atom of
                         "#t"              -> Bool True
                         "#f"              -> Bool False
                         _                 -> Atom atom


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
parseExpr =  parseAtom
         -- <|> parseChar
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do _ <- char '('
                x <- try parseList <|> parseDottedList
                _ <- char ')'
                return x
