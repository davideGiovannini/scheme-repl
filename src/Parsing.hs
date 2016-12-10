module Parsing
  ( readExpr
  , parseExpr
  , readExprList)
where

import Control.Monad.Except(throwError)
import Data (LispVal(..), ThrowsError, LispError(Parser))
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec (manyTill, endOfLine)



symbol :: Parser Char
symbol = oneOf "!#$%&|*/:<=>?@^_~\\-+"

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
              rest <- many $ letter <|> digit <|> symbol
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
parseExpr = do
  skipMany comment
  try parseNumber
         <|> parseAtom
         <|> parseString
         <|> parseQuoted
         <|> do _ <- char '('
                x <- try parseList <|> parseDottedList
                _ <- char ')'
                return x


comment :: Parser ()
comment = char ';' >> manyTill anyChar endOfLine >> return ()


readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
                             Left err  -> throwError $ Parser err
                             Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow $ endBy parseExpr spaces
