module TestParsing (parsingTests) where

import Parsing (LispVal(..))
import qualified Parsing
import Test.Hspec
import Test.QuickCheck
import qualified Text.Parsec as P


-- STRINGS
parseString :: String -> Either P.ParseError LispVal
parseString = P.parse Parsing.parseString "string"

parsedString :: String -> Either a LispVal
parsedString = Right . String

parsingStrings :: SpecWith ()
parsingStrings = describe "Parsing Strings" $ do
  emptyString
  normalString
  escapedQuotesString
  escapedCharacters

emptyString :: SpecWith ()
emptyString = it "can parse empty string" $
  parseString "\"\"" `shouldBe` parsedString ""

normalString :: SpecWith ()
normalString = it "can parse \"Hello World!\"" $
  parseString "\"Hello World!\"" `shouldBe` parsedString "Hello World!"

escapedQuotesString :: SpecWith ()
escapedQuotesString = it "can parse a string with \\\"escaped quotes \\\" " $
  parseString "\"\\\"\"" `shouldBe` parsedString "\""

escapedCharacters :: SpecWith ()
escapedCharacters = it "can parse a string with \"\n\t\r\\" $
  parseString "\" \\n\\r\\\\ \"" `shouldBe` parsedString " \n\r\\ "



-- NUMBERS

parseNumber :: String -> Either P.ParseError LispVal
parseNumber = P.parse Parsing.parseNumber "number"

parsingNumbers :: SpecWith ()
parsingNumbers = describe "Parsing integers:" $ do
    it "parses a random integer" $ property prop_integerAreParsed
    it "parses +10" $
      parseNumber "+10" `shouldBe` Right (Number 10)


prop_integerAreParsed :: Integer -> Bool
prop_integerAreParsed n = case parseNumber (show n) of
                  Right (Number m) -> n == m
                  _ -> False


-- CHARS

parseChar :: String -> Either P.ParseError LispVal
parseChar = P.parse Parsing.parseChar "char"

parsingChars :: SpecWith ()
parsingChars = describe "Parsing chars:" $ do
    it "parses a random char" $ property prop_randomCharsAreParsed
    parseSpaceChar

parseSpaceChar :: SpecWith ()
parseSpaceChar = do
  it "can parse #\\space" $
    parseChar "#\\space" `shouldBe` Right (Char ' ')
  it "can parse #\\ " $
    parseChar "#\\ " `shouldBe` Right (Char ' ')


prop_randomCharsAreParsed :: Char -> Bool
prop_randomCharsAreParsed char = case parseChar ("#\\" ++ [char]) of
                  Right (Char m) -> char == m
                  _ -> False


parsingTests :: SpecWith ()
parsingTests =
    describe "Parsing tests" $ do
        parsingNumbers
        parsingStrings
        parsingChars
