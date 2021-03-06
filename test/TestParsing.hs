module TestParsing (parsingTests) where

import Data (LispVal(..))
import Parsing(parseExpr)
import Test.Hspec
import Test.QuickCheck hiding (Result)
import qualified Text.Parsec as P

import TestArbitraryData()


parsingTests :: Spec
parsingTests =
    describe "Parsing tests" $ do
        parsingNumbers
        parsingStrings
        parsingLists
        parsingDottedLists
        parsingItself


-- Utility functions and type alias
type Result = Either P.ParseError LispVal

parser :: String -> String -> Result
parser = P.parse parseExpr


parses :: Result -> LispVal -> Bool
parses res value = res == Right value

-- STRINGS
parseString :: String -> Result
parseString = parser "string"


parsingStrings :: Spec
parsingStrings = describe "Parsing Strings" $ do
  emptyString
  normalString
  escapedQuotesString
  escapedCharacters

emptyString :: Spec
emptyString = it "can parse empty string" $
  parseString "\"\"" `parses` String ""

normalString :: Spec
normalString = it "can parse \"Hello World!\"" $
  parseString "\"Hello World!\"" `parses` String "Hello World!"

escapedQuotesString :: Spec
escapedQuotesString = it "can parse a string with \\\"escaped quotes \\\" " $
  parseString "\"\\\"\"" `parses` String "\""

escapedCharacters :: Spec
escapedCharacters = it "can parse a string with \\\" \\n \\t \\r \\\\" $
  parseString "\" \\n\\r\\\\ \"" `parses` String " \n\r\\ "



-- NUMBERS

parseNumber :: String -> Result
parseNumber = parser "number"

parsingNumbers :: Spec
parsingNumbers = describe "Parsing integers:" $ do
    it "parses a random integer" $ property prop_integerAreParsed
    it "parses +10" $
      parseNumber "+10" `parses` Number 10


prop_integerAreParsed :: Integer -> Bool
prop_integerAreParsed num = parseNumber (show num) `parses` Number num


-- LISTS

parseList :: String -> Result
parseList = parser "list"

parsingLists :: Spec
parsingLists = describe "Parsing lists" $ do
  it "parses an empty list" $
    parseList "()" `parses` List []
  it "parses (2 23 #t) list" $
    parseList "(2 23 #t)" `parses` List [Number 2, Number 23, Bool True]
  it "parses nested list: (2 (4 59) #t)" $
    parseList "(2 (4 59) #t)" `parses` List [Number 2, List [Number 4, Number 59], Bool True]


-- DOTTED LISTS

parseDotList :: String -> Result
parseDotList = parser "dotted list"

parsingDottedLists :: Spec
parsingDottedLists = describe "Parsing dotted lists" $
  it "parses dotted list (#t . #t)" $
    parseDotList "(#t . #t)" `parses` DottedList [Bool True] (Bool True)


-- SHOW

parsingItself :: Spec
parsingItself = describe "parsing string repr of LispVals" $ do
  it "can parse show repr of String" prop_parseShowString
  it "can parse show repr of Number" prop_parseShowNumber
  it "can parse show repr of Bool"   prop_parseShowBool
  it "can parse arbitrary LispVal" $ property prop_parseArbitraryLispVal


prop_parseShowString :: Property
prop_parseShowString = property $ prop_parseArbitraryLispVal . String

prop_parseShowNumber :: Property
prop_parseShowNumber = property $ prop_parseArbitraryLispVal . Number

prop_parseShowBool :: Property
prop_parseShowBool = property $ prop_parseArbitraryLispVal . Bool

prop_parseArbitraryLispVal :: LispVal -> Bool
prop_parseArbitraryLispVal lispVal =
  parser "arbitrary" (show lispVal) `parses` lispVal
