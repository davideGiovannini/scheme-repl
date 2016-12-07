import Test.Hspec

import TestParsing
import TestEvaluation

main :: IO ()
main = hspec $ do
    parsingTests
    evaluationTests
