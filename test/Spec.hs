import Test.Hspec

import TestParsing
import TestEvaluation
import TestPrimitives

main :: IO ()
main = hspec $ do
    parsingTests
    evaluationTests
    primitivesTests
