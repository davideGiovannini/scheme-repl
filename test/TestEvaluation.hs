module TestEvaluation (evaluationTests) where

import Test.Hspec

evaluationTests :: SpecWith ()
evaluationTests = describe "Evaluation Tests" $
    it "needs tests" $ True `shouldBe` True
