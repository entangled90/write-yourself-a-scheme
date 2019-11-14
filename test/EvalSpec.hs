
module EvalSpec where

import           Test.Hspec
import           LispVal
import           Eval

spec :: Spec
spec = describe "Eval" $ do
  context "when evaluating if statement" $ do
    it "should evaluate predicate" $ do
      let expression =
            List
              [ Atom "if"
              , List $ [Atom "==", Number 2, Number 2]
              , Number 2
              , Number 3
              ]
      eval expression `shouldBe` (Right $ Number 2)
