module ParserSpec where

import           Test.Hspec
import           Parser
import           Data.Either
import           LispVal


spec :: Spec
spec = describe "Parser" $ do
  context "when parsing a nested list \"(a (nested) test)\"" $ do
    it "should parse" $ do
      parse "(a (nested) test)"
        `shouldBe` (Right $ List [Atom "a", List $ [Atom "nested"], Atom "test"]
                   )
  context "when parsing quoted nested list \"(a '(nested) test)\"" $ do
    it "should parse" $ do
      parse "(a '(nested) test)"
        `shouldBe` ( Right
                   $ List
                       [ Atom "a"
                       , List $ [quote, List $ [Atom "nested"]]
                       , Atom "test"
                       ]
                   )
  context "when parsing an imbalanced list \"(a '(nested) test\"" $ do
    it "should return a Left" $ do
      parse "(a '(nested) test" `shouldSatisfy` isLeft
  context "when parsing an imbalanced list \"(\"" $ do
    it "should return a Left" $ do
      parse "(" `shouldSatisfy` isLeft

