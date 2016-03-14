{-# LANGUAGE OverloadedStrings #-}

import Blueprint.Parser
import Text.Trifecta.Delta
import Test.Tasty.Hspec
import Text.Trifecta 


main :: IO ()
main = hspec $ do
  describe "Blueprint.Parser" $ do
    describe ".name" $ do
      it "accepts a single char" $ do
         let res = parseString Blueprint.Parser.name (Columns 0 0) "_"
         case res of
             Success a  -> a `shouldBe` "_"
             _ -> fail $ show res
      it "accepts letters and numbers" $ do
         let res = parseString Blueprint.Parser.name (Columns 0 0) "_things239"
         case res of
             Success a  -> a `shouldBe` "_things239"
             _ -> fail $ show res
      it "fails when starting with a number" $ do
         let res = parseString Blueprint.Parser.name (Columns 0 0) "8test"
         case res of
             Success a  -> a `shouldBe` "8test"
             _ -> fail $ show res
