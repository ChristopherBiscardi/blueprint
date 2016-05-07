{-# LANGUAGE OverloadedStrings #-}

import           Blueprint.AST
import qualified Blueprint.Parser as BP
import           Test.Hspec.Core.Spec (fromSpecList)
import           Test.Tasty.Hspec
import           Text.Trifecta
import           Text.Trifecta.Delta

main :: IO ()
main = hspec $ do
  describe "Blueprint.Parser" parserTests
  describe "Blueprint" blueprintTests

startOfLine = Columns 0 0

parserTests :: Spec
parserTests = do
    describe ".name" $ do
      it "accepts a single char" $ do
         let res = parseString BP.name startOfLine "_"
         case res of
             Success a -> a `shouldBe` "_"
             _ -> fail $ show res
      it "accepts letters and numbers" $ do
         let res = parseString BP.name startOfLine "_things239"
         case res of
             Success a -> a `shouldBe` "_things239"
             _ -> fail $ show res
      it "fails when starting with a number" $ do
         let res = parseString BP.name startOfLine "8test"
         case res of
             Success a -> fail "Should not be able to parse this"
             Failure doc -> True `shouldBe` True

ff = Field Nothing "hero" Nothing Nothing (Just $ [Field Nothing "name" Nothing Nothing Nothing])

blueprintTests :: Spec
blueprintTests = do
  describe ".parse" $ do
    it "parses a simple query" $ do
      case BP.parseGraphQL "query HeroNameQuery {hero {name}}" of
        Success a -> a `shouldBe` Document [
          OperationDefinition QUERY (Just $ Name "HeroNameQuery") Nothing Nothing [ff]]
        Failure a -> putStrLn $ show a
