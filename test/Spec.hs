{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec.Core.Spec (fromSpecList)
import           Test.Tasty.Hspec
import           Text.Trifecta
import           Text.Trifecta.Delta
import           Text.Trifecta.Result
import Text.PrettyPrint.ANSI.Leijen (putDoc)

import           Blueprint.AST
import qualified Blueprint.Parser as BP
import SchemaParser (schemaParserTests)

main :: IO ()
main = hspec $ do
  describe "Blueprint.Parser" parserTests
  describe "Blueprint" blueprintTests

startOfLine = Columns 0 0

parserTests = do
  describe "name" nameParserTests
  describe "schema-language" schemaParserTests

nameParserTests :: Spec
nameParserTests = do
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
             Failure _doc -> True `shouldBe` True

ff = Field Nothing "hero" Nothing Nothing [Field Nothing "name" Nothing Nothing []]

blueprintTests :: Spec
blueprintTests = do
  describe ".parse" $ do
    it "parses `query HeroNameQuery {hero {name}}`" $ do
      case BP.parseGraphQL "query HeroNameQuery {hero {name}}" of
        Success a -> a `shouldBe` Document [
          OperationDefinition QUERY (Just $ Name "HeroNameQuery") Nothing Nothing [ff]]
        Failure a -> fail $ show a
    it "parses `{ hero { name } }`" $ do
      case BP.parseGraphQL "{ hero { name } }" of
        Success a -> a `shouldBe` Document [
          OperationDefinition QUERY Nothing Nothing Nothing [ff]]
        Failure a -> fail $ show a
    it "parses `query {hero {name }}" $ do
      case BP.parseGraphQL "query {hero {name }}" of
        Success a -> a `shouldBe` Document [
          OperationDefinition QUERY Nothing Nothing Nothing [ff]]
        Failure a -> fail $ show a
    it "parses `query {hero {name}}" $ do
      case BP.parseGraphQL "query {hero {name}}" of
        Success a -> a `shouldBe` Document [
          OperationDefinition QUERY Nothing Nothing Nothing [ff]]
        Failure a -> fail $ show a
    -- | TODO: can we make this error message nicer via formatting?
    describe "error messages" $ do
      it "errors on faulty opTypes" $ do
        case BP.parseGraphQL "finagle Name {hero {name}}" of
          Success a -> fail $ show a
          Failure (ErrInfo a b) -> True `shouldBe` True

