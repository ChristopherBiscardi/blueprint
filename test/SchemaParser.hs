{-# LANGUAGE OverloadedStrings #-}
module SchemaParser where

import qualified Data.Map.Strict as Map
import           Test.Tasty.Hspec
import           Text.Trifecta
import           Text.Trifecta.Delta
import           Text.Trifecta.Result

import           Blueprint.AST
import qualified Blueprint.Schema as S

startOfLine = Columns 0 0

schemaParserTests :: Spec
schemaParserTests = do
      it "parses an argument" $
        bpParseTest (S.argumentParser :: Parser (String, S.GraphQLArgumentConfig String))
                    "episode: Episode"
                    ("episode", S.GraphQLArgumentConfig "Episode" Nothing Nothing)
      it "parses a field" $
        bpParseTest (S.fieldParser :: Parser (S.FieldName, S.GraphQLFieldConfig))
                    "hero(episode: Episode): Character"
                    ("hero", S.GraphQLFieldConfig "Character"
                                                  (Map.fromList [("episode", S.GraphQLArgumentConfig "Episode" Nothing Nothing)])
                                                  Nothing
                                                  Nothing
                    )
      it "parses multiple fields" $ do
         res <- parseFromFileEx S.fieldsParser "test/schema/fields.graphql"
         case res of
             Success a -> a `shouldBe` (S.fields heroQueryType)
             Failure xs -> fail $ show xs
      it "parses a basic type" $ do
         res <- parseFromFileEx S.graphqlTypeParser "test/schema/type.graphql"
         case res of
             Success a -> a `shouldBe` heroQueryType
             Failure xs -> fail $ show xs
      it "parses a type with default arguments" $ do
         res <- parseFromFileEx S.graphqlTypeParser "test/schema/default-arguments.graphql"
         case res of
             Success a -> a `shouldBe` starshipType
             Failure xs -> fail $ show xs
      it "parses an enum type" $ do
         res <- parseFromFileEx S.enumParser "test/schema/enum.graphql"
         case res of
             Success a -> a `shouldBe` S.GraphQLEnum "USER_STATE" ["NOT_FOUND","ACTIVE","INACTIVE", "SUSPENDED"]
             Failure xs -> fail $ show xs
      it "parses a few types" $ do
         res <- parseFromFileEx S.typesParser "test/schema/few-types.graphql"
         case res of
             Success a -> a `shouldBe` [ heroQueryType
                                       , simpleType "Character"
                                       , simpleType "Droid"
                                       ]
             Failure xs -> fail $ show xs
      it "parses a basic Schema" $ do
         res <- parseFromFileEx S.schemaParser "test/schema/basic-schema.graphql"
         case res of
             Success a -> a `shouldBe` [ heroQueryType
                                       , simpleType "Character"
                                       , simpleType "Droid"
                                       ]
             Failure xs -> fail $ show xs
      it "fails to parse a basic Schema with a missing type" $ do
         res <- parseFromFileEx S.schemaParser "test/schema/basic-schema-missing-type.graphql"
         case res of
             Success a -> a `shouldBe` [ heroQueryType
                                       , simpleType "Character"
                                       , simpleType "Droid"
                                       ]
             Failure xs -> fail $ show xs


-- | test a parser against a string to equal a result
bpParseTest :: (Show a, Eq a) => Parser a -> String -> a -> Expectation
bpParseTest parser string result = do
  let res = parseString parser
                        startOfLine
                        string
  case res of
    Success a -> a `shouldBe` result
    Failure xs -> fail $ show res

heroQueryType :: S.GraphQLObject
heroQueryType = S.GraphQLObject "Query" (Map.fromList [
  ("hero", S.GraphQLFieldConfig
     { S.fType="Character"
     , S.fArgs=Map.fromList [
      ("episode", S.GraphQLArgumentConfig "Episode" Nothing Nothing)
      ]
     , S.fDeprecationReason=Nothing
     , S.fDescription=Nothing
     }),
    ("droid", S.GraphQLFieldConfig
     { S.fType="Droid"
     , S.fArgs=Map.fromList [
      ("id", S.GraphQLArgumentConfig "ID" Nothing Nothing)
      ]
     , S.fDeprecationReason=Nothing
     , S.fDescription=Nothing
     })
  ]) Nothing

starshipType :: S.GraphQLObject
starshipType = S.GraphQLObject "Starship" (Map.fromList [
  ("id", S.GraphQLFieldConfig
     { S.fType="ID"
     , S.fArgs=Map.empty
     , S.fDeprecationReason=Nothing
     , S.fDescription=Nothing
     }),
    ("name", S.GraphQLFieldConfig
     { S.fType="String"
     , S.fArgs=Map.empty
     , S.fDeprecationReason=Nothing
     , S.fDescription=Nothing
     }),
    ("length", S.GraphQLFieldConfig
     { S.fType="Float"
     , S.fArgs=Map.fromList [
      ("unit", S.GraphQLArgumentConfig "LengthUnit" (Just "METER") Nothing)
      ]
     , S.fDeprecationReason=Nothing
     , S.fDescription=Nothing
     })
  ]) Nothing

-- | Helper to construct a simple GraphQLObject with a single Name field
simpleType :: String -> S.GraphQLObject
simpleType objectName =
  S.GraphQLObject objectName
                  (Map.fromList [
                      ("name", S.GraphQLFieldConfig
                       { S.fType="String"
                       , S.fArgs=Map.empty
                       , S.fDeprecationReason=Nothing
                       , S.fDescription=Nothing
                       })
                      ])
                  Nothing
