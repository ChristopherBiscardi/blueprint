{-# LANGUAGE OverloadedStrings #-}

module Blueprint.Parser where

import Control.Applicative ((<|>))
import Data.String (IsString)
import Text.Parser.Char
import Text.Trifecta

import Blueprint.AST

-- | OperationType can be `query` or `mutation`
--   In the futue, this might be expanded to `subscription`
operationType :: Parser (Maybe OPERATION_TYPE)
operationType =   lookupOp <$> text "query"
              <|> lookupOp <$> text "mutation"

lookupOp :: (Eq a, IsString a) => a -> Maybe OPERATION_TYPE
lookupOp str = lookup str [("query", QUERY)
                          ,("mutation", MUTATION)
                          ]
-- | 
name :: Parser [Char]
name = do
  prefixChar <- char '_' <|> letter
  rest <- many alphaNum
  return $ [prefixChar] ++ rest

-- | Commas are optional in graphql
comma :: Parser Char
comma = char ','

