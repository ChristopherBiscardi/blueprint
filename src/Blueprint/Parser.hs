{-# LANGUAGE OverloadedStrings #-}

module Blueprint.Parser where

import           Control.Applicative ((<|>))
import           Data.Char           (chr)
import           Data.String         (IsString)
import           Text.Parser.Char
import           Text.Trifecta

import           Blueprint.AST

-- | OperationType can be `query` or `mutation`
--   In the futue, this might be expanded to `subscription`
operationType :: Parser (Maybe OPERATION_TYPE)
operationType =   lookupOp <$> text "query"
              <|> lookupOp <$> text "mutation"

lookupOp :: (Eq a, IsString a) => a -> Maybe OPERATION_TYPE
lookupOp str = lookup str [("query", QUERY)
                          ,("mutation", MUTATION)
                          ]

-- | names can start with _ or letters
--   The rest can be any alphanumeric [char]
name :: Parser [Char]
name = do
  prefixChar <- char '_' <|> letter
  rest <- many alphaNum
  return $ [prefixChar] ++ rest

-- | Commas are optional in graphql
comma :: Parser Char
comma = char ','

-- | http://facebook.github.io/graphql/#WhiteSpace
gSpace :: Parser Char
gSpace = char ' ' <|> char (toEnum 9)

comment :: Parser String
comment = do
  prefixChar <- char '#'
  rest <- many alphaNum
  return $ [prefixChar] ++ rest


-- cr :: Char
-- cr = chr 13

-- lf :: Char
-- lf = chr 10

