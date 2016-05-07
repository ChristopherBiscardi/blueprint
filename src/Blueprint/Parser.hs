{-# LANGUAGE OverloadedStrings #-}

module Blueprint.Parser where

import Control.Applicative ((<|>))
import Data.Char (chr)
import Data.String (IsString)
import Data.Text (Text)
import Text.Parser.Char
import Text.Trifecta
import Text.Trifecta.Delta

import qualified Blueprint.AST as AST

discardWhitespace :: Parser ()
discardWhitespace = do
  _ <- many gSpace
  return ()

parseGraphQL :: String -> Result AST.Document
parseGraphQL = parseString graphql (Columns 0 0)

graphql :: Parser AST.Document
graphql = AST.Document <$> many definition

definition :: Parser AST.Definition
definition = operationDefinition -- <|> fragmentDefinition

operationDefinition :: Parser AST.Definition
operationDefinition = do
  opType <- operationType
  discardWhitespace
  name' <- optional name
  discardWhitespace
  selectionSet' <- selectionSet
  return $ AST.OperationDefinition opType name' Nothing Nothing selectionSet'

selectionSet :: Parser AST.SelectionSet
selectionSet = braces $ many selection

selection :: Parser AST.Selection
selection = do
  selection' <- choice [field] -- , fragmentSpread, inlineFragment
  return selection'

field :: Parser AST.Selection -- Field
field = do
--  alias' <- optional name
  discardWhitespace
  name' <- name
  discardWhitespace
  -- arguments' <- optional arguments
  -- directives' <- optional directives
  selectionSet' <- optional $ selectionSet
  return $ AST.Field Nothing name' Nothing Nothing selectionSet'

arguments :: Parser [AST.Argument]
arguments = undefined

directives :: Parser [AST.Directive]
directives = undefined

fragmentSpread :: Parser AST.Selection -- FragmentSpread
fragmentSpread = undefined

inlineFragment :: Parser AST.Selection -- InlineFragment
inlineFragment = undefined

-- fragmentDefinition :: Parser FragmentDefinition
-- fragmentDefinition = undefined

-- | OperationType can be `query` or `mutation`
--   In the futue, this might be expanded to `subscription`
operationType :: Parser AST.OPERATION_TYPE
operationType = do
  op <- text "query" <|> text "mutation" :: Parser Text
  case op of
    "query" -> return AST.QUERY
    "mutation" -> return AST.MUTATION
    _ -> fail "operation must be `query` or `mutation`"

-- | names can start with _ or letters
--   The rest can be any alphanumeric [char]
name :: Parser AST.Name
name = do
  prefixChar <- char '_' <|> letter
  rest <- many alphaNum
  return $ AST.Name $ [prefixChar] ++ rest

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

