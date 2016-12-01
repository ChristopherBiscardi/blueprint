{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Blueprint.Schema where

import qualified Data.Map.Strict as Map
import Text.Trifecta
import Text.Trifecta.Delta
import Data.String (IsString(..))
import Control.Applicative ((<|>))
import Control.Lens
import Data.Either (rights, lefts)

import qualified Blueprint.Parser as BP
import qualified Blueprint.AST as AST

data GraphQLObject = GraphQLObject { _name :: String
        --                           , interfaces? :: GraphQLInterfacesThunk | Array<GraphQLInterfaceType>;
                                   , _fields :: Map.Map FieldName GraphQLFieldConfig
        --                           , isTypeOf?: (value: any, info?: GraphQLResolveInfo) => boolean;
                                   , _description :: Maybe String
                                   } deriving (Show, Eq)
newtype GraphQLOutputType = GraphQLOutputType String deriving (Show, Eq, IsString)
newtype GraphQLInputType = GraphQLInputType String deriving (Show, Eq, IsString)
newtype FieldName = FieldName String deriving (Show, Eq, Ord, IsString)
data GraphQLFieldConfig = GraphQLFieldConfig
  { _fType :: GraphQLOutputType
  , _fArgs :: Map.Map String (GraphQLArgumentConfig String)
  , _fDeprecationReason :: Maybe String
  , _fDescription :: Maybe String
  } deriving (Show, Eq)
data GraphQLArgumentConfig a = GraphQLArgumentConfig
  { _acType :: GraphQLInputType
  , _acDefaultValue :: Maybe a
  , _acDescription :: Maybe String
  } deriving (Show, Eq)
data Schema = Schema { mutation :: Maybe GraphQLObject
                     , query :: GraphQLObject
                     }

makeLenses ''GraphQLObject
makeLenses ''GraphQLFieldConfig
makeLenses ''GraphQLArgumentConfig

parseSchema :: String -> Result [GraphQLObject]
parseSchema = parseString typesParser (Columns 0 0)

isBaseScalar :: String -> Bool
isBaseScalar dep = dep `elem` ["String"]

schemaParser :: String -> [Either String [GraphQLObject]]
schemaParser str = do
  case parseSchema str of
    Failure aDoc -> Left $ show aDoc
    Success types -> do
      let typeNames = map (name) types
          validations = traverse (typeHasAllDependencies typeNames) types
          errorsList = lefts validations
      case length errorsList /= 0 of
        True -> errorsList
        False -> rights validations

-- | Given a GraphQLType definition, make sure all dependencies of
--   said type have their own definitions.
type ErrorMessage = String
type DefinedGraphQLTypes = [String]
type TypeName = String
typeHasAllDependencies :: DefinedGraphQLTypes -> GraphQLObject -> Maybe [ErrorMessage]
typeHasAllDependencies typeNames (GraphQLObject name' fields' _) = do
  let dependentTypes = concatMap getDependentTypes (Map.elems fields')
      errors = map (\(Just s) -> "GraphQL Type " ++ name' ++ " has unfufilled dependencies " ++ show dependency) (catMaybes dependentTypes)
--      dependencies = map (fieldsExist typeNames) (Map.elems fields')
  return undefined
  where
    -- | TypeName exists as a defined type or as a base Scalar
    isDefinedType :: TypeName -> [DefinedGraphQLTypes] -> Bool
    isDefinedType depName typeNames = depName `elem` typeNames || isBaseScalar depName
    fieldsExist :: [DefinedGraphQLTypes] -> GraphQLFieldConfig -> Maybe TypeName
    fieldsExist types (GraphQLFieldConfig dependencyName _ _ _) =
      case isDefinedType (getName dependencyName) types of
        True -> Nothing
        False -> Just dependencyName
    -- | Get a list of all types our current type depends on
    getDependentTypes :: GraphQLFieldConfig -> [String]
    getDependentTypes (GraphQLFieldConfig outputType args _ _) = do
      let t = args^..traverse.acType :: [GraphQLInputType]
      return $ map (\(GraphQLInputType n) -> n) t
    getName (GraphQLOutputType str) = str

typesParser :: Parser [GraphQLObject]
typesParser = some graphqlTypeParser

graphqlTypeParser :: Parser GraphQLObject
graphqlTypeParser = do
  _ <- string "type"
  _ <- whiteSpace
  AST.Name name <- BP.name
  _ <- whiteSpace
  fields <- between (symbol "{") (symbol "}") fieldsParser
  return $ GraphQLObject name fields Nothing

fieldsParser :: Parser (Map.Map FieldName GraphQLFieldConfig)
fieldsParser = do
  _ <- optional newline
  fields <- sepEndBy1 fieldParser newline
  return $ Map.fromList fields

-- | hero(episode: Episode): Character
fieldParser :: Parser (FieldName, GraphQLFieldConfig)
fieldParser = do
  let argumentSeparator = symbol ","
      betweenParens = between (symbol "(") (symbol ")")
  _ <- optional whiteSpace
  AST.Name name <- BP.name
  optionalArguments <- optional (betweenParens $ sepBy argumentParser argumentSeparator)
  _ <- char ':'
  _ <- optional whiteSpace
  AST.Name outputType <- BP.name
  -- | TODO: Detect and apply required ! to types
  isRequired <- optional $ char '!'
  return $ (FieldName name, GraphQLFieldConfig { _fType=GraphQLOutputType outputType
                                               , _fArgs=Map.fromList $ case optionalArguments of
                                                   Just arguments -> arguments
                                                   Nothing -> []
                                               , _fDeprecationReason=Nothing
                                               , _fDescription=Nothing
                                               })

-- | episode: Episode
argumentParser :: Parser (String, GraphQLArgumentConfig String)
argumentParser = do
  AST.Name name <- BP.name
  _ <- char ':'
  _ <- whiteSpace
  AST.Name argType <- BP.name
  -- | TODO: Detect and apply required ! to types
  isRequired <- optional $ char '!'
  defaultValue <- optional defaultValueParser
  return $ (name, GraphQLArgumentConfig { _acType=GraphQLInputType argType
                                        , _acDefaultValue=defaultValue
                                        , _acDescription=Nothing
                                        })

-- | parses `= thing`
defaultValueParser :: Parser String
defaultValueParser = do
  _ <- optional whiteSpace
  _ <- char '='
  _ <- optional whiteSpace
  AST.Name defaultValue <- BP.name
  return defaultValue

data GraphQLEnum = GraphQLEnum { eName :: String
                               , eOptions :: [String]
                               } deriving (Show, Eq)
enumParser :: Parser GraphQLEnum
enumParser = do
  _ <- string "enum"
  _ <- whiteSpace
  name <- many (char '_' <|> letter)
  _ <- whiteSpace
  opts <- between (symbol "{") (symbol "}") optsParser
  return $ GraphQLEnum name opts

optsParser :: Parser [String]
optsParser = do
  _ <- optional newline
  opts <- sepEndBy1 (do
                        _ <- optional whiteSpace
                        optValue <- some (char '_' <|> letter)
                        return optValue
                    ) newline
  return opts
