{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Blueprint.AST where

import Data.String (IsString(..))
import Data.Text (Text)

-- | Restricted to /[_A-Za-z][_0-9A-Za-z]*/
newtype Name = Name String
  deriving (Eq, Show, IsString)
-- | Name, but not `on`
newtype FragmentName = FragmentName Name
  deriving (Eq, Show)
-- | fragment on $Name, where $Name is the TypeCondition
newtype TypeCondition = TypeCondition Name
  deriving (Eq, Show)

newtype Document = Document [Definition] deriving (Eq, Show);

data Definition = OperationDefinition
                  { odOperationType :: OPERATION_TYPE
                  , odName :: Maybe Name
                  , odVDefs :: Maybe VariableDefinitions
                  , odDirectives :: Maybe [Directive]
                  , odSelectionSet :: SelectionSet
                  }
                | FragmentDefinition
                  { fdName :: FragmentName
                  , fdTypeCond :: TypeCondition
                  , fdDirective :: Maybe [Directive]
                  , fdSelectionSet :: SelectionSet
                  } deriving (Eq, Show)

data OPERATION_TYPE = QUERY | MUTATION deriving (Eq, Show)

data VariableDefinitions = VariableDefinitions deriving (Eq, Show)

data Directive = Directive { name :: Text
                           , args :: [Argument]
                           } deriving (Eq, Show)

data Argument = Argument { aName :: Name
                         , val :: Value
                         } deriving (Eq, Show)

data Value = Variable
           | IntValue
           | FloatValue
           | StringValue
           | BooleanValue
           | EnumValue
           | ListValueConst
           | ObjectValue
             deriving (Eq, Show)

type SelectionSet = [Selection]
data Selection = Field { fAlias :: Maybe Name
                       , fName :: Name
                       , fArgs :: Maybe [Argument]
                       , fDirectives :: Maybe [Directive]
                       , fSelectionSet :: SelectionSet
                       }
               | FragmentSpread FragmentName (Maybe [Directive])
               | InlineFragment TypeCondition (Maybe [Directive]) SelectionSet
               deriving (Eq, Show)
