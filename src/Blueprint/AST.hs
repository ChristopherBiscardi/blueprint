module Blueprint.AST where

import Data.Text (Text)

-- | Restricted to /[_A-Za-z][_0-9A-Za-z]*/
newtype Name = Name Text
-- | Name, but not `on`
newtype FragmentName = FragmentName Name
-- | fragment on $Name, where $Name is the TypeCondition
newtype TypeCondition = TypeCondition Name
newtype Alias = Alias Name


newtype Document = Document [Definition]

data Definition = OperationDefinition OPERATION_TYPE Name VariableDefinitions [Directive] SelectionSet
                | FragmentDefinition FragmentName TypeCondition [Directive] SelectionSet

data OPERATION_TYPE = QUERY | MUTATION deriving Show

data VariableDefinitions = VariableDefinitions

data Directive = Directive { name :: Text
                           , args :: [Argument]
                           }

data Argument = Argument { aName :: Name
                         , val :: Value
                         }

data Value = Variable
           | IntValue
           | FloatValue
           | StringValue
           | BooleanValue
           | EnumValue
           | ListValueConst
           | ObjectValue

data SelectionSet = SelectionSet [Selection]
data Selection = Selection Field FragmentSpread InlineFragment
data Field = Field { fAlias :: Maybe Alias
                   , fName :: Maybe Name
                   , fArgs :: [Argument]
                   , fDirectives :: [Directive]
                   , fSelectionSet :: Maybe SelectionSet
                   }
data FragmentSpread = FragmentSpread FragmentName [Directive]
data InlineFragment = InlineFragment TypeCondition [Directive] SelectionSet
