{-# LANGUAGE OverloadedStrings #-}
module Blueprint.GraphQL where

import Blueprint.AST
import qualified Data.Map as M
import Data.Text (Text)

import Blueprint.FreeHaxl

-- class GraphQLObject a where
--   name :: a -> Text
--   resolve :: a -> m b
--   fields :: a -> Maybe [Text]

-- | used to implement resolving functions
--   IE: using Haxl to fetch 3 droids
class Resolvable a where
  resolve :: oid -> FacebookReq a

-- | A datatype with a toJSON instance
data Info = Info { description :: Maybe Text }

-- | Used to traverse Queries and Mutations in order
--   to provide tools like graphiql with introspection
--   data.
class Introspectable a where
  introspect :: a -> Info

-- data Schema a = Schema { query :: Maybe a
--                      }


-- class Schema b where
--   query :: AST.Field -> b
data Droid = Droid { name :: Text
                   } deriving (Show)

database :: M.Map String Droid
database = M.fromList [("hero", Droid "r2d2")]

execute :: Document -> GVal
execute (Document ops) = executeOp (head ops)

executeOp :: Definition -> GVal
executeOp (OperationDefinition QUERY name Nothing Nothing set)
  = selectSet set
executeOp (FragmentDefinition name cond dir set) = undefined

selectSet :: SelectionSet -> GVal
selectSet fields = foldr resolveSet (GMap M.empty) fields
  -- M.fromList (map (flip resolveField database) set)


-- data GMap = GMap (M.Map String GMap)
--           | GEnd (M.Map String String) deriving Show

data GVal = GMap (M.Map String GVal)
          | GString String
            deriving Show

resolveSet :: Selection -> GVal -> GVal
resolveSet (Field _ (Name name') _ _ []) (GMap accMap)
  = GMap (M.insert name' (GString "yep") accMap)
resolveSet (Field _ (Name name') _ _ set) (GMap accMap)
  = GMap (M.insert name' (selectSet set) accMap)
resolveSet _ _ = undefined
-- resolveField :: Selection -> M.Map k v -> Maybe Droid
-- resolveField (Field _ (Name n) _ _ set) = resolveField set (M.lookup n database)

-- resolveValue :: String -> a
-- resolveValue key = 
