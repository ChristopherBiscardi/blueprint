{-# LANGUAGE OverloadedStrings #-}

module Blueprint
    ( module Blueprint.GraphQL
    ) where

import Data.Text (Text)

import Blueprint.GraphQL
import Blueprint.Parser

-- data Droid = Droid { dName :: Text
--                    } deriving (Show)

-- instance Resolvable Droid where
--   resolve did = Droid "r2d2"

-- instance Introspectable Droid where
--   introspect droid = Info $ Just "A Fricken Bot"

-- test :: Droid
-- test = resolve "1"

-- data Schema = Schema { query :: Maybe Query }

-- data Query = Query { droid :: Droid }

--mkGraphQL :: Schema -> 

-- data Schema a where
--   query :: GraphQLObject a => a -> Schema a
--  mutation :: GraphQLOutputObject








-- instance Resolvable Droid where
--   resolve "name" droid = Blueprint.GraphQL.name droid
-- data Query = Query { hero :: Droid }
-- instance Resolvable Query where
--   resolve "hero" = lookup "hero" database
