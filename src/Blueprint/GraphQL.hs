module Blueprint.GraphQL where

import           Data.Text (Text)

-- class GraphQLObject a where
--   name :: a -> Text
--   resolve :: a -> m b
--   fields :: a -> Maybe [Text]

class Resolve a where
  resolve :: oid -> a

data Schema a = Schema { query :: Maybe a
                     }

