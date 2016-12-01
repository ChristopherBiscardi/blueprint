{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics

data ContainerCreateResponse = ContainerCreateResponse { id       :: Text
                                                       , warnings :: [Text]
                                                       } deriving (Show, Generic)

instance FromJSON ContainerCreateResponse where
  parseJSON (Object o) = ContainerCreateResponse
                     <$> o .: "Id"
                     <*> o .: "Warnings"

instance ToJSON ContainerCreateResponse

instance GraphQLObject ContainerCreateResponse where
  description = "Contains the information returned to a client on the creation of a new container"
