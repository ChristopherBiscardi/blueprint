{-# LANGUAGE OverloadedStrings #-}

module Blueprint
    ( module Blueprint.GraphQL
    ) where

import Data.Text (Text)

import Blueprint.GraphQL

data Droid = Droid { dName :: Text
                   } deriving (Show)

instance Resolve Droid where
  resolve did = Droid "r2d2"

test :: Droid
test = resolve "1"
