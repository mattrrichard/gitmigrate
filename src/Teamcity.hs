{-# LANGUAGE OverloadedStrings #-}

module Teamcity where

import           Data.Aeson (FromJSON (..), Value, withObject, (.:))

data Config =
  Config { user     :: String
         , password :: String
         , url  :: String
         }
  deriving (Read, Show, Eq)

instance FromJSON Config where
  parseJSON = withObject "object" $ \o ->
    Config <$> o .: "user"
           <*> o .: "password"
           <*> o .: "url"
