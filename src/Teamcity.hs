{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Teamcity where

import           Data.Aeson   (FromJSON)
import           GHC.Generics

data Config =
  Config { user     :: String
         , password :: String
         , url      :: String
         }
  deriving (Read, Show, Eq, Generic)

instance FromJSON Config
