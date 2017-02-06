{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bitbucket where

import           Control.Lens               ((&), (?~), (^.), (^..), (^?))
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson                 (FromJSON (..), withObject, (.:))
import qualified Data.Aeson.Lens            as A
import qualified Data.ByteString.Char8      as BS
import qualified Data.Text                  as TS
import qualified Network.Wreq               as W

data Config =
  Config { user     :: String
         , password :: String
         , account  :: String
         }
  deriving (Read, Show, Eq)

instance FromJSON Config where
  parseJSON = withObject "object" $ \o ->
    Config <$> o .: "user"
           <*> o .: "password"
           <*> o .: "account"

baseUrl :: String
baseUrl = "https://api.bitbucket.org/2.0/repositories/"


type Bitbucket a = ReaderT Config IO a

eval = runReaderT

auth :: W.Options -> Bitbucket (W.Options)
auth opts = do
  u <- asks user
  p <- asks password

  return $ opts & W.auth ?~ W.basicAuth (BS.pack u) (BS.pack p)

type Slug = TS.Text

getRepoSlugs :: Bitbucket [Slug]
getRepoSlugs = do

  opts <- W.defaults & auth

  account <- asks account

  resp <- lift $ W.getWith opts (baseUrl ++ account)

  return $ resp ^. W.responseBody . values ^.. traverse . slug

  where
    values = A.key "values" . A._Array
    slug = A.key "slug" . A._String
