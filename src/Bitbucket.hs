{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitbucket where

import           Control.Lens               ((&), (.~), (?~), (^.), (^..), (^?))
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson                 (FromJSON, ToJSON (..), object,
                                             (.=))
import           Data.Aeson.Lens            (key)
import qualified Data.Aeson.Lens            as AL
import qualified Data.ByteString.Char8      as BS
import           Data.List                  (intersperse)
import           Data.Text                  (Text)
import qualified Data.Text                  as TS
import           GHC.Generics
import qualified Network.Wreq               as W

data Config =
  Config { user     :: String
         , password :: String
         , account  :: String
         }
  deriving (Generic, Read, Show, Eq)

instance FromJSON Config

type Bitbucket a = ReaderT Config IO a


eval = runReaderT

auth :: W.Options -> Bitbucket (W.Options)
auth opts = do
  u <- asks user
  p <- asks password

  return $ opts & W.auth ?~ W.basicAuth (BS.pack u) (BS.pack p)

baseUrl :: String
baseUrl = "https://api.bitbucket.org/2.0/repositories"

getRepoSlugs :: Bitbucket [Text]
getRepoSlugs = do

  opts <- W.defaults & auth

  account <- asks account

  resp <- lift $ W.getWith opts (baseUrl ++ account)

  return $ resp ^. W.responseBody . values ^.. traverse . slug

  where
    values = A.key "values" . A._Array
    slug = A.key "slug" . A._String
createRepo :: Repository -> Bitbucket ()
createRepo repo@(Repository _ slug) = do
  account <- asks account
  opts <- W.defaults
          & W.header "Content-type" .~ ["application/json"]
          & auth

  resp <- lift $ W.postWith opts (makeUrl [account, slug]) (toJSON repo)

  return ()


data Repository =
  Repository { repoName :: String
             , repoSlug :: String
             }


instance ToJSON Repository where
  toJSON (Repository name _) =
    object [ "scm" .= text "git"
           , "is_private" .= True
           , "name" .= name
           , "has_issues" .= False
           ]
    where text x = x :: Text


makeUrl xs = mconcat $ intersperse "/" (baseUrl : xs)
