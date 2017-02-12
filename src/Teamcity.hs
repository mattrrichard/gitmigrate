{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Teamcity where

import           Control.Lens         ((&), (.~), (?~), (^.), (^..), (^?))
import           Control.Monad        (forM)
import           Data.Aeson           (FromJSON)
import qualified Data.Aeson.Lens      as AL
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import           GHC.Generics
import qualified Network.Wreq         as W
import           WebApi

data Config =
  Config { user     :: String
         , password :: String
         , url      :: String
         }
  deriving (Read, Show, Eq, Generic)

instance FromJSON Config

instance ApiConfig Config where
  baseUrl c = url c ++ "/httpAuth/app/rest"
  apiUser = user
  apiPassword = password

type TeamCity = Api Config

getVcsRoots :: TeamCity [T.Text]
getVcsRoots = do
  resp <- getWith (W.defaults & acceptJson) ["vcs-roots"]

  return $ resp ^. roots ^.. traverse . ids

  where
    roots = W.responseBody . AL.key "vcs-root" . AL._Array
    ids = AL.key "id" . AL._String


getVcsUrl :: String -> TeamCity T.Text
getVcsUrl vcsId = do
  resp <- getWith (W.defaults & acceptPlain) ["vcs-roots", "id:" ++ vcsId, "properties", "url"]

  return $ resp ^. W.responseBody & T.decodeUtf8 . BL.toStrict


getAll :: TeamCity [(T.Text, T.Text)]
getAll = do
  roots <- getVcsRoots

  forM roots $ \r -> do
    u <- getVcsUrl $ T.unpack r
    return (r, u)
