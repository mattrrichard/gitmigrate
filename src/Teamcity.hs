{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Teamcity where

import           Control.Lens               ((&), (.~), (?~), (^.), (^..), (^?))
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson                 (FromJSON, ToJSON (..), object,
                                             (.=))
import           Data.Aeson.Lens            (key)
import qualified Data.Aeson.Lens            as AL
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy       as BL
import           Data.List                  (intersperse)
import           Data.Text                  (Text)
import qualified Data.Text                  as TS
import qualified Data.Text.Encoding         as T
import           GHC.Generics
import qualified Network.Wreq               as W
import qualified Network.Wreq.Session       as S

data Config =
  Config { user     :: String
         , password :: String
         , url      :: String
         }
  deriving (Read, Show, Eq, Generic)

instance FromJSON Config

type Teamcity a = ReaderT Config IO a

eval :: Teamcity a -> Config -> IO a
eval = runReaderT

acceptJson =
  W.header "accept" .~ ["application/json"]

contentJson =
  W.header "Content-type" .~ ["application/json"]

acceptPlain =
  W.header "accept" .~ ["text/plain"]

defaultOpts :: Teamcity W.Options
defaultOpts = do
  user <- asks user
  password <- asks password

  return $
    W.defaults
    & W.auth ?~ W.basicAuth (B.pack user) (B.pack password)
    & contentJson


getVcsRoots :: Teamcity [Text]
getVcsRoots = do
  url <- makeUrl ["vcs-roots"]
  opts <- acceptJson <$> defaultOpts

  resp <- lift $ W.getWith opts url

  return $ resp ^. roots ^.. traverse . ids

  where
    roots = W.responseBody . key "vcs-root" . AL._Array
    ids = key "id" . AL._String

getVcsUrl :: String -> Teamcity Text
getVcsUrl vcsId = do
  url <- makeUrl ["vcs-roots", "id:" ++ vcsId, "properties", "url"]
  opts <- acceptPlain <$> defaultOpts

  resp <- lift $ W.getWith opts url

  return $ resp ^. W.responseBody & T.decodeUtf8 . BL.toStrict


getAll :: Teamcity [(Text, Text)]
getAll = do
  roots <- getVcsRoots

  flip mapM roots $ \r -> do
    u <- getVcsUrl $ TS.unpack r
    return (r, u)

makeUrl :: [String] -> Teamcity String
makeUrl xs = do
  baseUrl <- asks url
  return $ mconcat $ intersperse "/" (baseUrl : ["httpAuth/app/rest"] ++ xs)
