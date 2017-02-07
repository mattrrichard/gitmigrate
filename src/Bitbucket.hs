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

  opts <- W.defaults
          & W.param "pagelen" .~ ["100"]
          & auth

  account <- asks account

  let firstPage = makeUrl [account]

  fmap mconcat $ unfoldrM firstPage $ \url -> lift $ do
    resp <- W.getWith opts url

    let slugs = resp ^. W.responseBody . values ^.. traverse . slug
    let nextStart = resp ^? W.responseBody . nextPage

    return (slugs, fmap TS.unpack nextStart)

  where
    values = key "values" . AL._Array
    slug = key "slug" . AL._String
    nextPage = key "next" . AL._String


unfoldrM :: Monad m => a -> (a -> m (b, Maybe a)) -> m [b]
unfoldrM a m = do
    (x, ma) <- m a
    case ma of
      Just a' -> (:) <$> return x <*> unfoldrM a' m
      Nothing -> return [x]


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
