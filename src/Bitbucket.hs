{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitbucket where

import           Control.Lens          (each, (&), (^.), (^..), (^?), (.~), (?~))
import           Control.Monad.Reader
import           Data.Aeson            (FromJSON, object, (.=))
import           Data.Aeson.Lens       (key, _Array, _String)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T
import           GHC.Generics
import qualified Network.Wreq          as W
import           WebApi

data Config =
  Config { user     :: String
         , password :: String
         , account  :: String
         }
  deriving (Generic, Read, Show, Eq)

instance FromJSON Config





instance ApiConfig Config where
  baseUrl c = "https://api.bitbucket.org/2.0/repositories/" ++ account c
  apiUser = user
  apiPassword = password


type Bitbucket = Api Config


getRepoSlugs :: Bitbucket [T.Text]
getRepoSlugs = do

  let opts = W.defaults & W.param "pagelen" .~ ["100"]

  fmap mconcat $ unfoldrM 1 $ \page -> do
    let opts' = opts & W.param "page" .~ [T.pack $ show page]
    resp <- getWith opts' []

    let slugs = resp ^. W.responseBody . values ^.. traverse . slug
    let nextStart = resp ^? W.responseBody . nextPage

    return (slugs, fmap (const $ page + 1) nextStart)

  where
    values = key "values" . _Array
    slug = key "slug" . _String
    nextPage = key "next" . _String


-- I think this is a bit of a bastardization because
-- typically the step function is `a -> m (Maybe (b, a))`
-- but this form was more convenient here.
unfoldrM :: Monad m => a -> (a -> m (b, Maybe a)) -> m [b]
unfoldrM a m = do
    (x, ma) <- m a
    case ma of
      Just a' -> (:) <$> return x <*> unfoldrM a' m
      Nothing -> return [x]








createRepo :: String -> Bitbucket (Maybe T.Text)
createRepo slug = do
  let opts = W.defaults & contentJson

  let postData = object [ "scm" .= ("git" :: T.Text)
                        , "is_private" .= True
                        , "name" .= slug
                        , "has_issues" .= False
                        ]

  resp <- postWith opts [slug] postData

  -- this is awful, but it works
  -- awful doesn't cover it.  holy shit
  let cloneLinks = resp ^. W.responseBody . key "links" . key "clone" . _Array
  let names = cloneLinks ^.. traverse . key "name" . _String
  let hrefs = cloneLinks ^.. traverse . key "href" . _String

  return $ lookup "ssh" (zip names hrefs)



