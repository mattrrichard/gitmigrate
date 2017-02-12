{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitbucket
  ( Config
  , Bitbucket
  , VConfig
  , makeConfig
  , getRepoSlugs
  , getDeployKeys
  , addDeployKey
  , createRepo
  ) where

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
  Config { user          :: String
         , password      :: String
         , account       :: String
         , deploymentKey :: String
         }
  deriving (Generic, Read, Show, Eq)

instance FromJSON Config

data VConfig = V1 Config | V2 Config

cfg (V1 c) = c
cfg (V2 c) = c

runV1 :: Bitbucket a -> Bitbucket a
runV1 = local $ V1 . cfg

makeConfig :: Config -> VConfig
makeConfig = V2

instance ApiConfig VConfig where
  baseUrl (V1 c) = "https://api.bitbucket.org/1.0/repositories/" ++ account c
  baseUrl (V2 c) = "https://api.bitbucket.org/2.0/repositories/" ++ account c

  apiUser = user . cfg
  apiPassword = password . cfg


type Bitbucket = Api VConfig


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


getDeployKeys :: String -> Bitbucket [T.Text]
getDeployKeys slug =
  runV1 $ do
    r <- get [slug, "deploy-keys"]
    return $ r ^. W.responseBody . _Array ^.. each . key "key" . _String


addDeployKey :: String -> String -> String -> Bitbucket ()
addDeployKey slug label key = do
  runV1 $ do
    let postData = object
                   [ "key" .= key
                   , "label" .= label
                   ]

    post [slug, "deploy-keys"] postData

  return ()


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

