{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RankNTypes                 #-}

module WebApi
  ( ApiConfig(..)
  , ApiT
  , Api
  , runApiSession
  , runApiWithouSession
  , readConfig
  , addAuthHeader
  , get
  , getAnonymous
  , getWith
  , getWithAnonymous
  , rawGet
  , post
  , postAnonymous
  , postWith
  , postWithAnonymous
  , rawPost
  , makeUrl
  , urlConcat
  , acceptJson
  , acceptPlain
  , contentJson
  ) where

import           Control.Lens          (Lens', (&), (.~), (?~), (^.), (^..),
                                        (^?))
import           Control.Monad.Reader
import           Data.Aeson            (ToJSON (..))
import qualified Data.Aeson.Lens       as AL
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BL
import           Data.List             (intersperse)
import           GHC.Generics
import qualified Network.Wreq          as W
import qualified Network.Wreq.Session  as S
import qualified Network.Wreq.Types    as W (Postable, Putable)


class ApiConfig c where
  baseUrl :: c -> String
  apiUser :: c -> String
  apiPassword :: c -> String


newtype ApiT c m a = Api { runApi :: ReaderT (Maybe S.Session, c) m a }
  deriving (Functor, Applicative, Monad)

instance Monad m => MonadReader c (ApiT c m) where
  ask = readConfig
  local f (Api (ReaderT r)) =
    Api $ ReaderT $ \(s, c) -> r (s, f c)

type Api c = ApiT c IO


runApiSession :: MonadIO m => Api c a -> c -> m a
runApiSession a c = liftIO $ S.withSession $ \s ->
  runReaderT (runApi a) (Just s, c)


runApiWithouSession :: MonadIO m => Api c a -> c -> m a
runApiWithouSession a c = liftIO $ runReaderT (runApi a) (Nothing, c)


readConfig :: Monad m => ApiT c m c
readConfig = Api $ asks snd


makeUrl :: String -> UrlParts -> String
makeUrl base = urlConcat . (base : )


urlConcat :: UrlParts -> String
urlConcat = mconcat . intersperse "/"


acceptJson :: W.Options -> W.Options
acceptJson =
  W.header "accept" .~ ["application/json"]


contentJson :: W.Options -> W.Options
contentJson =
  W.header "Content-type" .~ ["application/json"]


acceptPlain :: W.Options -> W.Options
acceptPlain =
  W.header "accept" .~ ["text/plain"]


type UrlParts = [String]

type ByteStringResponse = W.Response BL.ByteString

addAuthHeader :: ApiConfig c => W.Options -> Api c W.Options
addAuthHeader opts =
  authOpts <$> readConfig
  where
    authOpts c =
      opts & W.auth ?~ W.basicAuth (BS.pack $ apiUser c) (BS.pack $ apiPassword c)


getWith :: ApiConfig c => W.Options -> UrlParts -> Api c ByteStringResponse
getWith opts parts =
  addAuthHeader opts >>= getHelper parts


getHelper :: ApiConfig c => UrlParts -> W.Options -> Api c ByteStringResponse
getHelper parts opts = do
  c <- readConfig
  let url = makeUrl (baseUrl c) parts

  rawGet url opts

rawGet :: ApiConfig c => String -> W.Options -> Api c ByteStringResponse
rawGet url opts = Api $ do
  sess <- asks fst
  lift $ get sess opts url
  where
    get = usingOptionalSession W.getWith S.getWith


get :: ApiConfig c => UrlParts -> Api c ByteStringResponse
get = getWith W.defaults


getAnonymous :: ApiConfig c => UrlParts -> Api c ByteStringResponse
getAnonymous = flip getHelper W.defaults


getWithAnonymous :: ApiConfig c => W.Options -> UrlParts -> Api c ByteStringResponse
getWithAnonymous = flip getHelper


rawPost ::
  (ApiConfig c, W.Postable a)
  => String
  -> W.Options
  -> a
  -> Api c ByteStringResponse
rawPost url opts postData = Api $ do
  sess <- asks fst
  lift $ post sess opts url postData
  where
    post = usingOptionalSession W.postWith S.postWith


postHelper ::
  (ApiConfig c, W.Postable a)
  => UrlParts
  -> a
  -> W.Options
  -> Api c ByteStringResponse
postHelper parts postData opts  = do
  c <- readConfig
  let url = makeUrl (baseUrl c) parts
  rawPost url (defaultJsonContent opts) postData


postWith ::
  (ApiConfig c, W.Postable a)
  => W.Options
  -> UrlParts
  -> a
  -> Api c ByteStringResponse
postWith opts parts postData =
  addAuthHeader opts >>= postHelper parts postData


postWithAnonymous ::
  (ApiConfig c, W.Postable a)
  => W.Options
  -> UrlParts
  -> a
  -> Api c ByteStringResponse
postWithAnonymous opts parts postData =
  postHelper parts postData opts


post ::
  (ApiConfig c, W.Postable a)
  => UrlParts
  -> a
  -> Api c ByteStringResponse
post = postWith W.defaults


postAnonymous ::
  (ApiConfig c, W.Postable a)
  => UrlParts
  -> a
  -> Api c ByteStringResponse
postAnonymous = postWith W.defaults


usingOptionalSession withoutSess withSess =
  maybe withoutSess (flip withSess)
