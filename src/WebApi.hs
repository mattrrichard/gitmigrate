{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module WebApi where

import           Control.Lens               ((&), (.~), (?~), (^.), (^..), (^?), lens, Lens', view, set)
import qualified Control.Lens               as L
import           Control.Lens.Reified       (ReifiedGetter(..), runGetter)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson                 (FromJSON, ToJSON (..), object,
                                             (.=))
import           Data.Aeson.Lens            (key)
import qualified Data.Aeson.Lens            as AL
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as BL
import           Data.List                  (intersperse)
import           Data.Text                  (Text)
import qualified Data.Text                  as TS
import           GHC.Generics
import qualified Network.Wreq.Session as S
import qualified Network.Wreq as W
import           Network.Wreq.Types (Postable)


class ApiConfig c where
  baseUrl :: c -> String
  apiUser :: c -> String
  apiPassword :: c -> String


newtype Api c a = Api { runApi :: ReaderT (Maybe S.Session, c) IO a }
  deriving (Functor, Applicative, Monad)

evalWithSession :: Api c a -> c -> IO a
evalWithSession a c = S.withSession $ \s ->
  runReaderT (runApi a) (Just s, c)

eval :: Api c a -> c -> IO a
eval a c = runReaderT (runApi a) (Nothing, c)

config :: Api c c
config = Api $ asks snd

acceptJson =
  W.header "accept" .~ ["application/json"]

contentJson =
  W.header "Content-type" .~ ["application/json"]

acceptPlain =
  W.header "accept" .~ ["text/plain"]

defaultOpts :: ApiConfig c => c -> W.Options -> W.Options
defaultOpts c opts =
  opts & W.auth ?~ W.basicAuth (BS.pack $ apiUser c) (BS.pack $ apiPassword c)

type UrlParts = [String]


getWith :: ApiConfig c => W.Options -> UrlParts -> Api c (W.Response BL.ByteString)
getWith opts parts = Api $ do
  (sess, c) <- ask

  let url = makeUrl (baseUrl c) parts
  let opts' = defaultOpts c opts

  lift $ usingOptionalSession sess W.getWith S.getWith opts' url

get :: ApiConfig c => UrlParts -> Api c (W.Response BL.ByteString)
get = getWith W.defaults


postWith :: (ApiConfig c, Postable a) => W.Options -> UrlParts -> a -> Api c (W.Response BL.ByteString)
postWith opts parts postData = Api $ do
  (sess, c) <- ask

  let url = makeUrl (baseUrl c) parts
  let opts' = defaultOpts c opts

  lift $ maybe
    (W.postWith opts' url postData)
    (\s -> S.postWith opts' s url postData)
    sess

  lift $ usingOptionalSession sess W.postWith S.postWith opts' url postData

post :: (ApiConfig c, Postable a) => UrlParts -> a -> Api c (W.Response BL.ByteString)
post = postWith W.defaults


usingOptionalSession sess withoutSess withSess =
  maybe withoutSess (flip withSess) sess


makeUrl base = urlConcat . (base : )

urlConcat = mconcat . intersperse "/"
