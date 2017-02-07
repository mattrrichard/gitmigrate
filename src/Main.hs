{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Bitbucket       as BB
import           Data.Aeson      (FromJSON)
import qualified Data.ByteString as BS
import qualified Data.Yaml       as Y
import           GHC.Generics
import qualified Teamcity        as TC

data Config =
  Config { bitbucket :: BB.Config
         , teamcity  :: TC.Config
         }
  deriving (Read, Show, Eq, Generic)

instance FromJSON Config

loadConfig :: FilePath -> IO (Either String Config)
loadConfig path =
    Y.decodeEither <$> BS.readFile path

main :: IO ()
main = do
  config <- loadConfig "config.yaml" >>= \e ->
    case e of
      Left err -> fail $ "Could not load config: " ++ err
      Right cfg -> return cfg

  slugs <- BB.eval (BB.getRepoSlugs) (bitbucket config)
  print slugs

