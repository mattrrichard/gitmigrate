{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Bitbucket            as BB

import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson           (FromJSON)
import qualified Data.ByteString      as BS
import           Data.List            (isInfixOf)
import           Data.Maybe
import qualified Data.Text            as T
import qualified Data.Yaml            as Y
import           GHC.Generics
import qualified Git                  as G
import           System.Environment   (getArgs)
import qualified Teamcity             as TC
import qualified WebApi               as WA

data Config =
  Config { bitbucket :: BB.Config
         , teamcity  :: TC.Config
         }
  deriving (Read, Show, Eq, Generic)

instance FromJSON Config

loadConfig :: FilePath -> IO Config
loadConfig path = do
  e <- Y.decodeEither <$> BS.readFile path
  case e of
    Left err -> fail $ "Could not load config: " ++ err
    Right cfg -> return cfg












main :: IO ()
main = do
  config <- loadConfig "config.yaml"

  -- slugs <- BB.eval BB.getRepoSlugs $ bitbucket config
  -- print $ slugs

  -- origin <- getOrigin "."

  -- print $ maybe False (isInfixOf "bitbucket") origin

  -- (r : roots) <- TC.eval TC.getVcsRoots (teamcity config)
  -- r' <- TC.eval (TC.getVcsUrl (TS.unpack r)) (teamcity config)
  -- print r'
  -- roots <- TC.eval TC.getAll (teamcity config)

  -- mapM print roots
  -- push "." >>= print
  -- print =<< TC.eval TC.getAll (teamcity config)
  -- print =<< BB.eval (BB.createRepo (BB.Repository "test" "test")) (bitbucket config)


  return ()
