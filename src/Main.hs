{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Bitbucket          as BB
import           Data.Aeson         (FromJSON)
import qualified Data.ByteString    as BS
import           Data.List          (isInfixOf)
import qualified Data.Yaml          as Y
import           GHC.Generics
import           System.Environment (getArgs)
import           System.Exit        (ExitCode (..))
import           System.Process
import qualified Teamcity           as TC
import qualified Data.Text                  as TS

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


getOrigin :: FilePath -> IO (Maybe String)
getOrigin path = do
  let cmd = (proc "git" ["remote", "get-url", "origin"]) { cwd = Just path }

  (exitCode, out, err) <- readCreateProcessWithExitCode cmd ""

  return $ case exitCode of
    ExitSuccess -> Just out
    _ -> Nothing

--TODO: These need serious refactoring

setOrigin :: FilePath -> String -> IO (Maybe String)
setOrigin path url = do
  let cmd = (proc "git" ["remote", "set-url", "origin", url]) { cwd = Just path }

  (exitCode, out, err) <- readCreateProcessWithExitCode cmd ""
  return $ case exitCode of
    ExitSuccess -> Just out
    _ -> Nothing


push :: FilePath -> IO (Maybe String)
push path = do
  let cmd = (proc "git" ["push", "origin", "master"]) { cwd = Just path }

  (exitCode, out, err) <- readCreateProcessWithExitCode cmd ""
  return $ case exitCode of
    ExitSuccess -> Just out
    _ -> Nothing


-- main :: IO ()
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
  print =<< BB.eval (BB.createRepo (BB.Repository "test" "test")) (bitbucket config)

  return ()
