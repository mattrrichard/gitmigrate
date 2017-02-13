{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Main where

import           GHC.Generics

import           Control.Lens         (each, over, (&), (.~), (?~), (^.), (^..),
                                       (^?), _Left)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson           (FromJSON, object, (.=))
import           Data.Aeson.Lens      (key, _Array, _String)
import qualified Data.ByteString      as B
import           Data.Char            (toLower)
import           Data.List            (isInfixOf)
import qualified Data.Text            as T
import qualified Data.Yaml            as Y
import qualified Network.Wreq         as W
import qualified System.Directory     as D
import           System.Environment   (getArgs)
import           System.FilePath      ((</>))
import qualified System.FilePath      as F

import qualified Bitbucket            as BB
import qualified Git                  as G
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
  e <- Y.decodeEither <$> B.readFile path
  case e of
    Left err -> fail $ "Could not load config: " ++ err
    Right cfg -> return cfg


type Run = ReaderT Config IO

class Runnable r where
  run :: r a -> ReaderT Config IO a

instance Runnable BB.Bitbucket where
  run action = ReaderT $ WA.runApiSession action . BB.makeConfig . bitbucket

instance Runnable TC.TeamCity where
  run action = ReaderT $ WA.runApiSession action . teamcity

data Error =
    AlreadyBitbucket String
  | DuplicateSlug String
  | NoTeamcityRoot String
  | ErrorCreatingRepo
  | GitError GitOp String String

data GitOp = GetOrigin | SetOrigin | Push deriving (Show)

instance Show Error where
  show (AlreadyBitbucket origin) =  show origin ++ " appears to already be bitbucket."
  show (DuplicateSlug slug) = show slug ++ " is already a repo on bitbucket."
  show (NoTeamcityRoot origin) = "Could not find a teamcity root set up for " ++ show origin
  show ErrorCreatingRepo = "Couldn't create bitbucket repo for unknown reasons..."
  show (GitError op path err) =
    unlines [ "Error performing " ++ show op ++ " at path " ++ show path
            , "Git returned:"
            , err
            ]


(<!>) = flip mapError
mapError f m = ExceptT $
  over _Left f <$> runExceptT m

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither err = maybe (Left err) Right

maybeToError :: Monad m => e -> Maybe a -> ExceptT e m a
maybeToError err = ExceptT . return . maybeToEither err

(?!) :: Monad m => Maybe a -> e -> ExceptT e m a
(?!) = flip maybeToError

(<?!>) :: Monad m => m (Maybe a) -> e -> ExceptT e m a
(<?!>) m err = ExceptT $ maybeToEither err <$> m


report :: MonadIO m => String -> m ()
report = liftIO . putStrLn

type TCRootLookup = [(T.Text, T.Text)]

slowLoadTcRoots :: Run TCRootLookup
slowLoadTcRoots = do
  report "Loading teamcity vcs-roots.  Sometimes this is really slow"
  run TC.getAll

dirname p = do
  path <- liftIO $ D.canonicalizePath p
  return . last . F.splitDirectories $ path

migrateDirectory :: FilePath -> Maybe TCRootLookup -> Run (Either Error TCRootLookup)
migrateDirectory p rootsCache = do
  name <- dirname p
  report $ "Processing " ++ name

  runExceptT $ do
    origin <- G.getOrigin p <!> GitError GetOrigin p

    when ("bitbucket" `isInfixOf` origin)
      (throwError $ AlreadyBitbucket origin)

    report $ "Checking for name collision on " ++ show name
    slugs <- lift $ run BB.getRepoSlugs

    when (T.pack name `elem` slugs)
      (throwError $ DuplicateSlug name)

    roots <- maybe (lift slowLoadTcRoots) return rootsCache
    vcsRootId <- lookup (T.toLower $ T.pack origin) roots ?! NoTeamcityRoot origin
    report $ "Got vcs id " ++ T.unpack vcsRootId

    newRepo <- run (BB.createRepo name) <?!> ErrorCreatingRepo
    report $ "Created new repo at " ++ T.unpack newRepo
    lift . run $ BB.addDeployKeyFromConfig name
    report "Added deploy key"

    G.setOrigin p (T.unpack newRepo) <!> GitError SetOrigin p

    report "Pushing..."
    G.push p <!> GitError Push p

    lift . run $ TC.setVcsUrl (T.unpack vcsRootId) (T.unpack newRepo)

    report $ "Moved " ++ name ++ " to new origin " ++ show newRepo

    return roots


main :: IO ()
main = do
  config <- loadConfig "config.yaml"

  [target] <- getArgs
  dirs <- D.listDirectory target

  preLoadedRoots <- runReaderT slowLoadTcRoots config

  forM_ dirs $ \dir -> do
    let path = target </> dir
    result <- runReaderT (migrateDirectory path (Just preLoadedRoots)) config
    case result of
      Left err -> putStrLn $ "Error: " ++ show err
      Right _ -> putStrLn "Done"

  return ()



  return ()
