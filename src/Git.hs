module Git where

import           Control.Monad.Except
import           System.Exit          (ExitCode (..))
import           System.Process

runGit :: MonadIO m => [String] -> FilePath -> ExceptT String m String
runGit args path = ExceptT $ do
  let cmd = (proc "git" args) { cwd = Just path }
  (exitCode, out, err) <- liftIO $ readCreateProcessWithExitCode cmd ""

  return $ case exitCode of
    ExitSuccess -> Right out
    _ -> Left err

getOrigin :: MonadIO m => FilePath -> ExceptT String m String
getOrigin = fmap (head . lines) . runGit ["remote", "get-url", "origin"]

setOrigin :: MonadIO m => FilePath -> String -> ExceptT String m String
setOrigin path url = runGit ["remote", "set-url", "origin", url] path

push :: MonadIO m => FilePath -> ExceptT String m String
push = runGit ["push", "--mirror", "origin"]

clone :: MonadIO m => String -> FilePath -> ExceptT String m String
clone url = runGit ["clone", "--bare", url]

pull :: MonadIO m => FilePath -> ExceptT String m String
pull = runGit ["pull", "--all"]
