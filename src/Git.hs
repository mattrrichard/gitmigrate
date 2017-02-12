module Git where

import           System.Exit        (ExitCode (..))
import           System.Process

-- TODO: these are all nearly identical. please fix

getOrigin :: FilePath -> IO (Maybe String)
getOrigin path = do
  let cmd = (proc "git" ["remote", "get-url", "origin"]) { cwd = Just path }

  (exitCode, out, err) <- readCreateProcessWithExitCode cmd ""

  return $ case exitCode of
    ExitSuccess -> Just out
    _ -> Nothing

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
