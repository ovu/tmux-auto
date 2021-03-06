{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( getRunningProcessOnWindow
      , getBranchOnWindow
      , gitPullOnWindow
      , getAllGitRemoteBranches
      , executeScriptOnTmuxWindow
      , getNumberOfChangedFiles
      , getNumberOfUntrackedFiles
    ) where


import System.Process
import Data.Maybe
import Data.List (isPrefixOf)
import qualified Data.Text as T
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad
import System.Exit
import Text.Printf
import Data.List.Split
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import Data.String.Utils (strip, replace)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

getRunningProcessOnWindow :: String -> MaybeT IO String
getRunningProcessOnWindow windowNameAndPane = do 
    ( exitCode, processNumber, _ ) <- lift $ readProcessWithExitCode "tmux" ["list-panes", "-t", windowNameAndPane, "-F", "'#{pane_pid}'"] []
    guard ( exitCode == ExitSuccess )
    getChilProcessName $ removeSingleQuotes $ removeEndOfLine processNumber

getChilProcessName :: String -> MaybeT IO String
getChilProcessName processNumber = do
    ( exitCode, childProcessNumber, _) <- lift $ readProcessWithExitCode "bash" ["-c", "pgrep -P " ++ processNumber] []
    guard ( exitCode == ExitSuccess )
    let childProcessNumberWithoutFormat = removeEndOfLine childProcessNumber
    ( exitCodeChild, childProcessName, _ ) <- lift $ 
                                            readProcessWithExitCode "bash" ["-c", "ps -p " ++  childProcessNumberWithoutFormat ++ " -o command"] []
    guard ( exitCodeChild == ExitSuccess )
    return $ ( last . splitOn "/" ) $ removeCommandTitle $ removeEndOfLine childProcessName

getWindowCurrentDirectory :: String -> MaybeT IO String
getWindowCurrentDirectory windowNameAndPane = do
    ( exitCode, windowDir, _ ) <- lift $ readProcessWithExitCode "tmux" ["list-panes", "-t", windowNameAndPane, "-F", "'#{pane_current_path}'"] []
    guard ( exitCode == ExitSuccess )
    return $ (removeEndOfLine . removeSingleQuotes) windowDir

getBranchOnWindow :: String -> MaybeT IO String
getBranchOnWindow windowNameAndPane = do 
    windowDir <-  getWindowCurrentDirectory windowNameAndPane
    ( exitCodeChild, dirGitBranch, _ ) <- lift $ 
                                            readProcessWithExitCode "bash" ["-c", "git --git-dir " ++  windowDir ++ "/.git rev-parse --abbrev-ref HEAD"] []
    guard ( exitCodeChild == ExitSuccess )
    return $ removeEndOfLine dirGitBranch

gitPullOnWindow :: String -> MaybeT IO String
gitPullOnWindow windowNameAndPane = do 
    windowDir <-  getWindowCurrentDirectory windowNameAndPane
    ( exitCodeChild, gitPullResult, _ ) <- lift $ 
                                            readProcessWithExitCode "bash" ["-c", "git --git-dir " ++  windowDir ++ "/.git pull"] []
    guard ( exitCodeChild == ExitSuccess )
    return gitPullResult

getAllGitRemoteBranches :: String -> MaybeT IO [String]
getAllGitRemoteBranches windowNameAndPane = do 
    windowDir <-  getWindowCurrentDirectory windowNameAndPane
    ( exitCodeChild, gitRemoteBranchesResult, _ ) <- lift $ 
                                            readProcessWithExitCode "bash" ["-c", "git --git-dir " ++  windowDir ++ "/.git branch --remote"] []
    guard ( exitCodeChild == ExitSuccess )
    return $ removeAllEmptyLines $ map strip  $ (splitOnEndOfLine . removeOrigin) gitRemoteBranchesResult
  where
    removeAllEmptyLines = filter (/= "")
    splitOnEndOfLine = splitOn "\n"
    removeOrigin = replace "origin/" ""

executeScriptOnTmuxWindow :: String -> [String] -> MaybeT IO String
executeScriptOnTmuxWindow windowNameAndPane scripts = do
    let arguments = ["send-keys", "-t", windowNameAndPane] ++ scripts
    ( exitCode, resultScripts, _ ) <- lift $ readProcessWithExitCode "tmux" arguments []
    guard ( exitCode == ExitSuccess )
    return resultScripts

getNumberOfChangedFiles :: String -> MaybeT IO Int
getNumberOfChangedFiles windowNameAndPane = do
        windowDir <- getWindowCurrentDirectory windowNameAndPane
        currentDir <- lift getCurrentDirectory
        _ <- lift $ setCurrentDirectory windowDir
        ( exitCode, commandResult, _ ) <- lift $ readProcessWithExitCode "bash" ["-c", "git --git-dir " ++ windowDir ++  "/.git diff --name-status"] []
        if exitCode == ExitSuccess then do
          let changedLines = lines commandResult
          let unmergedFiles = length $ getUnmergedLines changedLines
          let changedFiles = length changedLines - unmergedFiles
          _ <- lift $ setCurrentDirectory currentDir
          return changedFiles
        else do
          _ <- lift $ setCurrentDirectory currentDir
          MaybeT $ return Nothing
    where
      getUnmergedLines = filter $ isPrefixOf "U"

getNumberOfUntrackedFiles :: String -> MaybeT IO Int
getNumberOfUntrackedFiles windowNameAndPane = do
        windowDir <- getWindowCurrentDirectory windowNameAndPane
        currentDir <- lift getCurrentDirectory
        _ <- lift $ setCurrentDirectory windowDir
        ( exitCode, commandResult, _ ) <- lift $ readProcessWithExitCode "bash" ["-c", "git --git-dir " ++ windowDir ++  "/.git status --s -uall"] []
        if exitCode == ExitSuccess then do
          let statusLines = lines commandResult
          let untrackedFiles = length $ getUntrackedLines statusLines
          _ <- lift $ setCurrentDirectory currentDir
          return untrackedFiles
        else do
          _ <- lift $ setCurrentDirectory currentDir
          MaybeT $ return  Nothing
    where getUntrackedLines = filter $ isPrefixOf "??"

-- Helper functions
removeEndOfLine :: String -> String
removeEndOfLine = filter (/= '\n')

removeSingleQuotes :: String -> String
removeSingleQuotes = filter (/= '\'')

removeCommandTitle :: String -> String
removeCommandTitle = replace  "COMMAND" ""
