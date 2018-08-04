{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( getRunningProcessOnWindow
      , getBranchOnWindow
      , gitPullOnWindow
      , getAllGitRemoteBranches
      , executeScriptOnTmuxWindow
    ) where


import System.Process
import Data.Maybe
import qualified Data.Text as T
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad
import System.Exit
import Text.Printf
import Data.List.Split

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
    return $ removeAllEmptyLines $ map trim  $ (splitOnEndOfLine . removeOrigin) gitRemoteBranchesResult
  where
    removeAllEmptyLines = filter (/= "")
    trim = T.unpack . T.strip . T.pack
    splitOnEndOfLine = splitOn "\n"
    removeOrigin aString  = replace aString "origin/" ""

executeScriptOnTmuxWindow :: String -> [String] -> MaybeT IO String
executeScriptOnTmuxWindow windowNameAndPane scripts = do
    let arguments = ["send-keys", "-t", windowNameAndPane] ++ scripts
    ( exitCode, resultScripts, _ ) <- lift $ readProcessWithExitCode "tmux" arguments []
    guard ( exitCode == ExitSuccess )
    return resultScripts

-- Helper functions
removeEndOfLine :: String -> String
removeEndOfLine = filter (/= '\n')

removeSingleQuotes :: String -> String
removeSingleQuotes = filter (/= '\'')

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)

removeCommandTitle :: String -> String
removeCommandTitle titleWithProcessName = replace titleWithProcessName "COMMAND" ""
