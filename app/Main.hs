{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Lib
import Control.Monad (forM, forM_)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Data.Data
import Data.List (find, isPrefixOf)
import System.Environment
import System.Directory (getHomeDirectory)
import Data.Maybe (fromMaybe)
import Data.Aeson
import Safe (headMay)
import Text.PrettyPrint.Tabulate
import qualified GHC.Generics as G
import qualified Text.PrettyPrint.Tabulate as T
import qualified Data.ByteString.Lazy as B


data ProcessOnWindow = ProcessOnWindow {windowName::String, processName::String, branch::String, changedFiles::String, untrackedFiles::String} deriving (Data, G.Generic) 

instance T.Tabulate ProcessOnWindow T.DoNotExpandWhenNested

data WindowScript = WindowScript {
  name:: String,
  scriptInitial:: [String],
  scriptBeforeChangingBranch:: [String],
  scriptFinal:: [String]
} deriving (Show, G.Generic)

instance FromJSON WindowScript
instance ToJSON WindowScript

configFile = "/.tmux-auto/tmux-auto.config"
getJson = B.readFile

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  b <- (eitherDecode <$> getJson (homeDir ++ configFile)) :: IO (Either String [WindowScript])
  listOfWindows <- case b of
        Left err -> do putStrLn err 
                       putStrLn "Error in config: Json Format is invalid!"
                       return []
        Right ps -> return ps
  branchNameArgument <- getArgs
  let targetBranchMaybe = headMay branchNameArgument
  case targetBranchMaybe of 
    Nothing -> putStrLn "Showing status"
    Just targetBranch -> forM_ listOfWindows (executeScripts targetBranch)
  putStrLn "---------------------------"
  status <- forM listOfWindows getStatus
  T.printTable  status
  where
    getStatus window = do
      let windowName = name window
      let windowPane = windowName ++ ":1"
      processName <- runMaybeT $ getRunningProcessOnWindow windowPane
      gitBranch <- runMaybeT $ getBranchOnWindow windowPane
      changedFiles <- runMaybeT $ getNumberOfChangedFiles windowPane
      untrackedFiles <- runMaybeT $ getNumberOfUntrackedFiles windowPane
      let processNameValue = fromMaybe "" processName
      let gitBranchValue = fromMaybe "" gitBranch
      let changedFilesValue = show $ fromMaybe 0 changedFiles
      let untrackedFilesValue = show $ fromMaybe 0 untrackedFiles
      return ProcessOnWindow{ windowName=windowName, processName=processNameValue, branch=gitBranchValue, changedFiles=changedFilesValue, untrackedFiles=untrackedFilesValue}

executeScripts :: String -> WindowScript -> IO()
executeScripts targetBranch window = do
  let windowName = name window
  let windowPane = windowName ++ ":1"
  printInGreenLn $ "Updating ---> " ++ windowName
  _ <- gitPullAndPrintResult windowName
  gitBranchesValue <- getAllBranches windowName
  _ <- mapM putStrLn gitBranchesValue
  currentBranchValue <- getCurrentBranch windowName
  printInYellowLn $ "Current branch:" ++ currentBranchValue
  _ <- runMaybeT $ executeScriptOnTmuxWindow windowPane (scriptInitial window)
  let matchedBranch = matchBranchInBranches gitBranchesValue targetBranch
  changeBranchIfPossibleAndExecScript windowPane matchedBranch (scriptBeforeChangingBranch window)
  _ <- runMaybeT $ executeScriptOnTmuxWindow windowPane (scriptFinal window)
  print "Finished"

changeBranchIfPossibleAndExecScript :: String ->  Maybe String -> [String] -> IO ()
changeBranchIfPossibleAndExecScript windowPane matchedBranch scriptsBeforeChangingBranch =
  case matchedBranch of
    Just newBranch -> do
      beforeChangingRes <- runMaybeT $ executeScriptOnTmuxWindow windowPane scriptsBeforeChangingBranch
      printInMagentaLn $ "Changing to branch:" ++ newBranch
      _ <- runMaybeT $ executeScriptOnTmuxWindow windowPane ["git checkout " ++ newBranch, "Enter"]
      return ()
    Nothing -> putStrLn "Already on branch. Branch not changed"

gitPullAndPrintResult :: String -> IO ()
gitPullAndPrintResult windowName = do
  pullResult <- runMaybeT $ gitPullOnWindow windowName
  let pullResultValue = fromMaybe "" pullResult
  putStrLn $ "Git pull result:" ++ pullResultValue

getAllBranches :: String -> IO [String]
getAllBranches windowName = do
  gitBranches <- runMaybeT $ getAllGitRemoteBranches windowName
  return $ fromMaybe [] gitBranches

getCurrentBranch :: String -> IO String
getCurrentBranch windowName = do
  currentBranch <- runMaybeT $ getBranchOnWindow windowName
  return $ fromMaybe "" currentBranch

matchBranchInBranches :: [String] -> String -> Maybe String
matchBranchInBranches listOfBranches branch =
    case exactMatch of 
      Nothing -> partialMatch
      matchedBranch -> matchedBranch
    where 
      exactMatch = find (== branch) listOfBranches
      partialMatch = find (isPrefixOf $ "feature/" ++ branch) listOfBranches


printInGreenLn aString = putStrLn $ "\x1b[32m" ++ aString ++ "\x1b[0m"
printInYellowLn aString = putStrLn $ "\x1b[33m" ++ aString ++ "\x1b[0m"
printInMagentaLn aString = putStrLn $ "\x1b[35m" ++ aString ++ "\x1b[0m"
