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
  pullResult <- runMaybeT $ gitPullOnWindow windowName
  let pullResultValue = fromMaybe "" pullResult
  putStrLn $ "Git pull result:" ++ pullResultValue
  gitBranches <- runMaybeT $ getAllGitRemoteBranches windowName
  let gitBranchesValue = fromMaybe [] gitBranches
  _ <- mapM putStrLn gitBranchesValue
  currentBranch <- runMaybeT $ getBranchOnWindow windowName
  let currentBranchValue = fromMaybe "" currentBranch
  printInYellowLn $ "Current branch:" ++ currentBranchValue
  let changeBranch = wantsToChangeBranch currentBranch gitBranches targetBranch
  let changeBranchValue = fromMaybe (False, "") changeBranch
  putStrLn $ show changeBranch
  resultInitialScripts <- runMaybeT $ executeScriptOnTmuxWindow windowPane (scriptInitial window)
  putStrLn $ "Initial script result:" ++ show resultInitialScripts
  if fst changeBranchValue then do 
    beforeChangingRes <- runMaybeT $ executeScriptOnTmuxWindow windowPane (scriptBeforeChangingBranch window)
    putStrLn $ "Before changing branch result:" ++  show beforeChangingRes
    printInMagentaLn $ "Changing to branch:" ++ snd changeBranchValue
    changeBranchRes <- runMaybeT $ executeScriptOnTmuxWindow windowPane ["git checkout " ++ snd changeBranchValue, "Enter"]
    putStrLn $ "Checkout branch result:" ++  show changeBranchRes
  else
    putStrLn "Already on branch. Branch not changed"
  resultFinalScripts <- runMaybeT $ executeScriptOnTmuxWindow windowPane (scriptFinal window)
  putStrLn $ "Final script result:" ++ show resultFinalScripts

wantsToChangeBranch :: Maybe String -> Maybe [String] -> String -> Maybe (Bool, String)
wantsToChangeBranch currentBranch gitBranches targetBranch = do
    current <- currentBranch
    branches <- gitBranches 
    matchedBranch <- matchBranchInBranches branches targetBranch
    return (matchedBranch /= current, matchedBranch)


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
