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
import System.Environment
import Data.Maybe (fromMaybe)
import Text.PrettyPrint.Tabulate
import qualified GHC.Generics as G
import qualified Text.PrettyPrint.Tabulate as T

data ProcessOnWindow = ProcessOnWindow {windowName::String, processName::String, branch::String} deriving (Data, G.Generic) 

instance T.Tabulate ProcessOnWindow T.DoNotExpandWhenNested

listOfWindows = ["git-prompt", "tmux-auto"]
-- 1. Git pull
-- b <- GetCurrentBranch
-- a getAllBranches
-- isBrachinList a -> if newBranch!== b executeScriptsWhenChangingBranch ->  checkout
-- isNotInList a -> getBranchThatMach feature/a/... - if newBranch!==b executeScriptsWhenChangingBranch ->  checkout


main :: IO ()
main = do
  branchNameArgument <- getArgs
  print branchNameArgument
  forM_ listOfWindows executeScripts
  putStrLn "---------------------------"
  status <- forM listOfWindows getStatus
  T.printTable  status
  where
    getStatus windowName = do
      processName <- runMaybeT $ getRunningProcessOnWindow $ windowName ++ ":1"
      gitBranch <- runMaybeT $ getBranchOnWindow $ windowName ++ ":1"
      let processNameValue = fromMaybe "" processName
      let gitBranchValue = fromMaybe "" gitBranch
      return ProcessOnWindow{ windowName=windowName, processName=processNameValue, branch=gitBranchValue}
    executeScripts windowName = do 
      printInGreenLn $ "Updating window ---> " ++ windowName
      pullResult <- runMaybeT $ gitPullOnWindow windowName
      let pullResultValue = fromMaybe "" pullResult
      putStrLn $ "Git pull result:" ++ pullResultValue
      gitBranches <- runMaybeT $ getAllGitRemoteBranches windowName
      let gitBranchesValue = fromMaybe [] gitBranches
      _ <- mapM putStrLn gitBranchesValue
      putStrLn "Done"

printInGreenLn aString = putStrLn $ "\x1b[32m" ++ aString ++ "\x1b[0m"
