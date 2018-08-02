{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where


import Lib
import Control.Monad (forM, forM_)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Data.Data
import Data.Maybe (fromMaybe)
import Text.PrettyPrint.Tabulate
import qualified GHC.Generics as G
import qualified Text.PrettyPrint.Tabulate as T

data ProcessOnWindow = ProcessOnWindow {windowName::String, processName::String, branch::String} deriving (Data, G.Generic) 

instance T.Tabulate ProcessOnWindow T.DoNotExpandWhenNested

listOfWindows = ["git-prompt", "tmux-auto"]


main :: IO ()
main = do
  status <- forM listOfWindows getStatus
  T.printTable  status
  where
    getStatus windowName = do
      processName <- runMaybeT $ getRunningProcessOnWindow $ windowName ++ ":1"
      gitBranch <- runMaybeT $ getBranchOnWindow $ windowName ++ ":1"
      let processNameValue = fromMaybe "" processName
      let gitBranchValue = fromMaybe "" gitBranch
      return ProcessOnWindow{ windowName=windowName, processName=processNameValue, branch=gitBranchValue}
