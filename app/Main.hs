{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where


import Lib
import Control.Monad (forM, forM_)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Data.Data
import Text.PrettyPrint.Tabulate
import qualified GHC.Generics as G
import qualified Text.PrettyPrint.Tabulate as T

data ProcessOnWindow = ProcessOnWindow {windowName::String, processName::String} deriving (Data, G.Generic) 

instance T.Tabulate ProcessOnWindow T.DoNotExpandWhenNested

listOfWindows = ["git-prompt", "tmux-auto"]


main :: IO ()
main = do
  status <- forM listOfWindows getStatus
  T.printTable  status
  where
    getStatus windowName = do
      processName <- runMaybeT $ getRunningProcessOnWindow $ windowName ++ ":1"
      case processName of
        Nothing -> return ProcessOnWindow{ windowName=windowName, processName=""}
        Just name -> return ProcessOnWindow{ windowName=windowName, processName=name}

