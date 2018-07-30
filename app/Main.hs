module Main where

import Lib
import Control.Monad (forM_)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

listOfWindows = ["git-prompt", "tmux-auto"]

main :: IO ()
main =
  forM_ listOfWindows getStatus
  where
    getStatus windowName = do
      processName <- runMaybeT $ getRunningProcessOnWindow $ windowName ++ ":1"
      case processName of
        Nothing -> putStrLn windowName
        Just name -> putStrLn $ windowName ++ "    " ++ name
