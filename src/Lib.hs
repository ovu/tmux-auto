module Lib
    ( getRunningProcessOnWindow
    ) where

import System.Process
import Data.Maybe
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
    ( exitCodeChild, childProcessName, _ ) <- lift $ readProcessWithExitCode "bash" ["-c", "ps -p " ++  childProcessNumberWithoutFormat] []
    guard ( exitCodeChild == ExitSuccess )
    return $ ( last . splitOn "/" ) $ removeEndOfLine childProcessName

removeEndOfLine :: String -> String
removeEndOfLine = filter (/= '\n')

removeSingleQuotes :: String -> String
removeSingleQuotes = filter (/= '\'')
