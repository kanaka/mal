module Readline
( addHistory, readline, load_history )
where

-- Pick one of these:
-- GPL license
import qualified System.Console.Readline as RL
-- BSD license
--import qualified System.Console.Editline.Readline as RL

import Control.Monad (when)
import System.Directory (getHomeDirectory, doesFileExist)
import System.IO.Error (tryIOError)

history_file :: IO String
history_file = do
    home <- getHomeDirectory
    return $ home ++ "/.mal-history"

load_history :: IO ()
load_history = do
    hfile <- history_file
    fileExists <- doesFileExist hfile
    when fileExists $ do
        content <- readFile hfile
        mapM_ RL.addHistory (lines content)

readline :: String -> IO (Maybe String)
readline = RL.readline

addHistory :: String -> IO ()
addHistory line = do
    hfile <- history_file
    _ <- tryIOError (appendFile hfile (line ++ "\n"))
    RL.addHistory line
