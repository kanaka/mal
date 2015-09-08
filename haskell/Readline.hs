module Readline
( readline, load_history )
where

-- Pick one of these:
-- GPL license
import qualified System.Console.Readline as RL
-- BSD license
--import qualified System.Console.Editline.Readline as RL

import Control.Monad (when)
import System.Directory (getHomeDirectory, doesFileExist)

import System.IO (hGetLine, hFlush, hIsEOF, stdin, stdout)
import System.IO.Error (tryIOError)

history_file = do
    home <- getHomeDirectory
    return $ home ++ "/.mal-history"

load_history = do
    hfile <- history_file
    fileExists <- doesFileExist hfile
    when fileExists $ do
        content <- readFile hfile
        mapM RL.addHistory (lines content)
        return ()
    return ()

readline prompt = do
    hfile <- history_file
    maybeLine <- RL.readline prompt
    case maybeLine of 
         Just line -> do
             RL.addHistory line
             res <- tryIOError (appendFile hfile (line ++ "\n"))
             return maybeLine
         _ -> return maybeLine
