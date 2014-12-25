module Readline
( readline, load_history )
where

-- Pick one of these:
-- GPL license
import qualified System.Console.Readline as RL
-- BSD license
--import qualified System.Console.Editline.Readline as RL

import System.Directory (getHomeDirectory)

import System.IO (hGetLine, hFlush, hIsEOF, stdin, stdout)

history_file = do
    home <- getHomeDirectory
    return $ home ++ "/.mal-history"

load_history = do
    hfile <- history_file
    content <- readFile hfile
    mapM RL.addHistory (lines content)

readline prompt = do
    hfile <- history_file
    maybeLine <- RL.readline prompt
    case maybeLine of 
         Just line -> do
             appendFile hfile (line ++ "\n")
             RL.addHistory line
             return maybeLine
         _ -> return maybeLine
