import System.IO (hFlush, stdout)
import Control.Monad.Except (liftIO, runExceptT)

import Readline (addHistory, readline, load_history)
import Types
import Reader (read_str)
import Printer (_pr_str)

-- read

mal_read :: String -> IOThrows MalVal
mal_read = read_str

-- eval

eval :: MalVal -> MalVal
eval = id

-- print

mal_print :: MalVal -> IOThrows String
mal_print = liftIO . Printer._pr_str True

-- repl

rep :: String -> IOThrows String
rep line = mal_print =<< (eval <$> mal_read line)

repl_loop :: IO ()
repl_loop = do
    line <- readline "user> "
    case line of
        Nothing -> return ()
        Just "" -> repl_loop
        Just str -> do
            addHistory str
            res <- runExceptT $ rep str
            out <- case res of
                Left mv -> (++) "Error: " <$> liftIO (Printer._pr_str True mv)
                Right val -> return val
            putStrLn out
            hFlush stdout
            repl_loop

main :: IO ()
main = do
    load_history

    repl_loop
