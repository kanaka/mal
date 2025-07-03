import Control.Monad.Except (runExceptT)

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

mal_print :: MalVal -> IO String
mal_print = _pr_str True

-- repl

repl_loop :: IO ()
repl_loop = do
    line <- readline "user> "
    case line of
        Nothing -> return ()
        Just "" -> repl_loop
        Just str -> do
            addHistory str
            res <- runExceptT $ eval <$> mal_read str
            out <- case res of
                Left mv -> (++) "Error: " <$> mal_print mv
                Right val -> mal_print val
            putStrLn out
            repl_loop

main :: IO ()
main = do
    load_history
    repl_loop
