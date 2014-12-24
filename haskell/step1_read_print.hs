import Control.Monad (when)
import Control.Monad.Error (throwError)

import Readline (readline, load_history)
import Types
import Reader (read_str)
import Printer (_pr_str)

-- read
mal_read :: String -> IO MalVal
mal_read str = read_str str

-- eval
eval :: MalVal -> String -> MalVal
eval ast env = ast

-- print
mal_print :: MalVal -> String
mal_print exp = show exp

-- repl
rep :: String -> IO String
rep line = do
    ast <- mal_read line
    return $ mal_print (eval ast "")

repl_loop :: IO ()
repl_loop = do
    line <- readline "user> "
    case line of
        Nothing -> return ()
        Just "" -> repl_loop
        Just str -> do
            out <- catchAny (rep str) $ \e -> do
                return $ "Error: " ++ (show e)
            putStrLn out
            repl_loop

main = do
    load_history
    repl_loop
