import System.IO (hFlush, stdout)

import Readline (readline, load_history)

-- read
mal_read str = str

-- eval
eval ast env = ast

-- print
mal_print exp = exp

-- repl
rep line = mal_print $ eval (mal_read line) ""

repl_loop = do
    line <- readline "user> "
    case line of
        Nothing -> return ()
        Just "" -> repl_loop
        Just str -> do
            putStrLn $ rep str
            repl_loop

main = do
    load_history
    repl_loop
