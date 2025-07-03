import Readline (addHistory, readline, load_history)

type MalVal = String

-- read

mal_read :: String -> MalVal
mal_read = id

-- eval

eval :: MalVal -> MalVal
eval = id

-- print

mal_print :: MalVal -> String
mal_print = id

-- repl

repl_loop :: IO ()
repl_loop = do
    line <- readline "user> "
    case line of
        Nothing -> return ()
        Just "" -> repl_loop
        Just str -> do
            addHistory str
            let out = mal_print $ eval $ mal_read str
            putStrLn out
            repl_loop

main :: IO ()
main = do
    load_history
    repl_loop
