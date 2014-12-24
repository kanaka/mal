import System.IO (hGetLine, hFlush, hIsEOF, stdin, stdout)
import Control.Monad

-- read
mal_read str = str

-- eval
eval ast env = ast

-- print
mal_print exp = exp

-- repl
rep line = mal_print $ eval (mal_read line) ""

repl_loop = do
    putStr "user> "
    hFlush stdout
    ineof <- hIsEOF stdin
    when (not ineof) $ do
        line <- hGetLine stdin
        if null line
            then repl_loop
            else do
                putStrLn $ rep line
                repl_loop

main = repl_loop
