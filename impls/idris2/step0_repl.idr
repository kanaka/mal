import System.File

read : String -> String
read = id

eval : String -> String
eval = id

print : String -> String
print = id

repl : a -> String -> (a -> String -> IO a) -> IO b
repl state prompt f = do
  putStr prompt
  fflush stdout
  input <- getLine
  f state input
  repl state prompt f

main : IO ()
main = repl () "user> " $ \state, input => do
  let result = print $ eval $ read input
  putStrLn result
  pure ()
