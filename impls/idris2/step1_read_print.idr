import System.File

import Types
import Reader

read : String -> IO (Maybe AST)
read s =
  case parseText s of
       Right [ast] => pure (Just ast)
       Right _ => do
         putStrLn "wack"
         pure Nothing
       Left e => do
         putStrLn $ "parse error: " ++ e
         pure Nothing

eval : AST -> IO AST
eval = pure

repl : a -> String -> (a -> String -> IO a) -> IO ()
repl state prompt f = do
  putStr prompt
  fflush stdout
  input <- getLine
  False <- fEOF stdin
    | True => putStr "\n"
  f state input
  repl state prompt f

main : IO ()
main = repl () "user> " $ \state, input => do
  Just ast <- read input
    | Nothing => pure ()
  result <- eval ast
  str <- toString True result
  putStrLn str
  pure ()
