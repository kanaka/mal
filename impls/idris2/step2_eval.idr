import System.File

import Types
import MonadTrans
import Reader
import Eval

read : String -> MalM (Maybe AST)
read s =
  case parseText s of
       Right [ast] => pure (Just ast)
       Right [] => pure Nothing
       Right _ => throwError $ Str "wack" -- too many ast's
       Left e => throwError $ Str $ "parse error: " ++ e

runMalM : MalM () -> StateT Env IO ()
runMalM x = ST $ \st => do
  Left e <- runExceptT $ runStateT x st
    | Right x => pure x
  putStr "error: "
  printLn e
  pure ((), st)

repl : a -> String -> (String -> StateT a IO ()) -> IO ()
repl state prompt f = do
  putStr prompt
  fflush stdout
  input <- getLine
  False <- fEOF stdin
    | True => putStr "\n"
  ((), state') <- runStateT (f input) state
  repl state' prompt f

main : IO ()
main = repl startingEnv "user> " $ \input => runMalM $ do
  Just ast <- read input
    | Nothing => pure ()
  result <- eval ast
  MonadTrans.liftIO $ printLn result
