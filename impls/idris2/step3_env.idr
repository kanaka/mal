import System.File

import Types
import MonadTrans
import Reader
import Eval
import Core

read : String -> MalM (Maybe AST)
read s =
  case parseText s of
       Right [ast] => pure (Just ast)
       Right [] => pure Nothing
       Right _ => throwError $ Str "wack" -- too many ast's
       Left e => throwError $ Str $ "parse error: " ++ e

runMalM : MalM () -> ReaderT Env IO ()
runMalM x = MkReaderT $ \r => do
  Right x <- runExceptT $ runReaderT x r
    | Left e => putStrLn $ "error: " ++ show e
  pure x

repl : r -> String -> (String -> ReaderT r IO ()) -> IO ()
repl r prompt f = do
  putStr prompt
  fflush stdout
  input <- getLine
  False <- fEOF stdin
    | True => putStr "\n"
  runReaderT (f input) r
  repl r prompt f

main : IO ()
main = do 
  startingEnv <- getStartingEnv
  repl startingEnv "user> " $ \input => runMalM $ do
    Just ast <- read input
      | Nothing => pure ()
    result <- eval ast
    MonadTrans.liftIO $ printLn result
