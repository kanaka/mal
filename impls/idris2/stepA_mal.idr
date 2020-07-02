import System
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

runMalM : MalM a -> ReaderT Env IO ()
runMalM x = MkReaderT $ \r => do
  Right _ <- runExceptT $ runReaderT x r
    | Left e => do
      se <- toString False e
      putStrLn $ "error: " ++ se
  pure ()

repl : r -> String -> (String -> ReaderT r IO ()) -> IO ()
repl r prompt f = do
  putStr prompt
  fflush stdout
  input <- getLine
  False <- fEOF stdin
    | True => putStr "\n"
  runReaderT (f input) r
  repl r prompt f

doRepl : Env -> IO ()
doRepl startingEnv = do
  flip runReaderT startingEnv $ runMalM $ do
    Just x <- read "(println (str \"Mal [\" *host-language* \"]\"))"
      | Nothing => pure ()
    eval x
    pure ()
  repl startingEnv "user> " $ \input => runMalM $ do
    Just ast <- read input
      | Nothing => pure ()
    result <- eval ast
    str <- MonadTrans.liftIO $ toString True result
    MonadTrans.liftIO $ putStrLn str

main : IO ()
main = do
  startingEnv <- getStartingEnv
  args <- getArgs
  case args of
       (_::f::_) => flip runReaderT startingEnv $ runMalM $
           eval $ List False [Symbol "load-file", Str f]
       _ => doRepl startingEnv
