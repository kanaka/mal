import System.IO (hFlush, stdout)
import Control.Monad.Except (liftIO, runExceptT)
import Data.Foldable (foldlM)

import Readline (addHistory, readline, load_history)
import Types
import Reader (read_str)
import Printer(_pr_list, _pr_str)
import Env (Env, env_apply, env_get, env_let, env_put, env_repl, env_set)
import Core (ns)

-- read

mal_read :: String -> IOThrows MalVal
mal_read = read_str

-- eval

let_bind :: Env -> [MalVal] -> IOThrows ()
let_bind _ [] = return ()
let_bind env (MalSymbol b : e : xs) = do
    liftIO . env_set env b =<< eval env e
    let_bind env xs
let_bind _ _ = throwStr "invalid let*"

apply_ast :: MalVal -> [MalVal] -> Env -> IOThrows MalVal

apply_ast (MalSymbol "def!") [MalSymbol a1, a2] env = do
    evd <- eval env a2
    liftIO $ env_set env a1 evd
    return evd
apply_ast (MalSymbol "def!") _ _ = throwStr "invalid def!"

apply_ast (MalSymbol "let*") [MalSeq _ _ params, a2] env = do
    let_env <- liftIO $ env_let env
    let_bind let_env params
    eval let_env a2
apply_ast (MalSymbol "let*") _ _ = throwStr "invalid let*"

apply_ast (MalSymbol "do") args env = foldlM (const $ eval env) Nil args

apply_ast (MalSymbol "if") [a1, a2, a3] env = do
    cond <- eval env a1
    eval env $ case cond of
        Nil              -> a3
        MalBoolean False -> a3
        _                -> a2
apply_ast (MalSymbol "if") [a1, a2] env = do
    cond <- eval env a1
    case cond of
        Nil              -> return Nil
        MalBoolean False -> return Nil
        _                -> eval env a2
apply_ast (MalSymbol "if") _ _ = throwStr "invalid if"

apply_ast (MalSymbol "fn*") [MalSeq _ _ params, ast] env = return $ MalFunction (MetaData Nil) fn where
    fn :: [MalVal] -> IOThrows MalVal
    fn args = do
        case env_apply env params args of
            Just fn_env -> eval fn_env ast
            Nothing -> do
                p <- liftIO $ _pr_list True " " params
                a <- liftIO $ _pr_list True " " args
                throwStr $ "actual parameters: " ++ a ++ " do not match signature: " ++ p
apply_ast (MalSymbol "fn*") _ _ = throwStr "invalid fn*"

apply_ast first rest env = do
    evd <- eval env first
    case evd of
        MalFunction _ f -> f =<< mapM (eval env) rest
        _               -> throwStr . (++) "invalid apply: " =<< liftIO (_pr_list True " " $ first : rest)

eval :: Env -> MalVal -> IOThrows MalVal
eval env ast = do
    traceEval <- liftIO $ env_get env "DEBUG-EVAL"
    case traceEval of
      Nothing                 -> pure ()
      Just Nil                -> pure ()
      Just (MalBoolean False) -> pure ()
      Just _                  -> liftIO $ do
            putStr "EVAL: "
            putStr =<< _pr_str True ast
            putStr "   "
            env_put env
            putStrLn ""
            hFlush stdout
    case ast of
        MalSymbol sym -> do
            maybeVal <- liftIO $ env_get env sym
            case maybeVal of
                Nothing  -> throwStr $ "'" ++ sym ++ "' not found"
                Just val -> return val
        MalSeq _ (Vect False) (a1 : as) -> apply_ast a1 as env
        MalSeq _ (Vect True)  xs        -> MalSeq (MetaData Nil) (Vect True) <$> mapM (eval env) xs
        MalHashMap _ xs                 -> MalHashMap (MetaData Nil) <$> mapM (eval env) xs
        _ -> return ast

-- print

mal_print :: MalVal -> IOThrows String
mal_print = liftIO . Printer._pr_str True

-- repl

rep :: Env -> String -> IOThrows String
rep env line = mal_print =<< eval env =<< mal_read line

repl_loop :: Env -> IO ()
repl_loop env = do
    line <- readline "user> "
    case line of
        Nothing -> return ()
        Just "" -> repl_loop env
        Just str -> do
            addHistory str
            res <- runExceptT $ rep env str
            out <- case res of
                Left mv -> (++) "Error: " <$> liftIO (Printer._pr_str True mv)
                Right val -> return val
            putStrLn out
            hFlush stdout
            repl_loop env

--  Read and evaluate a line. Ignore successful results, but crash in
--  case of error. This is intended for the startup procedure.
re :: Env -> String -> IO ()
re repl_env line = do
    res <- runExceptT $ eval repl_env =<< mal_read line
    case res of
        Left mv -> error . (++) "Startup failed: " <$> Printer._pr_str True mv
        Right _ -> return ()

defBuiltIn :: Env -> (String, Fn) -> IO ()
defBuiltIn env (sym, f) =
    env_set env sym $ MalFunction (MetaData Nil) f

main :: IO ()
main = do
    load_history

    repl_env <- env_repl

    -- core.hs: defined using Haskell
    mapM_ (defBuiltIn repl_env) Core.ns

    -- core.mal: defined using the language itself
    re repl_env "(def! not (fn* (a) (if a false true)))"

    repl_loop repl_env
