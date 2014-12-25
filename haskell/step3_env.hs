import System.IO (hFlush, stdout)
import Control.Monad (mapM)
import Control.Monad.Error (runErrorT)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map
import qualified Data.Traversable as DT

import Readline (readline, load_history)
import Types
import Reader (read_str)
import Printer (_pr_str)
import Env (Env, env_new, env_get, env_set)

-- read
mal_read :: String -> IOThrows MalVal
mal_read str = read_str str

-- eval
eval_ast :: MalVal -> Env -> IOThrows MalVal
eval_ast sym@(MalSymbol _) env = env_get env sym
eval_ast ast@(MalList lst m) env = do
    new_lst <- mapM (\x -> (eval x env)) lst
    return $ MalList new_lst m
eval_ast ast@(MalVector lst m) env = do
    new_lst <- mapM (\x -> (eval x env)) lst
    return $ MalVector new_lst m
eval_ast ast@(MalHashMap lst m) env = do
    new_hm <- DT.mapM (\x -> (eval x env)) lst
    return $ MalHashMap new_hm m
eval_ast ast env = return ast

let_bind :: Env -> [MalVal] -> IOThrows Env
let_bind env [] = return env
let_bind env (b:e:xs) = do
    evaled <- eval e env
    x <- liftIO $ env_set env b evaled
    let_bind env xs

apply_ast :: MalVal -> Env -> IOThrows MalVal
apply_ast ast@(MalList (MalSymbol "def!" : args) _) env = do
    case args of
         (a1@(MalSymbol _): a2 : []) -> do
            evaled <- eval a2 env
            liftIO $ env_set env a1 evaled
         _ -> throwStr "invalid def!"
apply_ast ast@(MalList (MalSymbol "let*" : args) _) env = do
    case args of
         (a1 : a2 : []) -> do
            params <- (_to_list a1)
            let_env <- liftIO $ env_new $ Just env
            let_bind let_env params
            eval a2 let_env
         _ -> throwStr "invalid let*"
apply_ast ast@(MalList _ _) env = do
    el <- eval_ast ast env
    case el of
         (MalList ((Func (Fn f) _) : rest) _) ->
            f $ rest
         el ->
            throwStr $ "invalid apply: " ++ (show el)

eval :: MalVal -> Env -> IOThrows MalVal
eval ast env = do
    case ast of
         (MalList _ _) -> apply_ast ast env
         _             -> eval_ast ast env


-- print
mal_print :: MalVal -> String
mal_print exp = show exp

-- repl
add [MalNumber a, MalNumber b] = return $ MalNumber $ a + b
add _ = throwStr $ "illegal arguments to +"
sub [MalNumber a, MalNumber b] = return $ MalNumber $ a - b
sub _ = throwStr $ "illegal arguments to -"
mult [MalNumber a, MalNumber b] = return $ MalNumber $ a * b
mult _ = throwStr $ "illegal arguments to *"
divd [MalNumber a, MalNumber b] = return $ MalNumber $ a `div` b
divd _ = throwStr $ "illegal arguments to /"

rep :: Env -> String -> IOThrows String
rep env line = do
    ast <- mal_read line
    exp <- eval ast env
    return $ mal_print exp

repl_loop :: Env -> IO ()
repl_loop env = do
    line <- readline "user> "
    case line of
        Nothing -> return ()
        Just "" -> repl_loop env
        Just str -> do
            res <- runErrorT $ rep env str
            out <- case res of
                Left (StringError str) -> return $ "Error: " ++ str
                Left (MalValError mv) -> return $ "Error: " ++ (show mv)
                Right val -> return val
            putStrLn out
            hFlush stdout
            repl_loop env

main = do
    load_history

    repl_env <- env_new Nothing
    env_set repl_env (MalSymbol "+") $ _func add
    env_set repl_env (MalSymbol "-") $ _func sub
    env_set repl_env (MalSymbol "*") $ _func mult
    env_set repl_env (MalSymbol "/") $ _func divd
    repl_loop repl_env
