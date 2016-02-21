import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Control.Monad (mapM)
import Control.Monad.Error (runErrorT)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map
import qualified Data.Traversable as DT

import Readline (readline, load_history)
import Types
import Reader (read_str)
import Printer (_pr_str)
import Env (Env, env_new, env_bind, env_find, env_get, env_set)
import Core as Core

-- read
mal_read :: String -> IOThrows MalVal
mal_read str = read_str str

-- eval
is_pair (MalList x _:xs) = True
is_pair (MalVector x _:xs) = True
is_pair _ = False

quasiquote :: MalVal -> MalVal
quasiquote ast =
    case ast of
         (MalList (MalSymbol "unquote" : a1 : []) _) -> a1
         (MalList (MalList (MalSymbol "splice-unquote" : a01 : []) _ : rest) _) ->
            MalList [(MalSymbol "concat"), a01, quasiquote (MalList rest Nil)] Nil
         (MalVector (MalList (MalSymbol "splice-unquote" : a01 : []) _ : rest) _) ->
            MalList [(MalSymbol "concat"), a01, quasiquote (MalVector rest Nil)] Nil
         (MalList (a0 : rest) _) -> MalList [(MalSymbol "cons"),
                                             quasiquote a0,
                                             quasiquote (MalList rest Nil)] Nil
         (MalVector (a0 : rest) _) -> MalList [(MalSymbol "cons"),
                                               quasiquote a0,
                                               quasiquote (MalVector rest Nil)] Nil
         _ -> MalList [(MalSymbol "quote"), ast] Nil

is_macro_call :: MalVal -> Env -> IOThrows Bool
is_macro_call (MalList (a0@(MalSymbol _) : rest) _) env = do
    e <- liftIO $ env_find env a0
    case e of
         Just e -> do
            f <- env_get e a0
            case f of
                 MalFunc {macro=True} -> return True
                 _                    -> return False
         Nothing -> return False
is_macro_call _ _ = return False

macroexpand :: MalVal -> Env -> IOThrows MalVal
macroexpand ast@(MalList (a0 : args) _) env = do
    mc <- is_macro_call ast env
    if mc then do
        mac <- env_get env a0
        case mac of 
             MalFunc {fn=(Fn f)} -> do
                new_ast <- f args
                macroexpand new_ast env
             _ ->
                return ast
    else
        return ast
macroexpand ast _ = return ast

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
apply_ast ast@(MalList (MalSymbol "quote" : args) _) env = do
    case args of
         a1 : [] -> return a1
         _ -> throwStr "invalid quote"
apply_ast ast@(MalList (MalSymbol "quasiquote" : args) _) env = do
    case args of
         a1 : [] -> eval (quasiquote a1) env
         _ -> throwStr "invalid quasiquote"

apply_ast ast@(MalList (MalSymbol "defmacro!" : args) _) env = do
    case args of
         (a1 : a2 : []) -> do
            func <- eval a2 env
            case func of
                MalFunc {fn=f, ast=a, env=e, params=p} -> do
                    let new_func = MalFunc {fn=f, ast=a, env=e,
                                            params=p, macro=True,
                                            meta=Nil} in
                        liftIO $ env_set env a1 new_func
                _ -> throwStr "defmacro! on non-function"
         _ -> throwStr "invalid defmacro!" 
apply_ast ast@(MalList (MalSymbol "macroexpand" : args) _) env = do
    case args of
         (a1 : []) -> macroexpand a1 env
         _ -> throwStr "invalid macroexpand" 
apply_ast ast@(MalList (MalSymbol "try*" : args) _) env = do
    case args of
         (a1 : []) -> eval a1 env
         (a1 : (MalList ((MalSymbol "catch*") : a21 : a22 : []) _) : []) -> do
            res <- liftIO $ runErrorT $ eval a1 env
            case res of
                Right val -> return val
                Left err -> do
                    exc <- case err of
                        (StringError str) -> return $ MalString str
                        (MalValError mv) -> return $ mv
                    try_env <- liftIO $ env_new $ Just env
                    liftIO $ env_set try_env a21 exc
                    eval a22 try_env
         _ -> throwStr "invalid try*"
apply_ast ast@(MalList (MalSymbol "do" : args) _) env = do
    case args of
         ([]) -> return Nil
         _  -> do
            el <- eval_ast (MalList args Nil) env
            case el of
                 (MalList lst _) -> return $ last lst
            
apply_ast ast@(MalList (MalSymbol "if" : args) _) env = do
    case args of
         (a1 : a2 : a3 : []) -> do
            cond <- eval a1 env
            if cond == MalFalse || cond == Nil
                then eval a3 env
                else eval a2 env
         (a1 : a2 : []) -> do
            cond <- eval a1 env
            if cond == MalFalse || cond == Nil
                then return Nil
                else eval a2 env
         _ -> throwStr "invalid if"
apply_ast ast@(MalList (MalSymbol "fn*" : args) _) env = do
    case args of
         (a1 : a2 : []) -> do
            params <- (_to_list a1)
            return $ (_malfunc a2 env (MalList params Nil)
                      (\args -> do
                        fn_env1 <- liftIO $ env_new $ Just env
                        fn_env2 <- liftIO $ env_bind fn_env1 params args
                        eval a2 fn_env2))
         _ -> throwStr "invalid fn*"
apply_ast ast@(MalList _ _) env = do
    mc <- is_macro_call ast env
    if mc then do
        new_ast <- macroexpand ast env
        eval new_ast env
    else
        case ast of
            MalList _ _ -> do
                el <- eval_ast ast env
                case el of
                    (MalList ((Func (Fn f) _) : rest) _) ->
                        f $ rest
                    (MalList ((MalFunc {ast=ast,
                                        env=fn_env,
                                        params=(MalList params Nil)} : rest)) _) -> do
                        fn_env1 <- liftIO $ env_new $ Just fn_env
                        fn_env2 <- liftIO $ env_bind fn_env1 params rest
                        eval ast fn_env2
                    el ->
                        throwStr $ "invalid apply: " ++ (show el)
            _ -> return ast

eval :: MalVal -> Env -> IOThrows MalVal
eval ast env = do
    case ast of
         (MalList _ _) -> apply_ast ast env
         _             -> eval_ast ast env


-- print
mal_print :: MalVal -> String
mal_print exp = show exp

-- repl

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
    args <- getArgs
    load_history

    repl_env <- env_new Nothing

    -- core.hs: defined using Haskell
    (mapM (\(k,v) -> (env_set repl_env (MalSymbol k) v)) Core.ns)
    env_set repl_env (MalSymbol "eval") (_func (\[ast] -> eval ast repl_env))
    env_set repl_env (MalSymbol "*ARGV*") (MalList [] Nil)

    -- core.mal: defined using the language itself
    runErrorT $ rep repl_env "(def! *host-language* \"haskell\")"
    runErrorT $ rep repl_env "(def! not (fn* (a) (if a false true)))"
    runErrorT $ rep repl_env "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))"
    runErrorT $ rep repl_env "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))"
    runErrorT $ rep repl_env "(def! *gensym-counter* (atom 0))"
    runErrorT $ rep repl_env "(def! gensym (fn* [] (symbol (str \"G__\" (swap! *gensym-counter* (fn* [x] (+ 1 x)))))))"
    runErrorT $ rep repl_env "(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) (let* (condvar (gensym)) `(let* (~condvar ~(first xs)) (if ~condvar ~condvar (or ~@(rest xs)))))))))"

    if length args > 0 then do
        env_set repl_env (MalSymbol "*ARGV*") (MalList (map MalString (drop 1 args)) Nil)
        runErrorT $ rep repl_env $ "(load-file \"" ++ (args !! 0) ++ "\")" 
        return ()
    else do
        runErrorT $ rep repl_env "(println (str \"Mal [\" *host-language* \"]\"))"
        repl_loop repl_env
