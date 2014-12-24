import System.Environment (getArgs)
import Control.Monad (when, mapM)
import Control.Monad.Error (throwError)
import qualified Data.Map as Map
import qualified Data.Traversable as DT

import Readline (readline, load_history)
import Types
import Reader (read_str)
import Printer (_pr_str)
import Env (Env, env_new, env_bind, env_find, env_get, env_set)
import Core as Core

-- read
mal_read :: String -> IO MalVal
mal_read str = read_str str

-- eval
is_pair (MalList x:xs) = True
is_pair (MalVector x:xs) = True
is_pair _ = False

quasiquote :: MalVal -> MalVal
quasiquote ast =
    case ast of
         (MalList (MalSymbol "unquote" : a1 : [])) -> a1
         (MalList (MalList (MalSymbol "splice-unquote" : a01 : []) : rest)) ->
            MalList [(MalSymbol "concat"), a01, quasiquote (MalList rest)]
         (MalVector (MalList (MalSymbol "splice-unquote" : a01 : []) : rest)) ->
            MalList [(MalSymbol "concat"), a01, quasiquote (MalVector rest)]
         (MalList (a0 : rest)) -> MalList [(MalSymbol "cons"),
                                           quasiquote a0,
                                           quasiquote (MalList rest)]
         (MalVector (a0 : rest)) -> MalList [(MalSymbol "cons"),
                                             quasiquote a0,
                                             quasiquote (MalVector rest)]
         _ -> MalList [(MalSymbol "quote"), ast]

is_macro_call :: MalVal -> Env -> IO Bool
is_macro_call (MalList (a0@(MalSymbol _) : rest)) env = do
    e <- env_find env a0
    case e of
         Just e -> do
            f <- env_get e a0
            case f of
                 MalFunc {macro=True} -> return True
                 _                    -> return False
         Nothing -> return False
is_macro_call _ _ = return False

macroexpand :: MalVal -> Env -> IO MalVal
macroexpand ast@(MalList (a0 : args)) env = do
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


eval_ast :: MalVal -> Env -> IO MalVal
eval_ast sym@(MalSymbol _) env = env_get env sym
eval_ast ast@(MalList lst) env = do
    new_lst <- mapM (\x -> (eval x env)) lst
    return $ MalList new_lst
eval_ast ast@(MalVector lst) env = do
    new_lst <- mapM (\x -> (eval x env)) lst
    return $ MalVector new_lst
eval_ast ast@(MalHashMap lst) env = do
    new_hm <- DT.mapM (\x -> (eval x env)) lst
    return $ MalHashMap new_hm
eval_ast ast env = return ast

let_bind :: Env -> [MalVal] -> IO Env
let_bind env [] = return env
let_bind env (b:e:xs) = do
    evaled <- eval e env
    x <- env_set env b evaled
    let_bind env xs

apply_ast :: MalVal -> Env -> IO MalVal
apply_ast ast@(MalList (MalSymbol "def!" : args)) env = do
    case args of
         (a1@(MalSymbol _): a2 : []) -> do
            evaled <- eval a2 env
            env_set env a1 evaled
         _ -> error $ "invalid def!"
apply_ast ast@(MalList (MalSymbol "let*" : args)) env = do
    case args of
         (MalList a1 : a2 : []) -> do
            let_env <- env_new $ Just env
            let_bind let_env a1
            eval a2 let_env
         (MalVector a1 : a2 : []) -> do
            let_env <- env_new $ Just env
            let_bind let_env a1
            eval a2 let_env
         _ -> error $ "invalid let*"
apply_ast ast@(MalList (MalSymbol "quote" : args)) env = do
    case args of
         a1 : [] -> return a1
         _ -> error $ "invalid quote"
apply_ast ast@(MalList (MalSymbol "quasiquote" : args)) env = do
    case args of
         a1 : [] -> eval (quasiquote a1) env
         _ -> error $ "invalid quasiquote"

apply_ast ast@(MalList (MalSymbol "defmacro!" : args)) env = do
    case args of
         (a1 : a2 : []) -> do
            func <- eval a2 env
            case func of
                MalFunc {fn=f, ast=a, env=e, params=p} -> do
                    let new_func = MalFunc {fn=f, ast=a, env=e,
                                            params=p, macro=True} in
                        env_set env a1 new_func
                _ -> error $ "defmacro! on non-function"
         _ -> error $ "invalid defmacro!" 
apply_ast ast@(MalList (MalSymbol "macroexpand" : args)) env = do
    case args of
         (a1 : []) -> macroexpand a1 env
         _ -> error $ "invalid macroexpand" 
apply_ast ast@(MalList (MalSymbol "do" : args)) env = do
    case args of
         ([]) -> return Nil
         _  -> do
            el <- eval_ast (MalList args) env
            case el of
                 (MalList el) -> return $ last el
            
apply_ast ast@(MalList (MalSymbol "if" : args)) env = do
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
         _ -> error $ "invalid if"
apply_ast ast@(MalList (MalSymbol "fn*" : args)) env = do
    let params = case args of
                     ((MalList lst) : _)   -> lst
                     ((MalVector lst) : _) -> lst in
        case args of
             (a1 : a2 : []) -> do
                return $ (_malfunc a2 env a1 (\args -> do
                    fn_env1 <- env_new $ Just env
                    fn_env2 <- (env_bind fn_env1 params args)
                    eval a2 fn_env2))
             _ -> error $ "invalid fn*"
apply_ast ast@(MalList _) env = do
    mc <- is_macro_call ast env
    if mc then do
        new_ast <- macroexpand ast env
        eval new_ast env
    else
        case ast of
            MalList _ -> do
                el <- eval_ast ast env
                case el of
                    (MalList (Func (Fn f) : rest)) ->
                        f $ rest
                    (MalList (MalFunc {ast=ast,
                                    env=fn_env,
                                    params=(MalList params)} : rest)) -> do
                        fn_env1 <- env_new $ Just fn_env
                        fn_env2 <- (env_bind fn_env1 params rest)
                        eval ast fn_env2
                    el ->
                        error $ "invalid apply: " ++ (show el)
            _ -> return ast

eval :: MalVal -> Env -> IO MalVal
eval ast env = do
    case ast of
         (MalList lst) -> apply_ast ast env
         _             -> eval_ast ast env


-- print
mal_print :: MalVal -> String
mal_print exp = show exp

-- repl

rep :: Env -> String -> IO String
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
            out <- catchAny (rep env str) $ \e -> do
                return $ "Error: " ++ (show e)
            putStrLn out
            repl_loop env

main = do
    args <- getArgs
    load_history

    repl_env <- env_new Nothing

    -- core.hs: defined using Haskell
    (mapM (\(k,v) -> (env_set repl_env (MalSymbol k) v)) Core.ns)
    env_set repl_env (MalSymbol "eval") (_func (\[ast] -> eval ast repl_env))
    env_set repl_env (MalSymbol "*ARGV*") (MalList [])

    -- core.mal: defined using the language itself
    rep repl_env "(def! not (fn* (a) (if a false true)))"
    rep repl_env "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))"
    rep repl_env "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))"
    rep repl_env "(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))"

    if length args > 0 then do
        env_set repl_env (MalSymbol "*ARGV*") (MalList (map MalString (drop 1 args)))
        rep repl_env $ "(load-file \"" ++ (args !! 0) ++ "\")" 
        return ()
    else 
        repl_loop repl_env
