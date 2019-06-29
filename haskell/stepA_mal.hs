import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Control.Monad (mapM)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans (liftIO)
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
is_macro_call (MalList ((MalSymbol a0) : _) _) env = do
    maybeE <- liftIO $ env_find env a0
    case maybeE of
         Just e -> do
            f <- env_get e a0
            case f of
                 MalFunc {macro=True} -> return True
                 _                    -> return False
         Nothing -> return False
is_macro_call _ _ = return False

macroexpand :: MalVal -> Env -> IOThrows MalVal
macroexpand ast@(MalList (MalSymbol a0 : args) _) env = do
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
eval_ast (MalSymbol s) env = env_get env s
eval_ast (MalList lst m) env = do
    new_lst <- mapM (\x -> (eval x env)) lst
    return $ MalList new_lst m
eval_ast (MalVector lst m) env = do
    new_lst <- mapM (\x -> (eval x env)) lst
    return $ MalVector new_lst m
eval_ast (MalHashMap lst m) env = do
    new_hm <- DT.mapM (\x -> (eval x env)) lst
    return $ MalHashMap new_hm m
eval_ast ast _ = return ast

let_bind :: Env -> [MalVal] -> IOThrows ()
let_bind _ [] = return ()
let_bind env (MalSymbol b : e : xs) = do
    evaled <- eval e env
    liftIO $ env_set env b evaled
    let_bind env xs
let_bind _ _ = throwStr "invalid let*"

unwrapSymbol :: MalVal -> IOThrows String
unwrapSymbol (MalSymbol s) = return s
unwrapSymbol _ = throwStr "fn* expects a sequence of symbols"

apply_ast :: MalVal -> Env -> IOThrows MalVal
apply_ast ast@(MalList [] _) _ = do
    return ast
apply_ast (MalList (MalSymbol "def!" : args) _) env = do
    case args of
         [MalSymbol a1, a2] -> do
            evaled <- eval a2 env
            liftIO $ env_set env a1 evaled
            return evaled
         _ -> throwStr "invalid def!"
apply_ast (MalList (MalSymbol "let*" : args) _) env = do
    case args of
         (a1 : a2 : []) -> do
            params <- _to_list a1
            let_env <- liftIO $ env_new $ Just env
            let_bind let_env params
            eval a2 let_env
         _ -> throwStr "invalid let*"
apply_ast (MalList (MalSymbol "quote" : args) _) _ = do
    case args of
         a1 : [] -> return a1
         _ -> throwStr "invalid quote"
apply_ast (MalList (MalSymbol "quasiquote" : args) _) env = do
    case args of
         a1 : [] -> eval (quasiquote a1) env
         _ -> throwStr "invalid quasiquote"

apply_ast (MalList (MalSymbol "defmacro!" : args) _) env =
    case args of
         (MalSymbol a1 : a2 : []) -> do
            func <- eval a2 env
            case func of
                MalFunc {} -> do
                    let new_func = func {macro=True, meta=Nil}
                    liftIO $ env_set env a1 new_func
                    return new_func
                _ -> throwStr "defmacro! on non-function"
         _ -> throwStr "invalid defmacro!"
apply_ast (MalList (MalSymbol "macroexpand" : args) _) env = do
    case args of
         (a1 : []) -> macroexpand a1 env
         _ -> throwStr "invalid macroexpand"
apply_ast (MalList (MalSymbol "try*" : args) _) env = do
    case args of
         (a1 : []) -> eval a1 env
         [a1, MalList [MalSymbol "catch*", MalSymbol a21, a22] _] -> do
            res <- liftIO $ runExceptT $ eval a1 env
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
apply_ast (MalList (MalSymbol "do" : args) _) env = do
    case args of
         ([]) -> return Nil
         _  -> do
            el <- eval_ast (MalList args Nil) env
            case el of
                 (MalList lst _) -> return $ last lst
                 _               -> throwStr "invalid do"

apply_ast (MalList (MalSymbol "if" : args) _) env = do
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
apply_ast (MalList (MalSymbol "fn*" : args) _) env = do
    case args of
         (a1 : a2 : []) -> do
            params <- (_to_list a1)
            symbols <- mapM unwrapSymbol params
            let f xs = do
                fn_env <- liftIO $ env_new $ Just env
                liftIO $ env_bind fn_env symbols xs
                eval a2 fn_env
            return $ MalFunc {f_ast=a2, f_params=symbols, meta=Nil, macro=False, fn=Fn f}
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
                    (MalList (MalFunc {fn=Fn f} : rest) _) ->
                        f rest
                    _ ->
                        throwStr $ "invalid apply: " ++ Printer._pr_str True el
            _ -> return ast
apply_ast _ _ = throwStr "internal error in apply_ast"


eval :: MalVal -> Env -> IOThrows MalVal
eval ast env = do
    case ast of
         (MalList _ _) -> apply_ast ast env
         _             -> eval_ast ast env


-- print
mal_print :: MalVal -> String
mal_print = Printer._pr_str True

-- repl

rep :: Env -> String -> IOThrows String
rep env line = do
    ast <- mal_read line
    e <- eval ast env
    return $ mal_print e

repl_loop :: Env -> IO ()
repl_loop env = do
    line <- readline "user> "
    case line of
        Nothing -> return ()
        Just "" -> repl_loop env
        Just str -> do
            res <- runExceptT $ rep env str
            out <- case res of
                Left (StringError s) -> return $ "Error: " ++ s
                Left (MalValError mv)  -> return $ "Error: " ++ _pr_str True mv
                Right val -> return val
            putStrLn out
            hFlush stdout
            repl_loop env

evalBuiltIn :: Env -> [MalVal] -> IOThrows MalVal
evalBuiltIn repl_env [ast] = eval ast repl_env
evalBuiltIn _ _ = throwStr "invalid eval"

main :: IO ()
main = do
    args <- getArgs
    load_history

    repl_env <- env_new Nothing

    -- core.hs: defined using Haskell
    mapM_ (uncurry $ env_set repl_env) Core.ns
    env_set repl_env "eval" (_func (evalBuiltIn repl_env))
    env_set repl_env "*ARGV*" (MalList [] Nil)

    -- core.mal: defined using the language itself
    _ <- runExceptT $ rep repl_env "(def! *host-language* \"haskell\")"
    _ <- runExceptT $ rep repl_env "(def! not (fn* (a) (if a false true)))"
    _ <- runExceptT $ rep repl_env "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))"
    _ <- runExceptT $ rep repl_env "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))"
    _ <- runExceptT $ rep repl_env "(def! inc (fn* [x] (+ x 1)))"
    _ <- runExceptT $ rep repl_env "(def! gensym (let* [counter (atom 0)] (fn* [] (symbol (str \"G__\" (swap! counter inc))))))"
    _ <- runExceptT $ rep repl_env "(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) (let* (condvar (gensym)) `(let* (~condvar ~(first xs)) (if ~condvar ~condvar (or ~@(rest xs)))))))))"

    if length args > 0 then do
        env_set repl_env "*ARGV*" (MalList (map MalString (drop 1 args)) Nil)
        _ <- runExceptT $ rep repl_env $ "(load-file \"" ++ (args !! 0) ++ "\")"
        return ()
    else do
        _ <- runExceptT $ rep repl_env "(println (str \"Mal [\" *host-language* \"]\"))"
        repl_loop repl_env
