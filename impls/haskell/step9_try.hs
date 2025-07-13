import System.Environment (getArgs)
import Control.Monad.Except (catchError, liftIO, runExceptT)
import Data.Foldable (foldlM, foldrM)

import Readline (addHistory, readline, load_history)
import Types
import Reader (read_str)
import Printer(_pr_list, _pr_str)
import Env
import Core (ns)

-- read

mal_read :: String -> IOThrows MalVal
mal_read = read_str

-- eval

qqIter :: MalVal -> MalVal -> IOThrows MalVal
qqIter (MalSeq _ (Vect False) [MalSymbol "splice-unquote", x]) acc = return $ toList [MalSymbol "concat", x, acc]
qqIter (MalSeq _ (Vect False) (MalSymbol "splice-unquote" : _)) _ = throwStr "invalid splice-unquote"
qqIter elt acc = do
    qqted <- quasiquote elt
    return $ toList [MalSymbol "cons", qqted, acc]

quasiquote :: MalVal -> IOThrows MalVal
quasiquote (MalSeq _ (Vect False) [MalSymbol "unquote", x]) = return x
quasiquote (MalSeq _ (Vect False) (MalSymbol "unquote" : _)) = throwStr "invalid unquote"
quasiquote (MalSeq _ (Vect False) ys) = foldrM qqIter (toList []) ys
quasiquote (MalSeq _ (Vect True) ys) = do
  lst <- foldrM qqIter (toList []) ys
  return $ toList [MalSymbol "vec", lst]
quasiquote ast@(MalHashMap _ _) = return $ toList [MalSymbol "quote", ast]
quasiquote ast@(MalSymbol _)    = return $ toList [MalSymbol "quote", ast]
quasiquote ast = return ast

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
    let_env <- liftIO $ env_new $ Just env
    let_bind let_env params
    eval let_env a2
apply_ast (MalSymbol "let*") _ _ = throwStr "invalid let*"

apply_ast (MalSymbol "quote") [a1] _ = return a1
apply_ast (MalSymbol "quote") _ _ = throwStr "invalid quote"

apply_ast (MalSymbol "quasiquote") [a1] env = eval env =<< quasiquote a1
apply_ast (MalSymbol "quasiquote") _ _ = throwStr "invalid quasiquote"

apply_ast (MalSymbol "defmacro!") [MalSymbol a1, a2] env = do
    func <- eval env a2
    case func of
        MalFunction _ f -> do
            let m = MalMacro f
            liftIO $ env_set env a1 m
            return m
        _ -> throwStr "defmacro! on non-function"
apply_ast (MalSymbol "defmacro!") _ _ = throwStr "invalid defmacro!"

apply_ast (MalSymbol "try*") [a1] env = eval env a1
apply_ast (MalSymbol "try*") [a1, MalSeq _ (Vect False) [MalSymbol "catch*", MalSymbol a21, a22]] env =
    catchError (eval env a1) $ \exc -> do
        try_env <- liftIO $ env_new $ Just env
        liftIO $ env_set try_env a21 exc
        eval try_env a22
apply_ast (MalSymbol "try*") _ _ = throwStr "invalid try*"

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
        fn_env <- liftIO $ env_new $ Just env
        let loop [] [] = eval fn_env ast
            loop [MalSymbol "&", k] vs = loop [k] [toList vs]
            loop (MalSymbol k : ks) (v : vs) = do
                liftIO $ env_set fn_env k v
                loop ks vs
            loop _ _ = do
                p <- liftIO $ _pr_list True " " params
                a <- liftIO $ _pr_list True " " args
                throwStr $ "actual parameters: " ++ a ++ " do not match signature: " ++ p
        loop params args
apply_ast (MalSymbol "fn*") _ _ = throwStr "invalid fn*"

apply_ast first rest env = do
    evd <- eval env first
    case evd of
        MalFunction _ f -> f =<< mapM (eval env) rest
        MalMacro m      -> eval env =<< m rest
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

mal_print :: MalVal -> IO String
mal_print = _pr_str True

-- repl

repl_loop :: Env -> IO ()
repl_loop env = do
    line <- readline "user> "
    case line of
        Nothing -> return ()
        Just "" -> repl_loop env
        Just str -> do
            addHistory str
            res <- runExceptT $ eval env =<< mal_read str
            out <- case res of
                Left mv -> (++) "Error: " <$> mal_print mv
                Right val -> mal_print val
            putStrLn out
            repl_loop env

--  Read and evaluate a line.  Ignore successful results, else print
--  an error message case of error.
--  The error function seems appropriate, but has no effect.
re :: Env -> String -> IO ()
re repl_env line = do
    res <- runExceptT $ eval repl_env =<< mal_read line
    case res of
        Left mv -> putStrLn . (++) "Startup failed: " =<< _pr_str True mv
        Right _ -> return ()

defBuiltIn :: Env -> (String, Fn) -> IO ()
defBuiltIn env (sym, f) =
    env_set env sym $ MalFunction (MetaData Nil) f

evalFn :: Env -> Fn
evalFn env [ast] = eval env ast
evalFn _ _ = throwStr "illegal call of eval"

main :: IO ()
main = do
    args <- getArgs

    repl_env <- env_new Nothing

    -- core.hs: defined using Haskell
    mapM_ (defBuiltIn repl_env) Core.ns
    defBuiltIn repl_env ("eval", evalFn repl_env)

    -- core.mal: defined using the language itself
    re repl_env "(def! not (fn* (a) (if a false true)))"
    re repl_env "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))"
    re repl_env "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))"

    case args of
        script : scriptArgs -> do
            env_set repl_env "*ARGV*" $ toList $ MalString <$> scriptArgs
            re repl_env $ "(load-file \"" ++ script ++ "\")"
        [] -> do
            env_set repl_env "*ARGV*" $ toList []

            load_history
            repl_loop repl_env
