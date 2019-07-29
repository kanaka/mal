import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Control.Monad ((<=<))
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans (liftIO)
import Data.Foldable (foldlM)

import Readline (addHistory, readline, load_history)
import Types
import Reader (read_str)
import Printer (_pr_str)
import Env (env_new, env_bind, env_get, env_set)
import Core (ns)

-- read

mal_read :: String -> IOThrows MalVal
mal_read = read_str

-- eval

-- eval_ast is replaced with pattern matching.

let_bind :: Env -> [MalVal] -> IOThrows ()
let_bind _ [] = return ()
let_bind env (MalSymbol b : e : xs) = do
    liftIO . env_set env b =<< eval env e
    let_bind env xs
let_bind _ _ = throwStr "invalid let*"

unWrapSymbol :: MalVal -> IOThrows String
unWrapSymbol (MalSymbol s) = return s
unWrapSymbol _ = throwStr "fn* parameter must be symbols"

newFunction :: MalVal -> Env -> [String] -> MalVal
newFunction a env p = MalFunction {f_ast=a, f_params=p, macro=False, meta=Nil,
    fn=(\args -> do
        fn_env <- liftIO $ env_new env
        ok <- liftIO $ env_bind fn_env p args
        case ok of
            True  -> eval fn_env a
            False -> throwStr $ "actual parameters do not match signature " ++ show p)}

apply_ast :: [MalVal] -> Env -> IOThrows MalVal

apply_ast [] _ = return $ toList []

apply_ast [MalSymbol "def!", MalSymbol a1, a2] env = do
    evd <- eval env a2
    liftIO $ env_set env a1 evd
    return evd
apply_ast (MalSymbol "def!" : _) _ = throwStr "invalid def!"

apply_ast [MalSymbol "let*", MalSeq _ _ params, a2] env = do
    let_env <- liftIO $ env_new env
    let_bind let_env params
    eval let_env a2
apply_ast (MalSymbol "let*" : _) _ = throwStr "invalid let*"

apply_ast (MalSymbol "do" : args) env = foldlM (const $ eval env) Nil args

apply_ast [MalSymbol "if", a1, a2, a3] env = do
    cond <- eval env a1
    eval env $ case cond of
        Nil              -> a3
        MalBoolean False -> a3
        _                -> a2
apply_ast [MalSymbol "if", a1, a2] env = do
    cond <- eval env a1
    case cond of
        Nil              -> return Nil
        MalBoolean False -> return Nil
        _                -> eval env a2
apply_ast (MalSymbol "if" : _) _ = throwStr "invalid if"

apply_ast [MalSymbol "fn*", MalSeq _ _ params, ast] env = newFunction ast env <$> mapM unWrapSymbol params
apply_ast (MalSymbol "fn*" : _) _ = throwStr "invalid fn*"

apply_ast ast env = do
    evd <- mapM (eval env) ast
    case evd of
        MalFunction {fn=f} : args -> f args
        _ -> throwStr . (++) "invalid apply: " =<< liftIO (Printer._pr_str True (toList ast))

eval :: Env -> MalVal -> IOThrows MalVal
eval env (MalSymbol sym)            = do
    maybeVal <- liftIO $ env_get env sym
    case maybeVal of
        Nothing  -> throwStr $ "'" ++ sym ++ "' not found"
        Just val -> return val
eval env (MalSeq _ (Vect False) xs) = apply_ast xs env
eval env (MalSeq m (Vect True)  xs) = MalSeq m (Vect True) <$> mapM (eval env) xs
eval env (MalHashMap m xs)          = MalHashMap m         <$> mapM (eval env) xs
eval _   ast                        = return ast

-- print

mal_print :: MalVal -> IOThrows String
mal_print = liftIO. Printer._pr_str True

-- repl

rep :: Env -> String -> IOThrows String
rep env = mal_print <=< eval env <=< mal_read

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
    env_set env sym $ MalFunction {fn=f, f_ast=Nil, f_params=[], macro=False, meta=Nil}

evalFn :: Env -> Fn
evalFn env [ast] = eval env ast
evalFn _ _ = throwStr "illegal call of eval"

main :: IO ()
main = do
    args <- getArgs
    load_history

    repl_env <- env_new []

    -- core.hs: defined using Haskell
    mapM_ (defBuiltIn repl_env) Core.ns
    defBuiltIn repl_env ("eval", evalFn repl_env)

    -- core.mal: defined using the language itself
    re repl_env "(def! not (fn* (a) (if a false true)))"
    re repl_env "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))"

    case args of
        script : scriptArgs -> do
            env_set repl_env "*ARGV*" $ toList $ MalString <$> scriptArgs
            re repl_env $ "(load-file \"" ++ script ++ "\")"
        [] -> do
            env_set repl_env "*ARGV*" $ toList []
            repl_loop repl_env
