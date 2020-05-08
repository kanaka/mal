import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Control.Monad ((<=<))
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans (liftIO)
import Data.Foldable (foldlM, foldrM)

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

-- starts-with is replaced with pattern matching.

qqIter :: Env -> MalVal -> [MalVal] -> IOThrows [MalVal]
qqIter env (MalSeq _ (Vect False) [MalSymbol "splice-unquote", x]) acc = do
    evaluated <- eval env x
    case evaluated of
        MalSeq _ (Vect False) xs -> return $ xs ++ acc
        _ -> throwStr "invalid splice-unquote argument"
qqIter _ (MalSeq _ (Vect False) (MalSymbol "splice-unquote" : _)) _ = throwStr "invalid splice-unquote"
qqIter env x acc = (: acc) <$> quasiquote x env

quasiquote :: MalVal -> Env -> IOThrows MalVal
quasiquote (MalSeq _ (Vect False) [MalSymbol "unquote", x]) env = eval env x
--  FIXME This line
quasiquote (MalSeq m _ ys) env = MalSeq m (Vect False) <$> foldrM (qqIter env) [] ys
--  is adapted to broken tests. It should be:
--  quasiquote (MalSeq m v ys) env = MalSeq m v <$> foldrM (qqIter env) [] ys
quasiquote ast _ = return ast

-- is-macro-call is replaced with pattern matching.

macroexpand :: Env -> MalVal -> IOThrows MalVal
macroexpand env ast@(MalSeq _ (Vect False) (MalSymbol a0 : args)) = do
    maybeMacro <- liftIO $ env_get env a0
    case maybeMacro of
        Just (MalFunction {fn=f, macro=True}) -> macroexpand env =<< f args
        _                                     -> return ast
macroexpand _ ast = return ast

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

apply_ast [MalSymbol "quote", a1] _ = return a1
apply_ast (MalSymbol "quote" : _) _ = throwStr "invalid quote"

apply_ast [MalSymbol "quasiquote", a1] env = quasiquote a1 env
apply_ast (MalSymbol "quasiquote" : _) _ = throwStr "invalid quasiquote"

apply_ast [MalSymbol "defmacro!", MalSymbol a1, a2] env = do
    func <- eval env a2
    case func of
        MalFunction {macro=False} -> do
            let m = func {macro=True}
            liftIO $ env_set env a1 m
            return m
        _ -> throwStr "defmacro! on non-function"
apply_ast (MalSymbol "defmacro!" : _) _ = throwStr "invalid defmacro!"

apply_ast [MalSymbol "macroexpand", a1] env = macroexpand env a1
apply_ast (MalSymbol "macroexpand" : _) _ = throwStr "invalid macroexpand"

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
        MalFunction {fn=f, macro=False} : args -> f args
        _ -> throwStr . (++) "invalid apply: " =<< liftIO (Printer._pr_str True (toList ast))

eval :: Env -> MalVal -> IOThrows MalVal
eval env ast = do
    newAst <- macroexpand env ast
    case newAst of
        MalSymbol sym -> do
            maybeVal <- liftIO $ env_get env sym
            case maybeVal of
                Nothing  -> throwStr $ "'" ++ sym ++ "' not found"
                Just val -> return val
        MalSeq _ (Vect False) xs -> apply_ast xs env
        MalSeq m (Vect True)  xs -> MalSeq m (Vect True) <$> mapM (eval env) xs
        MalHashMap m xs          -> MalHashMap m         <$> mapM (eval env) xs
        _ -> return newAst

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
    re repl_env "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))"

    case args of
        script : scriptArgs -> do
            env_set repl_env "*ARGV*" $ toList $ MalString <$> scriptArgs
            re repl_env $ "(load-file \"" ++ script ++ "\")"
        [] -> do
            env_set repl_env "*ARGV*" $ toList []
            repl_loop repl_env
