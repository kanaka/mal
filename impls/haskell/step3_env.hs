import System.IO (hFlush, stdout)
import Control.Monad.Except (liftIO, runExceptT)

import Readline (addHistory, readline, load_history)
import Types
import Reader (read_str)
import Printer (_pr_list, _pr_str)
import Env (Env, env_get, env_let, env_put, env_repl, env_set)

--
--  Set this to True for a trace of each call to Eval.
--
traceEval :: Bool
traceEval = False

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

apply_ast first rest env = do
    evd <- eval env first
    case evd of
        MalFunction _ f -> f =<< mapM (eval env) rest
        _               -> throwStr . (++) "invalid apply: " =<< liftIO (_pr_list True " " $ first : rest)

eval :: Env -> MalVal -> IOThrows MalVal
eval env ast = do
    case traceEval of
        True -> liftIO $ do
            putStr "EVAL: "
            putStr =<< _pr_str True ast
            putStr "   "
            env_put env
            putStrLn ""
            hFlush stdout
        False -> pure ()
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

add :: Fn
add [MalNumber a, MalNumber b] = return $ MalNumber $ a + b
add _ = throwStr $ "illegal arguments to +"

sub :: Fn
sub [MalNumber a, MalNumber b] = return $ MalNumber $ a - b
sub _ = throwStr $ "illegal arguments to -"

mult :: Fn
mult [MalNumber a, MalNumber b] = return $ MalNumber $ a * b
mult _ = throwStr $ "illegal arguments to *"

divd :: Fn
divd [MalNumber a, MalNumber b] = return $ MalNumber $ a `div` b
divd _ = throwStr $ "illegal arguments to /"

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

defBuiltIn :: Env -> String -> Fn -> IO ()
defBuiltIn env sym f =
    env_set env sym $ MalFunction (MetaData Nil) f

main :: IO ()
main = do
    load_history

    repl_env <- env_repl

    defBuiltIn repl_env "+" add
    defBuiltIn repl_env "-" sub
    defBuiltIn repl_env "*" mult
    defBuiltIn repl_env "/" divd

    repl_loop repl_env
