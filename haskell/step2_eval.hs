import Control.Monad (when, mapM)
import Control.Monad.Error (throwError)
import qualified Data.Map as Map
import qualified Data.Traversable as DT

import Readline (readline, load_history)
import Types
import Reader (read_str)
import Printer (_pr_str)

-- read
mal_read :: String -> IO MalVal
mal_read str = read_str str

-- eval
eval_ast :: MalVal -> (Map.Map String MalVal) -> IO MalVal
eval_ast (MalSymbol sym) env = do
    case Map.lookup sym env of
         Nothing -> error $ "'" ++ sym ++ "' not found"
         Just v  -> return v
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

apply_ast :: MalVal -> (Map.Map String MalVal) -> IO MalVal
apply_ast ast@(MalList _) env = do
    el <- eval_ast ast env
    case el of
         (MalList (Func (Fn f) : rest)) ->
            f $ rest
         el ->
            error $ "invalid apply: " ++ (show el)

eval :: MalVal -> (Map.Map String MalVal) -> IO MalVal
eval ast env = do
    case ast of
         (MalList lst) -> apply_ast ast env
         _             -> eval_ast ast env


-- print
mal_print :: MalVal -> String
mal_print exp = show exp

-- repl
add args = case args of
    [MalNumber a, MalNumber b] -> return $ MalNumber $ a + b
    _ -> error $ "illegal arguments to +"
sub args = case args of
    [MalNumber a, MalNumber b] -> return $ MalNumber $ a - b
    _ -> error $ "illegal arguments to -"
mult args = case args of
    [MalNumber a, MalNumber b] -> return $ MalNumber $ a * b
    _ -> error $ "illegal arguments to *"
divd args = case args of
    [MalNumber a, MalNumber b] -> return $ MalNumber $ a `div` b
    _ -> error $ "illegal arguments to /"

repl_env :: Map.Map String MalVal
repl_env = Map.fromList [("+", _func add),
                         ("-", _func sub),
                         ("*", _func mult),
                         ("/", _func divd)]

rep :: String -> IO String
rep line = do
    ast <- mal_read line
    exp <- eval ast repl_env
    return $ mal_print exp

repl_loop :: IO ()
repl_loop = do
    line <- readline "user> "
    case line of
        Nothing -> return ()
        Just "" -> repl_loop
        Just str -> do
            out <- catchAny (rep str) $ \e -> do
                return $ "Error: " ++ (show e)
            putStrLn out
            repl_loop

main = do
    load_history
    repl_loop
