import System.IO (hFlush, stdout)
import Control.Monad (mapM)
import Control.Monad.Error (runErrorT)
import qualified Data.Map as Map
import qualified Data.Traversable as DT

import Readline (readline, load_history)
import Types
import Reader (read_str)
import Printer (_pr_str)

-- read
mal_read :: String -> IOThrows MalVal
mal_read str = read_str str

-- eval
eval_ast :: MalVal -> (Map.Map String MalVal) -> IOThrows MalVal
eval_ast (MalSymbol sym) env = do
    case Map.lookup sym env of
         Nothing -> throwStr $ "'" ++ sym ++ "' not found"
         Just v  -> return v
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

apply_ast :: MalVal -> (Map.Map String MalVal) -> IOThrows MalVal
apply_ast ast@(MalList _ _) env = do
    el <- eval_ast ast env
    case el of
         (MalList ((Func (Fn f) _) : rest) _) ->
            f $ rest
         el ->
            throwStr $ "invalid apply: " ++ (show el)

eval :: MalVal -> (Map.Map String MalVal) -> IOThrows MalVal
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

repl_env :: Map.Map String MalVal
repl_env = Map.fromList [("+", _func add),
                         ("-", _func sub),
                         ("*", _func mult),
                         ("/", _func divd)]

rep :: String -> IOThrows String
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
            res <- runErrorT $ rep str
            out <- case res of
                Left (StringError str) -> return $ "Error: " ++ str
                Left (MalValError mv) -> return $ "Error: " ++ (show mv)
                Right val -> return val
            putStrLn out
            hFlush stdout
            repl_loop

main = do
    load_history
    repl_loop
