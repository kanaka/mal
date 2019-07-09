import System.IO (hFlush, stdout)
import Control.Monad ((<=<))
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map

import Readline (addHistory, readline, load_history)
import Types
import Reader (read_str)
import Printer (_pr_str)

-- read

mal_read :: String -> IOThrows MalVal
mal_read = read_str

-- eval

-- eval_ast is replaced with pattern matching.

apply_ast :: [MalVal] -> IOThrows MalVal

apply_ast [] = return $ toList []

apply_ast ast = do
    evd <- mapM eval ast
    case evd of
        MalFunction {fn=f} : args -> f args
        _ -> throwStr . (++) "invalid apply: " =<< liftIO (Printer._pr_str True (toList ast))

eval :: MalVal -> IOThrows MalVal
eval (MalSymbol sym)            = do
    case Map.lookup sym repl_env of
        Nothing  -> throwStr $ "'" ++ sym ++ "' not found"
        Just val -> return val
eval (MalSeq _ (Vect False) xs) = apply_ast xs
eval (MalSeq m (Vect True)  xs) = MalSeq m (Vect True) <$> mapM eval xs
eval (MalHashMap m xs)          = MalHashMap m         <$> mapM eval xs
eval ast                        = return ast

-- print

mal_print :: MalVal -> IOThrows String
mal_print = liftIO. Printer._pr_str True

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

repl_env :: Map.Map String MalVal
repl_env = Map.fromList [("+", _func add),
                         ("-", _func sub),
                         ("*", _func mult),
                         ("/", _func divd)]

rep :: String -> IOThrows String
rep = mal_print <=< eval <=< mal_read

repl_loop :: IO ()
repl_loop = do
    line <- readline "user> "
    case line of
        Nothing -> return ()
        Just "" -> repl_loop
        Just str -> do
            addHistory str
            res <- runExceptT $ rep str
            out <- case res of
                Left mv -> (++) "Error: " <$> liftIO (Printer._pr_str True mv)
                Right val -> return val
            putStrLn out
            hFlush stdout
            repl_loop

_func :: Fn -> MalVal
_func f = MalFunction {fn=f, f_ast=Nil, f_params=[], macro=False, meta=Nil}

main :: IO ()
main = do
    load_history

    repl_loop
