import System.IO (hFlush, stdout)
import Control.Monad.Except (liftIO, runExceptT)
import qualified Data.Map as Map

import Readline (addHistory, readline, load_history)
import Types
import Reader (read_str)
import Printer (_pr_list, _pr_str)

--
--  Set this to True for a trace of each call to Eval.
--
traceEval :: Bool
traceEval = False

-- read

mal_read :: String -> IOThrows MalVal
mal_read = read_str

-- eval

apply_ast :: MalVal -> [MalVal] -> IOThrows MalVal
apply_ast first rest = do
    evd <- eval first
    case evd of
        MalFunction _ f -> f =<< mapM eval rest
        _               -> throwStr . (++) "invalid apply: " =<< liftIO (_pr_list True " " $ first : rest)

eval :: MalVal -> IOThrows MalVal
eval ast = do
    case traceEval of
        True -> liftIO $ do
            putStr "EVAL: "
            putStrLn =<< _pr_str True ast
            hFlush stdout
        False -> pure ()
    case ast of
        MalSymbol sym -> do
            case Map.lookup sym repl_env of
                Nothing  -> throwStr $ "'" ++ sym ++ "' not found"
                Just val -> return val
        MalSeq _ (Vect False) (a1 : as) -> apply_ast a1 as
        MalSeq _ (Vect True)  xs        -> MalSeq (MetaData Nil) (Vect True) <$> mapM eval xs
        MalHashMap _ xs                 -> MalHashMap (MetaData Nil) <$> mapM eval xs
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

repl_env :: Map.Map String MalVal
repl_env = Map.fromList [("+", _func add),
                         ("-", _func sub),
                         ("*", _func mult),
                         ("/", _func divd)]

rep :: String -> IOThrows String
rep line = mal_print =<< eval =<< mal_read line

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
_func f = MalFunction (MetaData Nil) f

main :: IO ()
main = do
    load_history

    repl_loop
