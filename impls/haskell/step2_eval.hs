import Control.Monad.Except (liftIO, runExceptT)
import qualified Data.Map.Strict as Map

import Readline (addHistory, readline, load_history)
import Types
import Reader (read_str)
import Printer(_pr_list, _pr_str)

type Env = Map.Map String MalVal

-- read

mal_read :: String -> IOThrows MalVal
mal_read = read_str

-- eval

apply_ast :: MalVal -> [MalVal] -> Env -> IOThrows MalVal
apply_ast first rest env = do
    evd <- eval env first
    case evd of
        MalFunction _ f -> f =<< mapM (eval env) rest
        _               -> throwStr . (++) "invalid apply: " =<< liftIO (_pr_list True " " $ first : rest)

eval :: Env -> MalVal -> IOThrows MalVal
eval env ast = do
    --  putStr "EVAL: "
    --  putStrLn =<< mal_print ast
    case ast of
        MalSymbol sym -> do
            let maybeVal = Map.lookup sym env
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

main :: IO ()
main = do
    let repl_env = Map.fromList [("+", MalFunction (MetaData Nil) add),
                                 ("-", MalFunction (MetaData Nil) sub),
                                 ("*", MalFunction (MetaData Nil) mult),
                                 ("/", MalFunction (MetaData Nil) divd)]

    load_history
    repl_loop repl_env
