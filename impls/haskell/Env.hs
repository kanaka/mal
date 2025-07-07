module Env
( Env, env_get, env_new, env_put, env_set )
where

import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.Map.Strict as Map

import Printer (_pr_str)
import Types

data Env = Env (Maybe Env) (IORef (Map.Map String MalVal))

env_new :: Maybe Env -> IO Env
env_new outer = Env outer <$> newIORef Map.empty

env_get :: Env -> String -> IO (Maybe MalVal)
env_get (Env maybeOuter ref) key = do
    m <- readIORef ref
    case Map.lookup key m of
      Nothing -> case maybeOuter of
          Nothing    -> return Nothing
          Just outer -> env_get outer key
      justVal -> return justVal

env_set :: Env -> String -> MalVal -> IO ()
env_set (Env _ ref) key value = modifyIORef ref $ Map.insert key value

put1 :: (String, MalVal) -> IO ()
put1 (key, value) = do
  putChar ' '
  putStr key
  putChar ':'
  putStr =<< _pr_str True value

env_put :: Env -> IO ()
env_put (Env _ ref) = mapM_ put1 =<< Map.assocs <$> readIORef ref
