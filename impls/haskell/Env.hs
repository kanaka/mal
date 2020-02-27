module Env
( Env, env_new, env_bind, env_get, env_set )
where

import Data.IORef (modifyIORef, newIORef, readIORef)
import qualified Data.Map  as Map

import Types

-- The Env type si defined in Types module to avoid dep cycle.

env_new :: Env -> IO Env
env_new outer = (: outer) <$> newIORef (Map.fromList [])

--  True means that the actual arguments match the signature.
env_bind :: Env -> [String] -> [MalVal] -> IO Bool
env_bind env (k : ks) (v : vs) | k /= "&" = do
    env_set env k v
    env_bind env ks vs
env_bind env ["&", k] vs = do
    env_set env k $ toList vs
    return True
env_bind _ [] [] = return True
env_bind _ _ _ = return False

env_get :: Env -> String -> IO (Maybe MalVal)
env_get [] _ = return Nothing
env_get (ref : outer) key = do
    hm <- readIORef ref
    case Map.lookup key hm of
        Nothing -> env_get outer key
        justVal -> return justVal

env_set :: Env -> String -> MalVal -> IO ()
env_set (ref : _) key val = modifyIORef ref $ Map.insert key val
env_set [] _ _ = error "assertion failed in env_set"
