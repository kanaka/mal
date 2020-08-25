module Env
( Env, env_apply, env_get, env_let, env_put, env_repl, env_set )
where

import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.Map.Strict as Map

import Printer (_pr_str)
import Types

data Binds = Variable (IORef (Map.Map String MalVal))
           | Constant (Map.Map String MalVal)

type Env = [Binds]

env_repl :: IO Env
env_repl = (: []) . Variable <$> newIORef Map.empty

env_let :: Env -> IO Env
env_let outer = (: outer) . Variable <$> newIORef Map.empty

--  catch* should also use this
env_apply :: Env -> [MalVal] -> [MalVal] -> Maybe (Env)
env_apply outer keys values = (: outer) . Constant <$> bind keys values Map.empty

bind :: [MalVal] -> [MalVal] -> Map.Map String MalVal -> Maybe (Map.Map String MalVal)
bind [MalSymbol "&", (MalSymbol k)] vs       m = Just $ Map.insert k (toList vs) m
bind (MalSymbol k : ks)             (v : vs) m = bind ks vs $ Map.insert k v m
bind []                             []       m = Just m
bind _                              _        _ = Nothing

env_get :: Env -> String -> IO (Maybe MalVal)
env_get env key = loop env where
  loop :: Env -> IO (Maybe MalVal)
  loop [] = return Nothing
  loop (Constant m : outer) = case Map.lookup key m of
    Nothing -> loop outer
    justVal -> return justVal
  loop (Variable ref : outer) = do
    m <- readIORef ref
    case Map.lookup key m of
      Nothing -> loop outer
      justVal -> return justVal

--  def! and let*
env_set :: Env -> String -> MalVal -> IO ()
env_set (Variable ref : _) key value = modifyIORef ref $ Map.insert key value
env_set _         _   _     = error "assertion failed in env.env_set"

put1 :: (String, MalVal) -> IO ()
put1 (key, value) = do
  putChar ' '
  putStr key
  putChar ':'
  putStr =<< _pr_str True value

env_put :: Env -> IO ()
env_put []        = error "assertion failed in Env.env_format"
env_put (Variable ref : _) = mapM_ put1 =<< Map.assocs <$> readIORef ref
env_put (Constant m : _) = mapM_ put1 $ Map.assocs m
