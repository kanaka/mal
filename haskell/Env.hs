module Env
( Env, env_new, null_env, env_get, env_set )
where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad.Trans (liftIO)
import Data.List (elemIndex)
import qualified Data.Map  as Map

import Types
import Printer

data EnvData = EnvPair (Maybe Env, (Map.Map String MalVal))

type Env = IORef EnvData

env_new :: Maybe Env -> IO Env
env_new outer  = newIORef $ EnvPair (outer, (Map.fromList []))

null_env = env_new Nothing

env_bind :: Env -> [MalVal] -> [MalVal] -> IO Env
env_bind env binds exprs = do
    case (elemIndex (MalSymbol "&") binds) of
        Just idx -> return env
        Nothing  -> return env

{-
isBound :: Env -> MalVal -> IO Bool
--isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var
isBound envRef (MalSymbol key) = do
    e <- readIORef envRef
    case e of
        EnvPair (o,m) -> case Map.lookup key m of
                              Nothing -> return False
                              Just _  -> return True
-}

env_find :: Env -> MalVal -> IO (Maybe Env)
env_find envRef sym@(MalSymbol key) = do
    e <- readIORef envRef
    case e of
        EnvPair (o, m) -> case Map.lookup key m of
                               Nothing  -> case o of
                                                Nothing -> return Nothing
                                                Just o  -> env_find o sym
                               Just val -> return $ Just envRef

env_get :: Env -> MalVal -> IO MalVal
env_get envRef sym@(MalSymbol key) = do
    e1 <- liftIO $ env_find envRef sym
    case e1 of
         Nothing   -> error $ "'" ++ key ++ "' not found"
         Just eRef -> do
            e2 <- liftIO $ readIORef eRef
            case e2 of
                EnvPair (o,m) -> case Map.lookup key m of
                                      Nothing  -> error $ "env_get error"
                                      Just val -> return val


env_set :: Env -> MalVal -> MalVal -> IO MalVal
env_set envRef (MalSymbol key) val = do
    e <- readIORef envRef
    case e of
        EnvPair (o,m) -> writeIORef envRef $ EnvPair (o, (Map.insert key val m))
    return val
