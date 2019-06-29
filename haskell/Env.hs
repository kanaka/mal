module Env
( Env, env_new, env_bind, env_find, env_get, env_set )
where

import Data.IORef (modifyIORef, newIORef, readIORef)
import Control.Monad.Trans (liftIO)
import Data.List (elemIndex)
import qualified Data.Map  as Map

import Types

-- These Env types are defined in Types module to avoid dep cycle
--data EnvData = EnvPair (Maybe Env, (Map.Map String MalVal))
--type Env = IORef EnvData

env_new :: Maybe Env -> IO Env
env_new outer  = newIORef $ EnvPair (outer, (Map.fromList []))

env_bind :: Env -> [String] -> [MalVal] -> IO ()
env_bind envRef binds exprs = do
   case (elemIndex "&" binds) of
        Nothing  -> do
            -- bind binds to exprs
            mapM_ (\(b,e) -> env_set envRef b e) $ zip binds exprs
        Just idx -> do
            -- Varargs binding
            _ <- mapM (\(b,e) -> env_set envRef b e) $
                zip (take idx binds) (take idx exprs)
            env_set envRef (binds !! (idx + 1))
                                (MalList (drop idx exprs) Nil)

env_find :: Env -> String -> IO (Maybe Env)
env_find envRef key = do
    e <- readIORef envRef
    case e of
        EnvPair (o, m) -> case Map.lookup key m of
                               Nothing  -> case o of
                                                Nothing -> return Nothing
                                                Just outer -> env_find outer key
                               Just _   -> return $ Just envRef

env_get :: Env -> String -> IOThrows MalVal
env_get envRef key = do
    e1 <- liftIO $ env_find envRef key
    case e1 of
         Nothing   -> throwStr $ "'" ++ key ++ "' not found"
         Just eRef -> do
            e2 <- liftIO $ readIORef eRef
            case e2 of
                EnvPair (_, m) -> case Map.lookup key m of
                                      Nothing  -> throwStr $ "env_get error"
                                      Just val -> return val


env_set :: Env -> String -> MalVal -> IO ()
env_set env key val = liftIO $ modifyIORef env f where
    f (EnvPair (o, m)) = EnvPair (o, Map.insert key val m)
