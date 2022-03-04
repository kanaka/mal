module Env where

import Prelude

import Data.List (List(..), (:))
import Data.Map (fromFoldable, insert, lookup)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (error)
import Effect.Ref as Ref
import Types (Local, MalExpr, RefEnv, toList)



-- Environment

initEnv :: Local
initEnv = fromFoldable Nil


newEnv :: RefEnv -> Effect RefEnv
newEnv re = flip (:) re <$> Ref.new initEnv



-- VARIABLE

get :: RefEnv -> String -> Effect (Maybe MalExpr)
get Nil _ = pure Nothing
get (ref:outer) ky = do
  envs <- Ref.read ref
  case lookup ky envs of
    Nothing -> get outer ky
    ex      -> pure ex


sets :: RefEnv -> List String -> List MalExpr -> Effect Boolean
sets _ Nil Nil             = pure true
sets env ("&":k:Nil) exs   = set env k (toList exs) *> pure true
sets env (ky:kys) (ex:exs) = set env ky ex *> sets env kys exs
sets _ _ _                 = pure false


set :: RefEnv -> String -> MalExpr -> Effect Unit
set (re:_) ky ex = Ref.modify_ (insert ky ex) re
set Nil _ _      = error "assertion failed in env_set"