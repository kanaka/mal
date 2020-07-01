module Core

import Control.Monad.Syntax
import Data.List
import Data.Strings
import Eval
import Reader
import Types

-- Builtins
interface MalType a where
  toMalAst : a -> AST
  fromMalAst : AST -> Maybe a

implementation MalType AST where
  toMalAst = id
  fromMalAst = Just

implementation MalType () where
  toMalAst () = Nil
  fromMalAst Nil = Just ()
  fromMalAst _ = Nothing

implementation MalType Integer where
  toMalAst = Number
  fromMalAst (Number x) = Just x
  fromMalAst _ = Nothing

implementation MalType Int where
  toMalAst = Number . cast
  fromMalAst (Number x) = Just $ cast x
  fromMalAst _ = Nothing

implementation MalType String where
  toMalAst = Str
  fromMalAst (Str s) = Just s
  fromMalAst _ = Nothing

implementation MalType Bool where
  toMalAst = Boolean
  fromMalAst (Boolean b) = Just b
  fromMalAst _ = Nothing

implementation MalType a => MalType (List a) where
  toMalAst = List True . map toMalAst -- Should this be List or Vector?
  fromMalAst (List _ xs) = traverse fromMalAst xs
  fromMalAst Nil = Just []
  fromMalAst _ = Nothing

implementation MalType a => MalType (SortedMap String a) where
  toMalAst = Map . map toMalAst
  fromMalAst (Map m) = traverse fromMalAst m
  fromMalAst _ = Nothing

interface MalFunction a where
  toMalFunc : a -> List AST -> MalM AST

implementation MalType a => MalFunction a where
  toMalFunc x [] = pure $ toMalAst x
  toMalFunc _ xs = do
    traverse eval xs -- (+ 1 2 (println "hi")) should print hi
    throwError $ Str "Too many arguments"

implementation MalType a => MalFunction (MalM a) where
  toMalFunc x [] = map toMalAst x
  toMalFunc _ xs = do
    traverse eval xs
    throwError $ Str "Too many arguments"

implementation (MalType a, MalFunction b) => MalFunction (a -> b) where
  toMalFunc _ [] = throwError $ Str "Too few arguments"
  toMalFunc f (x::xs) = do
    Just x' <- map fromMalAst $ eval x
      | Nothing => traverse_ eval xs *> throwError (Str "Wrong argument type")
    toMalFunc (f x') xs

defBuiltin : List AST -> MalM AST
defBuiltin [] = throwError $ Str "def!: too few arguments"
defBuiltin [_] = throwError $ Str "def!: too few arguments"
defBuiltin [Symbol n, x] = do
  x' <- eval x
  insert n x'
  pure x'
defBuiltin [_, _] = throwError $ Str "def!: expecting symbol"
defBuiltin _ = throwError $ Str "def!: too many arguments"

letBuiltin : List AST -> MalM AST
letBuiltin [] = throwError $ Str "let*: too few arguments"
letBuiltin [_] = throwError $ Str "let*: too few arguments"
letBuiltin [List _ defs, ans] = withLocalEnv $ go defs
  where
    go : List AST -> MalM AST
    go [] = eval ans
    go (Symbol n::x::rest) = do
      x' <- eval x
      insert n x'
      go rest
    go _ = throwError $ Str "let*: malformed arguments"
letBuiltin [_, _] = throwError $ Str "let*: malformed arguments"
letBuiltin _ = throwError $ Str "let*: too many arguments"

doBuiltin : List AST -> MalM AST
doBuiltin xs = do
  Just x <- map last' $ traverse eval xs
    | Nothing => throwError $ Str "do: too few arguments"
  pure x

ifBuiltin : List AST -> MalM AST
ifBuiltin [] = throwError $ Str "if: too few arguments"
ifBuiltin [_] = throwError $ Str "if: too few arguments"
ifBuiltin [cond, tt] = do
  cond' <- eval cond
  if truthiness cond' then eval tt else pure Nil
ifBuiltin [cond, tt, ff] = do
  cond' <- eval cond
  if truthiness cond' then eval tt else eval ff
ifBuiltin _ = throwError $ Str "if: too few arguments"

fnBuiltin : List AST -> MalM AST
fnBuiltin [List _ argNames, res] = do
  argNames' <- traverse getSymbol argNames
  parentEnv <- ask
  pure $ Func $ \args => do
    args' <- traverse eval args
    local (const parentEnv) $ withLocalEnv $ do
      bindArgs argNames' args'
      eval res
  where getSymbol : AST -> MalM String
        getSymbol (Symbol n) = pure n
        getSymbol _ = throwError $ Str "fn: malformed arguments"
        bindArgs : List String -> List AST -> MalM ()
        bindArgs [] [] = pure ()
        bindArgs [] _ = throwError $ Str "too many arguments"
        bindArgs ["&",n] xs = insert n $ List False xs
        bindArgs _ [] = throwError $ Str "too few arguments"
        bindArgs (n::ns) (x::xs) = do
          insert n x
          bindArgs ns xs
fnBuiltin _ = throwError $ Str "fn: malformed arguments"

isList : AST -> Bool
isList (List False _) = True
isList _ = False

isVector : AST -> Bool
isVector (List True _) = True
isVector _ = False

-- Specialized on MalM to help type inference
liftIO' : MalType a => IO a -> MalM AST
liftIO' = map toMalAst . MonadTrans.liftIO

prStr : List AST -> MalM AST
prStr xs = do
  xs' <- traverse eval xs
  pure $ Str $ fastAppend $ intersperse " " $ map (toString True) xs'
str : List AST -> MalM AST
str xs = do
  xs' <- traverse eval xs
  pure $ Str $ fastAppend $ map (toString False) xs'
prn : List AST -> MalM AST
prn xs = do
  xs' <- traverse eval xs
  liftIO' $ putStrLn $ fastAppend $ intersperse " " $ map (toString True) xs'
println : List AST -> MalM AST
println xs = do
  xs' <- traverse eval xs
  liftIO' $ putStrLn $ fastAppend $ intersperse " " $ map (toString False) xs'

baseEnv : SortedMap String AST
baseEnv = fromList [
  ("nil", Nil),
  ("true", Boolean True),
  ("false", Boolean False),
  ("+", Func $ toMalFunc $ (+) {ty=Integer}),
  ("-", Func $ toMalFunc $ (-) {ty=Integer}),
  ("*", Func $ toMalFunc $ (*) {ty=Integer}),
  ("/", Func $ toMalFunc $ div {ty=Integer}),
  ("def!", Func defBuiltin),
  ("let*", Func letBuiltin),
  ("if", Func ifBuiltin),
  ("fn*", Func fnBuiltin),
  ("do", Func doBuiltin),
  ("list", Func $ map (List False) . traverse eval),
  ("list?", Func $ toMalFunc isList),
  ("vector", Func $ map (List True) . traverse eval),
  ("vector?", Func $ toMalFunc isVector),
  ("count", Func $ toMalFunc $ the (List AST -> Integer) $ cast . List.length),
  ("=", Func $ toMalFunc $ (==) {ty=AST}),
  (">", Func $ toMalFunc $ (>) {ty=Integer}),
  (">=", Func $ toMalFunc $ (>=) {ty=Integer}),
  ("<", Func $ toMalFunc $ (<) {ty=Integer}),
  ("<=", Func $ toMalFunc $ (<=) {ty=Integer}),
  -- TODO: implement in corelib when varargs is done
  ("pr-str", Func prStr),
  ("str", Func str),
  ("prn", Func prn),
  ("println", Func println)
]

coreLib : String
coreLib = "
(def! empty? (fn* (l) (= (count l) 0)))
(def! not (fn* (a) (if a false true)))
"

public export
getStartingEnv : IO Env
getStartingEnv = do
  env <- map pure $ newIORef baseEnv
  res <- runExceptT $ flip runReaderT env $ do
    defs <- either (throwError . Str . ("parse error: "++)) pure $ parseText coreLib
    traverse_ eval defs
  case res of
       Right () => pure ()
       Left e => putStrLn $ "CORELIB: error: " ++ toString False e
  pure env
