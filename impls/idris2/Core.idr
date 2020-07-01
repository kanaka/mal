module Core

import Control.Monad.Syntax
import Data.List
import Data.Strings
import System
import System.File

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
  toMalAst () = Symbol "nil"
  fromMalAst (Symbol "nil") = Just ()
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
  toMalAst True = Symbol "true"
  toMalAst False = Symbol "false"
  fromMalAst (Symbol "true") = Just True
  fromMalAst (Symbol "false") = Just False
  -- fromMalAst (Symbol "nil") = Just False -- Maybe?
  fromMalAst _ = Nothing

implementation MalType (IORef AST) where
  toMalAst = Atom
  fromMalAst (Atom a) = Just a
  fromMalAst _ = Nothing

implementation MalType a => MalType (List a) where
  toMalAst = List False . map toMalAst
  fromMalAst (List _ xs) = traverse fromMalAst xs
  fromMalAst (Symbol "nil") = Just []
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

-- Specialized on MalM to help type inference
liftIO' : MalType a => IO a -> MalM AST
liftIO' = map toMalAst . MonadTrans.liftIO

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
  if truthiness cond' then eval tt else pure $ Symbol "nil"
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

readStringBuiltin : String -> MalM AST
readStringBuiltin s =
  case parseText s of
       Right [] => pure $ Symbol "nil"
       Right [ast] => pure ast
       Right xs => pure $ List True xs
       Left e => throwError $ Str $ "parse error: " ++ e

slurpBuiltin : String -> MalM String
slurpBuiltin file = do
  res <- liftIO $ readFile file
  case res of
       Right contents => pure contents
       Left _ => throwError $ Str $ "slurp: could not read file " ++ file

atomBuiltin : AST -> MalM AST
atomBuiltin = liftIO' . newIORef

resetBuiltin : IORef AST -> AST -> MalM AST
resetBuiltin r x = do
  liftIO $ writeIORef r x
  pure x

derefBuiltin : IORef AST -> MalM AST
derefBuiltin = MonadTrans.liftIO . readIORef

swapBuiltin : List AST -> MalM AST
swapBuiltin [] = throwError $ Str "swap!: too few arguments"
swapBuiltin [_] = throwError $ Str "swap!: too few arguments"
swapBuiltin xs = do
  Atom x::Func f::args <- traverse eval xs
    | _ => throwError $ Str "swap!: type error"
  old <- liftIO $ readIORef x
  new <- f (old::args)
  resetBuiltin x new

consBuiltin : AST -> List AST -> List AST
consBuiltin = (::)

concatBuiltin : List AST -> MalM AST
concatBuiltin = map (List False) . go <=< traverse eval
  where go : List AST -> MalM (List AST)
        go (List _ l::ls) = map (l ++) $ go ls
        go (_::_) = throwError $ Str "concat: expected list"
        go [] = pure []

quoteBuiltin : List AST -> MalM AST
quoteBuiltin [x] = pure x
quoteBuiltin _ = throwError $ Str "quote: wanted exactly one argument"

quasiquoteBuiltin : List AST -> MalM AST
quasiquoteBuiltin [x] = qq x
  where expandSplice : AST -> MalM (List AST)
        expandSplice (List False [Symbol "splice-unquote", x]) = do
          x' <- eval x
          case x' of
               List _ xs => pure xs
               _ => pure [x']
        expandSplice x = pure [x]
        qq : AST -> MalM AST
        qq (Symbol x) = pure $ Symbol x
        qq (Str s) = pure $ Str s
        qq (Number x) = pure $ Number x
        qq (Atom a) = pure $ Atom a
        qq (WithMeta a b) = pure $ WithMeta a b -- TODO
        qq (List False [Symbol "unquote", a]) = eval a
        qq (List _ xs) = do
          xs' <- traverse qq xs
          map (List False . concat) $ traverse expandSplice xs'
        qq (Map m) = map Map $ traverse qq m
        qq (Func f) = pure $ Func f
quasiquoteBuiltin _ = throwError $ Str "quote: wanted exactly one argument"

prStr : List AST -> MalM AST
prStr xs = do
  xs' <- traverse eval xs
  sxs <- liftIO $ traverse (toString True) xs'
  pure $ Str $ fastAppend $ intersperse " " sxs
str : List AST -> MalM AST
str xs = do
  xs' <- traverse eval xs
  sxs <- liftIO $ traverse (toString False) xs'
  pure $ Str $ fastAppend sxs
prn : List AST -> MalM AST
prn xs = do
  xs' <- traverse eval xs
  sxs <- liftIO $ traverse (toString True) xs'
  liftIO' $ putStrLn $ fastAppend $ intersperse " " sxs
println : List AST -> MalM AST
println xs = do
  xs' <- traverse eval xs
  sxs <- liftIO $ traverse (toString False) xs'
  liftIO' $ putStrLn $ fastAppend $ intersperse " " sxs

baseEnv : SortedMap String AST
baseEnv = fromList [
  ("nil", Symbol "nil"),
  ("true", Symbol "true"),
  ("false", Symbol "false"),
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
  ("println", Func println),
  ("read-string", Func $ toMalFunc readStringBuiltin),
  ("slurp", Func $ toMalFunc slurpBuiltin),
  ("eval", Func $ toMalFunc $ withGlobalEnv . eval),
  ("atom", Func $ toMalFunc atomBuiltin),
  ("atom?", Func $ toMalFunc isAtom),
  ("deref", Func $ toMalFunc derefBuiltin),
  ("reset!", Func $ toMalFunc resetBuiltin),
  ("swap!", Func swapBuiltin),
  ("cons", Func $ toMalFunc consBuiltin),
  ("concat", Func concatBuiltin),
  ("quote", Func quoteBuiltin),
  ("quasiquote", Func quasiquoteBuiltin)
]

coreLib : String
coreLib = "
(def! empty? (fn* (l) (= (count l) 0)))
(def! not (fn* (a) (if a false true)))
(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\\nnil)\")))))
"

public export
getStartingEnv : IO Env
getStartingEnv = do
  args <- getArgs
  let argv =
    case args of
         (_::_::rest) => List False $ map Str rest
         _ => List False []
  env <- map pure $ newIORef $ insert "*ARGV*" argv baseEnv
  res <- runExceptT $ flip runReaderT env $ do
    defs <- either (throwError . Str . ("parse error: "++)) pure $ parseText coreLib
    traverse_ eval defs
  case res of
       Right () => pure ()
       Left e => do
         se <- toString False e
         putStrLn $ "CORELIB: error: " ++ se
  pure env
