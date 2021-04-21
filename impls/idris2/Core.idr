module Core

import Control.Monad.Syntax
import Data.List
import Data.Maybe
import Data.Strings
import System
import System.Clock
import System.File

import Class
import Eval
import Reader
import Types

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

defmacroBuiltin : List AST -> MalM AST
defmacroBuiltin [] = throwError $ Str "defmacro!: too few arguments"
defmacroBuiltin [_] = throwError $ Str "defmacro!: too few arguments"
defmacroBuiltin [Symbol n, x] = do
  Func _ m <- eval x
    | _ => throwError $ Str "defmacro!: expected a function"
  let m' = Func True $ eval <=< local (record { evalFunc = don'tEval }) . m
  insert n m'
  pure m'
defmacroBuiltin [_, _] = throwError $ Str "defmacro!: expecting symbol"
defmacroBuiltin _ = throwError $ Str "defmacro!: too many arguments"

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
  pure $ Func False $ \args => do
    args' <- traverse eval args
    local (const parentEnv) $ withLocalEnv $ do
      bindArgs argNames' args'
      -- If it's a regular function, then it only runs during fullEval mode.
      -- If it's a macro, we need to evaluate it even it macroexpand mode.
      -- So fullEval is the right thing to do here.
      fullEval res
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
swapBuiltin (a::f::args) = do
  (Atom x, Func _ f') <- [| MkPair (eval a) (eval f) |]
    | _ => throwError $ Str "swap!: type error"
  new <- f' (List False [Symbol "deref", Atom x]::args)
  resetBuiltin x new

consBuiltin : AST -> List AST -> List AST
consBuiltin = (::)

firstBuiltin : List AST -> AST
firstBuiltin [] = Symbol "nil"
firstBuiltin (x::_) = x

restBuiltin : List AST -> List AST
restBuiltin [] = []
restBuiltin (_::xs) = xs

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
        qq (List False [Symbol "unquote", a]) = eval a
        qq (List _ xs) = do
          xs' <- traverse qq xs
          map (List False . concat) $ traverse expandSplice xs'
        qq (Map m) = map Map $ traverse qq m
        qq (Func m f) = pure $ Func m f
quasiquoteBuiltin _ = throwError $ Str "quote: wanted exactly one argument"

macroexpandBuiltin : List AST -> MalM AST
macroexpandBuiltin [x] = local (record { evalFunc = macroexpand }) $ eval x
macroexpandBuiltin _ = throwError $ Str "macroexpand: wanted exactly one argument"

tryBuiltin : List AST -> MalM AST
tryBuiltin [x] = eval x
tryBuiltin [x, List False [Symbol "catch*", Symbol e, handler]] =
  catchError (eval x) $ \err => withLocalEnv $ do
    insert e err
    eval handler
tryBuiltin _ = throwError $ Str "try*: malformed arguments"

typeofBuiltin : AST -> AST
typeofBuiltin (Symbol _) = Keyword "symbol"
typeofBuiltin (Str s) = Keyword $ if map fst (strUncons s) == Just '\xff' then "keyword" else "string"
typeofBuiltin (Number _) = Keyword "number"
typeofBuiltin (Atom _) = Keyword "atom"
typeofBuiltin (List False _) = Keyword "list"
typeofBuiltin (List True _) = Keyword "vector"
typeofBuiltin (Map _) = Keyword "map"
typeofBuiltin (Func False _) = Keyword "fn"
typeofBuiltin (Func True _) = Keyword "macro"

assocBuiltin : List AST -> MalM AST
assocBuiltin = traverse eval >=> assoc
  where go : SortedMap String AST -> List AST -> MalM (SortedMap String AST)
        go m [] = pure m
        go m [_] = throwError $ Str "odd number of arguments"
        go m (Str k::v::rest) = go (insert k v m) rest
        go m _ = throwError $ Str "expected a string or keyword"
        assoc : List AST -> MalM AST
        assoc [] = throwError $ Str "too few arguments"
        assoc (Map m::rest) = map Map $ go m rest
        assoc _ = throwError $ Str "expected a map"

dissocBuiltin : List AST -> MalM AST
dissocBuiltin = traverse eval >=> dissoc
  where go : SortedMap String AST -> List AST -> MalM (SortedMap String AST)
        go m [] = pure m
        go m (Str k::rest) = go (delete k m) rest
        go m _ = throwError $ Str "expecting a string or keyword"
        dissoc : List AST -> MalM AST
        dissoc [] = throwError $ Str "too few arguments"
        dissoc (Map m::rest) = map Map $ go m rest
        dissoc _ = throwError $ Str "expected a map"

getBuiltin : SortedMap String AST -> String -> AST
getBuiltin m k = fromMaybe (Symbol "nil") $ lookup k m

containsBuiltin : SortedMap String AST -> String -> Bool
containsBuiltin m k = isJust $ lookup k m

timeBuiltin : MalM Integer
timeBuiltin = do
  MkClock s ns <- liftIO $ clockTime UTC
  let ms = 1000 * s + ns `div` 1000000
  pure ms

conjBuiltin : List AST -> MalM AST
conjBuiltin = conj <=< traverse eval
  where conj : List AST -> MalM AST
        conj (List False xs::rest) = pure $ List False $ foldl (flip (::)) xs rest
        conj (List True xs::rest) = pure $ List True $ xs ++ rest
        conj (_::_) = throwError $ Str "conj: expecting a list or a vector"
        conj [] = throwError $ Str "conj: too few arguments"

seqBuiltin : AST -> MalM AST
seqBuiltin (Symbol "nil") = pure $ Symbol "nil"
seqBuiltin (List _ []) = pure $ Symbol "nil"
seqBuiltin (Str "") = pure $ Symbol "nil"
seqBuiltin (List _ xs) = pure $ List False xs
seqBuiltin (Str s) = case strUncons s of
                          Just ('\xff', _) => throwError $ Str "seq: expecting a list, vector, or string"
                          _ => pure $ List False $ map (Str . pack . pure) $ unpack s
seqBuiltin _ = throwError $ Str "seq: expecting a list, vector, or string"

readlineBuiltin : String -> MalM AST
readlineBuiltin s =
  case strUncons s of
       Just ('\xff', _) => throwError $ Str "readline: expecing a string"
       _ => liftIO $ do
         False <- fEOF stdin
           | True => putStr "\n" *> pure (Symbol "nil")
         putStr s
         map Str getLine

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
  ("*host-language*", Str "idris2"),
  ("+", Func False $ toMalFunc $ (+) {ty=Integer}),
  ("-", Func False $ toMalFunc $ (-) {ty=Integer}),
  ("*", Func False $ toMalFunc $ (*) {ty=Integer}),
  ("/", Func False $ toMalFunc $ div {ty=Integer}),
  ("def!", Func False $ defBuiltin),
  ("defmacro!", Func False $ defmacroBuiltin),
  ("let*", Func False letBuiltin),
  ("if", Func False ifBuiltin),
  ("fn*", Func False fnBuiltin),
  ("do", Func False doBuiltin),
  ("list", Func False $ map (List False) . traverse eval),
  ("vector", Func False $ map (List True) . traverse eval),
  ("count", Func False $ toMalFunc $ the (List AST -> Integer) $ cast . List.length),
  ("=", Func False $ toMalFunc $ (==) {ty=AST}),
  (">", Func False $ toMalFunc $ (>) {ty=Integer}),
  (">=", Func False $ toMalFunc $ (>=) {ty=Integer}),
  ("<", Func False $ toMalFunc $ (<) {ty=Integer}),
  ("<=", Func False $ toMalFunc $ (<=) {ty=Integer}),
  -- TODO: implement in corelib
  ("pr-str", Func False prStr),
  ("str", Func False str),
  ("prn", Func False prn),
  ("println", Func False println),
  ("read-string", Func False $ toMalFunc readStringBuiltin),
  ("slurp", Func False $ toMalFunc slurpBuiltin),
  ("eval", Func False $ toMalFunc $ withGlobalEnv . eval),
  ("atom", Func False $ toMalFunc atomBuiltin),
  ("deref", Func False $ toMalFunc derefBuiltin),
  ("reset!", Func False $ toMalFunc resetBuiltin),
  ("swap!", Func False swapBuiltin),
  ("cons", Func False $ toMalFunc consBuiltin),
  ("concat", Func False concatBuiltin),
  ("first", Func False $ toMalFunc firstBuiltin),
  ("rest", Func False $ toMalFunc restBuiltin),
  ("quote", Func False quoteBuiltin),
  ("quasiquote", Func False quasiquoteBuiltin),
  ("macroexpand", Func False macroexpandBuiltin),
  ("try*", Func False tryBuiltin),
  ("throw", Func False $ toMalFunc $ the (AST -> MalM AST) throwError),
  ("idris-typeof", Func False $ toMalFunc typeofBuiltin),
  ("idris-str-to-keyword", Func False $ toMalFunc Keyword),
  ("symbol", Func False $ toMalFunc Symbol),
  ("assoc", Func False assocBuiltin),
  ("dissoc", Func False dissocBuiltin),
  ("keys", Func False $ toMalFunc $ the (SortedMap String AST -> List String) keys),
  ("vals", Func False $ toMalFunc $ the (SortedMap String AST -> List AST) values),
  ("get", Func False $ toMalFunc getBuiltin),
  ("contains?", Func False $ toMalFunc containsBuiltin),
  ("readline", Func False $ toMalFunc readlineBuiltin),
  ("time-ms", Func False $ toMalFunc timeBuiltin),
  ("conj", Func False conjBuiltin),
  ("seq", Func False $ toMalFunc seqBuiltin),
  ("meta", Func False $ const $ throwError $ Str "unimplemented"),
  ("with-meta", Func False $ const $ throwError $ Str "unimplemented")
]

coreLib : String
coreLib = "
(def! empty? (fn* (l) (= (count l) 0)))
(def! not (fn* (a) (if a false true)))
(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\\nnil)\")))))
(defmacro! cond
  (fn* (& xs) (if (> (count xs) 0)
        (list 'if (first xs)
              (if (> (count xs) 1)
                  (first (rest xs))
                  (throw \"odd number of forms to cond\"))
              (cons 'cond (rest (rest xs)))
        )
  ))
)
(def! nth (fn* (xs n)
  (cond
    (not (sequential? xs)) (throw \"expecting a list or vector\")
    (if (< n 0) true (empty? xs)) (throw \"nth: index out of bounds\")
    (= n 0) (first xs)
    true (nth (rest xs) (- n 1))
  )
))
(def! apply
  (let*
    ( init (fn* (l) (if (<= (count l) 1) () (cons (first l) (init (rest l)))))
    , last (fn* (l) (if (<= (count l) 1) (first l) (last (rest l))))
    )
    (fn* (f & args)
      (if (empty? args)
          (throw \"apply: expected at least two arguments\")
          (eval
            (cons f
              (map (fn* (x) (list 'quote x))
                (concat (init args) (last args))))
          )
      )
    )
  )
)
(def! map
  (fn* (f list)
    (if (empty? list)
        ()
        (cons (f (first list)) (map f (rest list)))
    )
  )
)
(def! number? (fn* (x) (= (idris-typeof x) :number)))
(def! string? (fn* (x) (= (idris-typeof x) :string)))
(def! list? (fn* (x) (= (idris-typeof x) :list)))
(def! vector? (fn* (x) (= (idris-typeof x) :vector)))
(def! atom? (fn* (x) (= (idris-typeof x) :atom)))
(def! map? (fn* (x) (= (idris-typeof x) :map)))
(def! keyword? (fn* (x) (= (idris-typeof x) :keyword)))
(def! nil? (fn* (x) (= x nil)))
(def! true? (fn* (x) (= x true)))
(def! false? (fn* (x) (= x false)))
;; nil, true, and false are implemented as symbols, but (symbol? nil) == false
(def! symbol? (fn* (x)
  (if (= (idris-typeof x) :symbol)
      (if (nil? x)
          false
          (if (true? x)
              false
              (if (false? x) false true)
          )
      )
      false
  )
))
(def! fn? (fn* (x) (= (idris-typeof x) :fn)))
(def! macro? (fn* (x) (= (idris-typeof x) :macro)))
(def! sequential? (fn* (x) (if (list? x) true (vector? x))))
(def! keyword (fn* (x) (if (keyword? x) x (idris-str-to-keyword x))))
(def! hash-map (fn* (& args) (eval `(assoc {} ~@args))))
"

public export
getStartingEnv : IO Env
getStartingEnv = do
  args <- getArgs
  let argv =
    case args of
         (_::_::rest) => List False $ map Str rest
         _ => List False []
  env <- map (\v => MkEnv fullEval [v]) $ newIORef $ insert "*ARGV*" argv baseEnv
  res <- runExceptT $ flip runReaderT env $ do
    defs <- either (throwError . Str) pure $ parseText coreLib
    traverse_ eval defs
  case res of
       Right () => pure ()
       Left e => do
         se <- toString False e
         putStrLn $ "CORELIB: error: " ++ se
  pure env
