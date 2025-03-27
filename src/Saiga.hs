{-# LANGUAGE OverloadedStrings #-}

module Saiga(prettyExpr, Expr(..), SaigaElement(..), SaigaProgram, SaigaAttribute, (<?>),
             (<.>), (===), (<&&>), (<||>), (<:>), guard, otherwise,  int, nil, not,
             Domain(..), prettyDomain, AttributeCtx, makeAttributeCtx, evalWithLog, LogEntry(..)) where

import Prelude hiding (otherwise, not, log)
import Data.String ( IsString(..) )
import PicoJava(AST(..))
import Control.Monad.Writer hiding (guard)
import Control.Monad.Except hiding (guard)
import Control.Monad.State hiding (guard)
import Data.List (intercalate, find, singleton)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)

data Expr a = IVal Int
            | BVal Bool
            | SVal String
            | Nil
            | Cons (Expr a) (Expr a)
            | Head (Expr a)
            | Tail (Expr a)
            | Attr (Expr a) a [Expr a]
            | Func String [Expr a]
            | IfElse (Expr a) (Expr a) (Expr a)
            | IfEq (Expr a) (Expr a) (Expr a) (Expr a)
            | IfLt (Expr a) (Expr a) (Expr a) (Expr a)
            | Arg Int
            | Node
            | Let String (Expr a) (Expr a)
            | Var String
            deriving (Show, Eq)

instance IsString (Expr a) where
  fromString = SVal

priority :: Expr a -> Int

priority IfElse {} = 10
priority Cons {} = 9
priority Func {} = 8
priority Attr {} = 8
priority Head {} = 8
priority Tail {} = 8
priority _ = 1


parenthesize :: Ord a => a -> a -> String -> String
parenthesize parentPrio currentPrio str = if parentPrio > currentPrio then "(" ++ str ++ ")"
                                          else str

prettyExpr :: Show a => Int -> Expr a -> String
prettyExpr _ (IVal n) = show n
prettyExpr _ (BVal b) = show b
prettyExpr _ (SVal s) = "\"" ++ s ++ "\""
prettyExpr _ Nil = "nil"
prettyExpr parentPrio c@(Cons h t) =
  let currentPrio = priority c in
    parenthesize parentPrio currentPrio (prettyExpr currentPrio h ++ " : " ++ prettyExpr currentPrio t)
prettyExpr _ (Head l) = "head(" ++ prettyExpr 0 l ++ ")"
prettyExpr _ (Tail t) = "tail(" ++ prettyExpr 0 t ++ ")"
prettyExpr parentPrio a@(Attr n attr args) =
  let currentPrio = priority a in
    parenthesize parentPrio currentPrio (prettyExpr currentPrio n ++ "." ++ show attr ++ "(" ++ intercalate ", " (map (prettyExpr 0) args) ++ ")")
prettyExpr _ (Func name args) = name ++ "(" ++ intercalate ", " (map (prettyExpr 0) args) ++ ")"
prettyExpr parentPrio b@(IfElse c t f) =
  let currentPrio = priority b in
    parenthesize parentPrio currentPrio ("if " ++ prettyExpr currentPrio c ++ " then " ++ prettyExpr currentPrio t ++ " else " ++ prettyExpr currentPrio f)
prettyExpr _ (Arg n) = "arg[" ++ show n ++ "]"
prettyExpr _ Node = "node"
prettyExpr _ (Var name) = name
prettyExpr _ (Let name expr1 expr2) = "let " ++ name ++ " = " ++ (prettyExpr 0 expr1) ++ " in " ++ (prettyExpr 0 expr2)

prettyExpr parentPrio b@(IfEq l r t f) =
  let currentPrio = priority b in
    parenthesize parentPrio currentPrio
    ("if " ++
      prettyExpr currentPrio l ++ " == " ++ prettyExpr currentPrio r ++
      " then " ++ prettyExpr currentPrio t ++
      " else " ++ prettyExpr currentPrio f)

prettyExpr parentPrio b@(IfLt l r t f) =
  let currentPrio = priority b in
    parenthesize parentPrio currentPrio
    ("if " ++
      prettyExpr currentPrio l ++ " < " ++ prettyExpr currentPrio r ++
      " then " ++ prettyExpr currentPrio t ++
      " else " ++ prettyExpr currentPrio f)

--prettyExpr _ e = "Missing pretty printing for expression " ++ show e


data SaigaElement attr a = Attribute attr Int (Expr attr)
                         | CircularAttribute attr Int (Expr attr)
                           (Expr attr) -- initial value
                           String -- join operator
                         | BuiltinAttribute attr Int (Domain a -> [Domain a] -> Domain a)
                         | Function String Int (Expr attr)
                         | BuiltinFunction String Int ([Domain a] -> Domain a)


type SaigaProgram attr a = [SaigaElement attr a]

class (Ord a, Show a, Enum a) => SaigaAttribute a

isAttribute :: SaigaElement attr a -> Bool
isAttribute (Attribute {}) = True
isAttribute (BuiltinAttribute {}) = True
isAttribute _ = False

numFormalArgs :: SaigaElement attr a -> Int
numFormalArgs (Attribute _ n _) = n
numFormalArgs (BuiltinAttribute _ n _) = n
numFormalArgs (Function _ n _) = n
numFormalArgs (BuiltinFunction _ n _) = n

isBuiltin :: SaigaElement attr a -> Bool
isBuiltin (BuiltinAttribute {}) = True
isBuiltin (BuiltinFunction {}) = True
isBuiltin _ = False


lookupAttribute :: (SaigaAttribute attr) => attr -> SaigaProgram attr a -> Maybe (SaigaElement attr a)
lookupAttribute name = find (\case
                                Attribute attr _ _ -> attr == name
                                BuiltinAttribute attr _ _ -> attr == name
                                CircularAttribute attr _ _ _ _ -> attr == name
                                _ -> False)

lookupFunction :: (SaigaAttribute attr) => String -> SaigaProgram attr a -> Maybe (SaigaElement attr a)
lookupFunction name = find (\case
                               Function f _ _ -> f == name
                               BuiltinFunction f _ _ -> f == name
                               _ -> False)

makeAttributeCtx :: SaigaAttribute attr => SaigaProgram attr a -> AttributeCtx attr a
makeAttributeCtx p = AttributeCtx {
  lookup = \attr -> case lookupAttribute attr p of
      Just (Attribute _ _ expr) -> Just expr
      _ -> Nothing,

  builtin = \attr -> case lookupAttribute attr p of
      Just (BuiltinAttribute _ _ f) -> Just (f . DNode)
      _ -> Nothing,

  circular = \attr -> case lookupAttribute attr p of
      Just (CircularAttribute _ _ f i j) -> Just (f, i, j)
      _ -> Nothing,

  func = \name -> case lookupFunction name p of
      Just (Function _ _ expr) -> Just expr
      _ -> Nothing,

  builtinFunc = \name -> case lookupFunction name p of
      Just (BuiltinFunction _ _ f) -> Just f
      _ -> Nothing
  }


-- data AttributeDef a = AttributeDef {attr::a, equation::(Expr a)}
--   deriving (Eq, Show)


int = IVal
nil = Nil


infixr 5 <:>
(<:>) = Cons

infixl 8 <.>
(<.>) :: Expr a -> (Expr a -> Expr a) -> Expr a
(<.>) n f = f n

infixl 9 <?>
(<?>) attr arg = \n -> Attr n attr arg


infixl 2 <||>
(<||>) l r = IfElse l (BVal True) r

infixr 3 <&&>
(<&&>) l r = IfElse l (IfElse r (BVal True) (BVal False)) (BVal False)

not e = IfElse e (BVal False) (BVal True)


infix 4 ===
(===) :: Expr a -> Expr a -> Expr a
(===) l r = IfEq l r (BVal True) (BVal False)


otherwise :: Expr a
otherwise = BVal True

guard :: [(Expr a, Expr a)] -> Expr a
-- optimize the case when the condition is ===
guard [(IfEq l r (BVal True) (BVal False), expr0), (BVal True, expr1)] = IfEq l r expr0 expr1
guard ((IfEq l r (BVal True) (BVal False), expr0) : gs) = IfEq l r expr0 (guard gs)
-- general case
guard [(cond0, expr0), (BVal True, expr1)] = IfElse cond0 expr0 expr1
guard ((cond0, expr0) : gs) = IfElse cond0 expr0 (guard gs)

-- domain of values
data Domain a = DInt Int
              | DString String
              | DBool Bool
              | DNode (AST a)
              | DList [Domain a]
              deriving (Show, Eq, Ord)

prettyDomain :: Show a => Domain a -> String
prettyDomain (DInt n) = show n
prettyDomain (DString s) = show s
prettyDomain (DBool b) = show b
prettyDomain (DNode n) = show n.kind
prettyDomain (DList ds) = "[" ++ intercalate ", "  (prettyDomain <$> ds) ++ "]"


-- context for attributes
data AttributeCtx attr a = AttributeCtx {
  lookup :: attr -> Maybe (Expr attr),
  builtin :: attr -> Maybe (AST a -> [Domain a] -> Domain a),
  circular :: attr -> Maybe (Expr attr, Expr attr, String),
  func :: String -> Maybe (Expr attr),
  builtinFunc :: String -> Maybe ([Domain a] -> Domain a)
  }

data AttrState a = AttrState {
  attrValue :: Domain a,
  attrComputed :: Bool,
  attrVisited :: Bool
  }

mkAttrState :: Domain a -> AttrState a
mkAttrState v = AttrState v False False

data EvalEnv a = EvalEnv {
  node :: Maybe (AST a),
  args :: [Domain a],
  vars :: Map String (Domain a)
  }

mkFuncEvalEnv :: [Domain a] -> EvalEnv a
mkFuncEvalEnv args = EvalEnv Nothing args Map.empty

mkAttrEvalEnv :: AST a -> [Domain a] -> EvalEnv a
mkAttrEvalEnv n args = EvalEnv (Just n) args Map.empty

mkExprEvalEnv :: EvalEnv a
mkExprEvalEnv = EvalEnv Nothing [] Map.empty

bindVar :: String -> Domain a -> EvalEnv a -> EvalEnv a
bindVar name v env = env {vars = Map.insert name v env.vars}

lookupVar :: String -> EvalEnv a -> Maybe (Domain a)
lookupVar name env = Map.lookup name env.vars

data EvalCtx attr a = EvalCtx {
  attrCache :: Map (AST a , attr, [Domain a]) (AttrState a),
  inCircle :: Bool,
  change :: Bool
  }

mkEvalCtx :: EvalCtx attr a
mkEvalCtx = EvalCtx Map.empty False False



computeFixpoint :: (Ord a,
                    Ord attr)
                => AttributeCtx attr a
                -> EvalEnv a
                -> attr -- attribute
                -> Expr attr -- equation
                -> Domain a -- old value
                -> EvalMonad attr a (Domain a)

computeFixpoint ctx env attr expr v = do
  ectx <- get
  put $ ectx {change = False}
  v' <- evalM ctx env expr
  ectx' <- get
  let attrCacheKey = (fromJust env.node, attr, env.args)
      as = fromJust $ Map.lookup attrCacheKey ectx.attrCache
  if v' /= v || ectx'.change then do
    put $ ectx' {
      change = True,
      attrCache = Map.insert attrCacheKey as {attrValue = v'} ectx'.attrCache
      }
    computeFixpoint ctx env attr expr v'
  else do
    return v'


evalCircularAttr :: (Ord a,
                     Ord attr)
                 => AttributeCtx attr a
                 -> EvalEnv a -- environment (args, node, variables)
                 -> attr -- attribute
                 -> Expr attr -- equation
                 -> Expr attr -- initial value
                 -> EvalMonad attr a (Domain a)

evalCircularAttr ctx env attr expr iexpr = do
  ectx <- get
  let attrCacheKey = (fromJust env.node, attr, env.args)
  case Map.lookup attrCacheKey ectx.attrCache of
    Just as -> if as.attrComputed then return as.attrValue
               else if ectx.inCircle then if as.attrVisited then return as.attrValue
                                          else do -- !visited
      put $ ectx {attrCache = Map.insert attrCacheKey as {attrVisited = True} ectx.attrCache}
      v' <- evalM ctx env expr
      ectx' <- get
      put $ ectx' {change = ectx'.change || v' /= as.attrValue,
                   attrCache = Map.insert attrCacheKey as {attrValue = v',
                                                           attrVisited = False} ectx'.attrCache
                  }
      return v'
                    else do -- !inCircle
      put $ ectx {inCircle = True,
                  attrCache = Map.insert attrCacheKey as {attrVisited = True} ectx.attrCache}
      v' <- computeFixpoint ctx env attr expr as.attrValue
      ectx' <- get
      put $ ectx' {inCircle = False,
                   attrCache = Map.insert attrCacheKey as {attrValue = v',
                                                           attrVisited = False,
                                                           attrComputed = True}
                               ectx'.attrCache}
      return v'
    Nothing -> do -- this should trigger a circular evaluation
      v' <- evalM ctx env iexpr
      ectx' <- get
      put $ ectx' {attrCache = Map.insert attrCacheKey (mkAttrState v') ectx'.attrCache}
      evalCircularAttr ctx env attr expr iexpr


type EvalMonad attr a r = ExceptT String (WriterT [LogEntry attr a] (State (EvalCtx attr a))) r


data LogEntry attr a = LogEntry [Domain a] (Maybe (AST a)) (Expr attr) (Domain a)
                     | LogInfo String


logRet :: EvalEnv a  -> Expr attr -> Domain a -> EvalMonad attr a (Domain a)
logRet env e r = do
  lift $ tell [LogEntry env.args env.node e r]
  return r


logRetAttr :: EvalEnv a -> Expr attr -> Domain a -> EvalMonad attr a (Domain a)
logRetAttr env e r = do
  lift $ tell [LogEntry env.args env.node e r]
  return r

logErr :: EvalEnv a -> Expr attr -> String -> EvalMonad attr a (Domain a)
logErr env e m = do
  lift $ tell [LogEntry env.args env.node  e (DBool False)]
  throwError m

logInfo :: String -> EvalMonad attr a ()
logInfo s = lift $ tell [LogInfo s]


evalM :: (Ord a, Ord attr)
      => AttributeCtx attr a -- context
      -> EvalEnv a -- eval environment
      -> Expr attr -- expression
      -> EvalMonad attr a (Domain a)

evalM _ env e@(IVal i) = logRet env e (DInt i)

evalM _ env e@(BVal b) = logRet env e (DBool b)

evalM _ env  e@(SVal s) = logRet env e (DString s)

evalM _ env e@Nil = logRet env e (DList [])

evalM _ env e@(Arg k) = logRet env e (env.args !! k)

evalM _ env e@Node = logRet env e (DNode $ fromJust env.node)

evalM ctx env e@(Cons x xs) = do
  x' <- evalM ctx env x
  xs' <- evalM ctx env xs
  case xs' of
    r@(DList xs'') -> logRet env e (DList (x':xs''))
    _ -> logErr env e $ "Second argument of Cons must be a a list."

evalM ctx env e@(Head l) = do
  l' <- evalM ctx env l
  case l' of
    DList (v:_) -> logRet env e v
    _ -> logErr env e $ "Head operation defined only for non-empty lists."

evalM ctx env e@(Tail l) = do
  l' <- evalM ctx env l
  case l' of
    DList (_:vs) -> logRet env e (DList vs)
    _ -> logErr env e $ "Tail operation defined only for non-empty lists."

evalM ctx env e@(IfElse c t f) = do
  c' <- evalM ctx env c
  case c' of
    (DBool True) -> evalM ctx env t
    (DBool False) -> evalM ctx env f
    r -> logErr env e $ "If condition must evaluate to a boolean value."

evalM ctx env e@(IfEq l r t f) = do
  l' <- evalM ctx env l
  r' <- evalM ctx env r
  if l' == r' then evalM ctx env t
    else evalM ctx env f

evalM ctx env e@(IfLt l r t f) = do
  l' <- evalM ctx env l
  r' <- evalM ctx env r
  if l' < r' then evalM ctx env t
    else evalM ctx env f

evalM ctx env expr@(Func name es) = do
  args' <- mapM (evalM ctx env) es
  case ctx.func name of
    Just expr -> evalM ctx (mkFuncEvalEnv args') expr
    _ -> case ctx.builtinFunc name of
      Just f -> logRet (mkFuncEvalEnv args') expr (f args')
      Nothing -> logErr env expr $ "No builtin function " ++ name

evalM ctx env e@(Attr b attr args) = do
  arg' <- mapM (evalM ctx env) args
  b' <- evalM ctx env b
  case b' of
    DNode b'' -> let env' = mkAttrEvalEnv b'' arg' in
      case ctx.builtin attr of
        Just f -> logRetAttr env' e (f b'' arg')
        Nothing -> case ctx.lookup attr of
                     Just eq -> do
                       r <- evalM ctx env' eq
                       logRetAttr env' e r
                     Nothing -> case ctx.circular attr of
                       Just (eq, i, j) -> do
                         r <- evalCircularAttr ctx env' attr eq i
                         logRetAttr env' e r
                       Nothing -> logErr env e "Missing attribute definition"
    _ -> logErr env e "Only AST nodes have attributes."


evalM _ env e@(Var name) = case lookupVar name env of
  Just v -> logRet env e v
  Nothing -> logErr env e $ "Missing binding for variable " ++ name

evalM ctx env e@(Let name expr1 expr2) = do
  v <- evalM ctx env expr1
  v' <- evalM ctx (bindVar name v env) expr2
  logRet env e v'

evalWithLog :: (Ord a, Ord attr)
            => AttributeCtx attr a -- context
            -> [Domain a] -- argument values
            -> AST a -- current node
            -> Expr attr -- expression
            -> (Either String (Domain a), [LogEntry attr a])

evalWithLog ctx args n e =
  evalState (runWriterT $ runExceptT (evalM ctx (mkAttrEvalEnv n args) e)) mkEvalCtx
