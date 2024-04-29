{-# LANGUAGE OverloadedStrings #-}

module Saiga(prettyExpr, Expr(..), AttributeDef(..), SaigaElement(..), SaigaProgram, SaigaAttribute, (<?>),
             (<.>), (===), (<&&>), (<:>), guard, otherwise, ifOK, isUnknown, int, nil, not,
             Domain(..), prettyDomain, AttributeCtx, makeAttributeCtx, evalWithLog, LogEntry(..)) where

import Prelude hiding (otherwise, not, log)
import Data.String ( IsString(..) )
import PicoJava(AST(..))
import Control.Monad.Writer hiding (guard)
import Control.Monad.Except hiding (guard)
import Data.List (intercalate, find)

data Expr a = IVal Int
            | BVal Bool
            | SVal String
            | Nil
            | Cons (Expr a) (Expr a)
            | Head {list::Expr a}
            | Tail {list::Expr a}
            | Attr {node::Expr a, attr::a, arg::Expr a}
            | Func {name::String, arg::Expr a}
            | IfElse {cond::Expr a, true::Expr a, false::Expr a}
            | Arg
            | Node
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
prettyExpr parentPrio a@(Attr n attr arg) =
  let currentPrio = priority a in
    parenthesize parentPrio currentPrio (prettyExpr currentPrio n ++ "." ++ show attr ++ "(" ++ prettyExpr 0 arg ++ ")")
prettyExpr _ (Func name arg) = name ++ "(" ++ prettyExpr 0 arg ++ ")"
prettyExpr parentPrio b@(IfElse c t f) =
  let currentPrio = priority b in
    parenthesize parentPrio currentPrio ("if " ++ prettyExpr currentPrio c ++ " then " ++ prettyExpr currentPrio t ++ " else " ++ prettyExpr currentPrio f)
prettyExpr _ Arg = "arg"
prettyExpr _ Node = "node"

data SaigaElement attr a = Attribute attr (Expr attr)
                         | BuiltinAttribute attr (Domain a -> Domain a -> Domain a)
                         | Function String (Expr attr)
                         | BuiltinFunction String (Domain a -> Domain a)


type SaigaProgram attr a = [SaigaElement attr a]

class (Eq a, Show a, Enum a) => SaigaAttribute a

isAttribute :: SaigaElement attr a -> Bool
isAttribute (Attribute {}) = True
isAttribute (BuiltinAttribute {}) = True
isAttribute _ = False


isBuiltin :: SaigaElement attr a -> Bool
isBuiltin (BuiltinAttribute {}) = True
isBuiltin (BuiltinFunction {}) = True
isBuiltin _ = False


lookupAttribute :: (SaigaAttribute attr) => attr -> SaigaProgram attr a -> Maybe (SaigaElement attr a)
lookupAttribute name = find (\case
                                Attribute attr _ -> attr == name
                                BuiltinAttribute attr _ -> attr == name
                                _ -> False)

lookupFunction :: (SaigaAttribute attr) => String -> SaigaProgram attr a -> Maybe (SaigaElement attr a)
lookupFunction name = find (\case
                               Function f _ -> f == name
                               BuiltinFunction f _ -> f == name
                               _ -> False)

makeAttributeCtx :: SaigaAttribute attr => SaigaProgram attr a -> AttributeCtx attr a
makeAttributeCtx p = AttributeCtx {
  lookup = \attr -> case lookupAttribute attr p of
      Just (Attribute _ expr) -> AttributeDef attr expr
      _ -> error "Attribute definition not found.",

  builtin = \attr -> case lookupAttribute attr p of
      Just (BuiltinAttribute _ f) -> Just (f . DNode)
      _ -> Nothing,

  func = \name -> case lookupFunction name p of
      Just (Function _ expr) -> Just expr
      _ -> Nothing,

  builtinFunc = \name -> case lookupFunction name p of
      Just (BuiltinFunction _ f) -> Just f
      _ -> Nothing
  }


data AttributeDef a = AttributeDef {attr::a, equation::(Expr a)}
  deriving (Eq, Show)


int = IVal
nil = Nil


infixr 5 <:>
(<:>) = Cons

infixl 8 <.>
(<.>) :: Expr a -> (Expr a -> Expr a) -> Expr a
(<.>) n f = f n

infixl 9 <?>
(<?>) attr arg = \n -> Attr n attr arg


infixr 3 <&&>
(<&&>) l r = IfElse l (IfElse r (BVal True) (BVal False)) (BVal False)

not e = IfElse e (BVal False) (BVal True)


equal = Func "eq"
infix 4 ===
(===) l r = equal $ l <:> r <:> Nil


otherwise :: Expr a
otherwise = BVal True

guard :: [(Expr a, Expr a)] -> Expr a
guard [(cond0, expr0), (BVal True, expr1)] = IfElse cond0 expr0 expr1
guard ((cond0, expr0) : gs) = IfElse cond0 expr0 (guard gs)

isUnknown = Func "isUnknown"
ifOK tExpr fExpr = IfElse (isUnknown tExpr) fExpr tExpr


-- domain of values
data Domain a = DInt Int
              | DString String
              | DBool Bool
              | DNode (AST a)
              | DList [Domain a]
              deriving (Show, Eq)

prettyDomain :: Show a => Domain a -> String
prettyDomain (DInt n) = show n
prettyDomain (DString s) = show s
prettyDomain (DBool b) = show b
prettyDomain (DNode n) = show n.kind
prettyDomain (DList ds) = "[" ++ intercalate ", "  (prettyDomain <$> ds) ++ "]"


-- context for attributes
data AttributeCtx attr a = AttributeCtx {
  lookup :: attr -> AttributeDef attr,
  builtin :: attr -> Maybe (AST a -> Domain a -> Domain a),
  func :: String -> Maybe (Expr attr),
  builtinFunc :: String -> Maybe (Domain a -> Domain a)
  }




data LogEntry attr a = LogEntry (Domain a) (AST a) (Expr attr) (Domain a)

evalM :: AttributeCtx attr a -- context
      -> Domain a -- argument value
      -> AST a -- current node
      -> Expr attr -- expression
      -> ExceptT String (Writer [LogEntry attr a]) (Domain a)

logRet :: Domain a -> AST a -> Expr attr -> Domain a -> ExceptT String (Writer [LogEntry attr a]) (Domain a)
logRet arg n e r = do
  -- lift $ tell [LogEntry arg n e r]
  return r


logRetAttr :: Domain a -> AST a -> Expr attr -> Domain a -> ExceptT String (Writer [LogEntry attr a]) (Domain a)
logRetAttr arg n e r = do
  lift $ tell [LogEntry arg n e r]
  return r


logErr :: Domain a -> AST a -> Expr attr -> String -> ExceptT String (Writer [LogEntry attr a]) (Domain a)
logErr arg n e m = do
  lift $ tell [LogEntry arg n e (DBool False)]
  throwError m

evalM _ arg n e@(IVal i) = logRet arg n e (DInt i)

evalM _ arg n e@(BVal b) = logRet arg n e (DBool b)

evalM _ arg n  e@(SVal s) = logRet arg n e (DString s)

evalM _ arg n e@Nil = logRet arg n e (DList [])

evalM _ arg n e@Arg = logRet arg n e arg

evalM _ arg n e@Node = logRet arg n e (DNode n)

evalM ctx arg n e@(Cons x xs) = do
  x' <- evalM ctx arg n x
  xs' <- evalM ctx arg n xs
  case xs' of
    r@(DList xs'') -> logRet arg n e (DList (x':xs''))
    _ -> logErr arg n e $ "Second argument of Cons must be a a list."

evalM ctx arg n e@(Head l) = do
  l' <- evalM ctx arg n l
  case l' of
    DList (v:_) -> logRet arg n e v
    _ -> logErr arg n e $ "Head operation defined only for non-empty lists."

evalM ctx arg n e@(Tail l) = do
  l' <- evalM ctx arg n l
  case l' of
    DList (_:vs) -> logRet arg n e (DList vs)
    _ -> logErr arg n e $ "Tail operation defined only for non-empty lists."

evalM ctx arg n e@(IfElse c t f) = do
  c' <- evalM ctx arg n c
  case c' of
    (DBool True) -> evalM ctx arg n t
    (DBool False) -> evalM ctx arg n f
    r -> logErr arg n e $ "If condition must evalMuate to a boolean value."

evalM ctx arg n expr@(Func name e) = do
  arg' <- evalM ctx arg n e
  case ctx.func name of
    Just expr -> evalM ctx arg' n expr
    _ -> case ctx.builtinFunc name of
      Just f -> logRet arg n expr (f arg')
      Nothing -> logErr arg n expr $ "No builtin function " ++ name

evalM ctx argb n e@(Attr b attr arg) = do
  arg' <- evalM ctx argb n arg
  b' <- evalM ctx argb n b
  case b' of
    DNode b'' -> case ctx.builtin attr of
      Just f -> logRetAttr arg' b'' e (f b'' arg')
      Nothing -> do
        let eq = (ctx.lookup attr).equation
        r <- evalM ctx arg' b'' eq
        logRetAttr arg' b'' e r
    _ -> logErr argb n e "Only AST nodes have attributes."


evalWithLog :: AttributeCtx attr a -- context
            -> Domain a -- argument value
            -> AST a -- current node
            -> Expr attr -- expression
            -> (Either String (Domain a), [LogEntry attr a])

evalWithLog ctx arg n e =
  let w = runExceptT (evalM ctx arg n e) in
    runWriter w
