{-# LANGUAGE OverloadedStrings #-}

module Saiga(prettyExpr, Expr(..), AttributeDef(..), SaigaElement(..), SaigaProgram, SaigaAttribute, (<?>),
             (<.>), (===), (<&&>), (<:>), guard, otherwise,  int, nil, not,
             Domain(..), prettyDomain, AttributeCtx, makeAttributeCtx, evalWithLog, LogEntry(..)) where

import Prelude hiding (otherwise, not, log)
import Data.String ( IsString(..) )
import PicoJava(AST(..))
import Control.Monad.Writer hiding (guard)
import Control.Monad.Except hiding (guard)
import Data.List (intercalate, find, singleton)


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
            | Arg Int
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
prettyExpr parentPrio a@(Attr n attr args) =
  let currentPrio = priority a in
    parenthesize parentPrio currentPrio (prettyExpr currentPrio n ++ "." ++ show attr ++ "(" ++ intercalate ", " (map (prettyExpr 0) args) ++ ")")
prettyExpr _ (Func name args) = name ++ "(" ++ intercalate ", " (map (prettyExpr 0) args) ++ ")"
prettyExpr parentPrio b@(IfElse c t f) =
  let currentPrio = priority b in
    parenthesize parentPrio currentPrio ("if " ++ prettyExpr currentPrio c ++ " then " ++ prettyExpr currentPrio t ++ " else " ++ prettyExpr currentPrio f)
prettyExpr _ (Arg n) = "arg[" ++ show n ++ "]"
prettyExpr _ Node = "node"

data SaigaElement attr a = Attribute attr Int (Expr attr)
                         | BuiltinAttribute attr Int (Domain a -> [Domain a] -> Domain a)
                         | Function String Int (Expr attr)
                         | BuiltinFunction String Int ([Domain a] -> Domain a)


type SaigaProgram attr a = [SaigaElement attr a]

class (Eq a, Show a, Enum a) => SaigaAttribute a

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
                                _ -> False)

lookupFunction :: (SaigaAttribute attr) => String -> SaigaProgram attr a -> Maybe (SaigaElement attr a)
lookupFunction name = find (\case
                               Function f _ _ -> f == name
                               BuiltinFunction f _ _ -> f == name
                               _ -> False)

makeAttributeCtx :: SaigaAttribute attr => SaigaProgram attr a -> AttributeCtx attr a
makeAttributeCtx p = AttributeCtx {
  lookup = \attr -> case lookupAttribute attr p of
      Just (Attribute _ _ expr) -> AttributeDef attr expr
      _ -> error "Attribute definition not found.",

  builtin = \attr -> case lookupAttribute attr p of
      Just (BuiltinAttribute _ _ f) -> Just (f . DNode)
      _ -> Nothing,

  func = \name -> case lookupFunction name p of
      Just (Function _ _ expr) -> Just expr
      _ -> Nothing,

  builtinFunc = \name -> case lookupFunction name p of
      Just (BuiltinFunction _ _ f) -> Just f
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
  lookup :: attr -> AttributeDef attr,
  builtin :: attr -> Maybe (AST a -> [Domain a] -> Domain a),
  func :: String -> Maybe (Expr attr),
  builtinFunc :: String -> Maybe ([Domain a] -> Domain a)
  }




data LogEntry attr a = LogEntry [Domain a] (AST a) (Expr attr) (Domain a)

evalM :: Eq a =>
         AttributeCtx attr a -- context
      -> [Domain a] -- argument values
      -> AST a -- current node
      -> Expr attr -- expression
      -> ExceptT String (Writer [LogEntry attr a]) (Domain a)

logRet :: [Domain a] -> AST a -> Expr attr -> Domain a -> ExceptT String (Writer [LogEntry attr a]) (Domain a)
logRet args n e r = do
  -- lift $ tell [LogEntry arg n e r]
  return r


logRetAttr :: [Domain a] -> AST a -> Expr attr -> Domain a -> ExceptT String (Writer [LogEntry attr a]) (Domain a)
logRetAttr args n e r = do
  lift $ tell [LogEntry args n e r]
  return r


logErr :: [Domain a] -> AST a -> Expr attr -> String -> ExceptT String (Writer [LogEntry attr a]) (Domain a)
logErr args n e m = do
  lift $ tell [LogEntry args n e (DBool False)]
  throwError m

evalM _ args n e@(IVal i) = logRet args n e (DInt i)

evalM _ args n e@(BVal b) = logRet args n e (DBool b)

evalM _ args n  e@(SVal s) = logRet args n e (DString s)

evalM _ args n e@Nil = logRet args n e (DList [])

evalM _ args n e@(Arg k) = logRet args n e (args !! k)

evalM _ args n e@Node = logRet args n e (DNode n)

evalM ctx args n e@(Cons x xs) = do
  x' <- evalM ctx args n x
  xs' <- evalM ctx args n xs
  case xs' of
    r@(DList xs'') -> logRet args n e (DList (x':xs''))
    _ -> logErr args n e $ "Second argument of Cons must be a a list."

evalM ctx args n e@(Head l) = do
  l' <- evalM ctx args n l
  case l' of
    DList (v:_) -> logRet args n e v
    _ -> logErr args n e $ "Head operation defined only for non-empty lists."

evalM ctx args n e@(Tail l) = do
  l' <- evalM ctx args n l
  case l' of
    DList (_:vs) -> logRet args n e (DList vs)
    _ -> logErr args n e $ "Tail operation defined only for non-empty lists."

evalM ctx args n e@(IfElse c t f) = do
  c' <- evalM ctx args n c
  case c' of
    (DBool True) -> evalM ctx args n t
    (DBool False) -> evalM ctx args n f
    r -> logErr args n e $ "If condition must evaluate to a boolean value."

evalM ctx args n e@(IfEq l r t f) = do
  l' <- evalM ctx args n l
  r' <- evalM ctx args n r
  if l' == r' then evalM ctx args n t
    else evalM ctx args n f

evalM ctx args n expr@(Func name es) = do
  args' <- mapM (evalM ctx args n) es
  case ctx.func name of
    Just expr -> evalM ctx args' n expr
    _ -> case ctx.builtinFunc name of
      Just f -> logRet args n expr (f args')
      Nothing -> logErr args n expr $ "No builtin function " ++ name

evalM ctx argb n e@(Attr b attr args) = do
  arg' <- mapM (evalM ctx argb n) args
  b' <- evalM ctx argb n b
  case b' of
    DNode b'' -> case ctx.builtin attr of
      Just f -> logRetAttr arg' b'' e (f b'' arg')
      Nothing -> do
        let eq = (ctx.lookup attr).equation
        r <- evalM ctx arg' b'' eq
        logRetAttr arg' b'' e r
    _ -> logErr argb n e "Only AST nodes have attributes."


evalWithLog :: Eq a =>
               AttributeCtx attr a -- context
            -> [Domain a] -- argument values
            -> AST a -- current node
            -> Expr attr -- expression
            -> (Either String (Domain a), [LogEntry attr a])

evalWithLog ctx args n e =
  let w = runExceptT (evalM ctx args n e) in
    runWriter w
