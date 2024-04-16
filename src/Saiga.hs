{-# LANGUAGE OverloadedStrings #-}

module Saiga(prettyAttributeDef, Expr(..), AttributeDef(..), (<?>),
             (<.>), (===), (<&&>), (<:>), guard, otherwise, ifOK, isUnknown, int, nil, not,
             Domain(..), AttributeCtx(..), eval) where

import Prelude hiding (otherwise, not)
import Data.String ( IsString(..) )
import PicoJava(AST(..))

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


data AttributeDef a = AttributeDef {attr::a, equation::(Expr a)}
  deriving (Eq, Show)

prettyAttributeDef :: Show a => AttributeDef a -> String
prettyAttributeDef (AttributeDef attr e) = "node" ++ "." ++ show attr ++ "(" ++ "arg" ++ ") = " ++ prettyExpr 0 e

int = IVal
bool = BVal
nil = Nil
head = Head
tail = Tail
n = Node


infixr <:>
(<:>) = Cons

infixl 8 <.>
(<.>) :: Expr a -> (Expr a -> Expr a) -> Expr a
(<.>) n f = f n

infixl 9 <?>
(<?>) attr arg = \n -> Attr n attr arg


infixl 8 <&&>
(<&&>) l r = IfElse l (IfElse r (BVal True) (BVal False)) (BVal False)

not e = IfElse e (BVal False) (BVal True)


equal = Func "eq"
infix 2 ===
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

-- context for attributes
data AttributeCtx attr a = AttributeCtx {
  lookup :: attr -> AttributeDef attr,
  builtin :: attr -> Maybe (AST a -> Domain a -> Domain a),
  func :: String -> Maybe (Expr attr),
  builtinFunc :: String -> Maybe (Domain a -> Domain a)
  }


-- big-step semantics for attribute evaluation
eval :: Show a => AttributeCtx attr a -- context
     -> Domain a -- argument value
     -> AST a -- current node
     -> Expr attr -- expression
     -> Domain a

eval _ _ _ (IVal i) = DInt i

eval _ _ _ (BVal b) = DBool b

eval _ _ _ (SVal s) = DString s

eval _ _ _ Nil = DList []

eval _ arg _ (Arg) = arg

eval _ _ n (Node) = DNode n

eval ctx arg n (Cons x xs) = let x' = (eval ctx arg n x) in
  case (eval ctx arg n xs) of
    DList xs' -> DList (x':xs')
    _ -> error "Second Cons argument must be a list"

eval ctx arg n (Head l) = case eval ctx arg n l of
  DList (v:_) -> v
  r -> error $ "Head operation defined only for non-empty lists." ++ show r

eval ctx arg n (Tail l) =
  case eval ctx arg n l  of
  DList (_:vs) -> DList vs
  r -> error $ "Tail operation defined only for non-empty lists." ++ show r

eval ctx arg n (IfElse c t f) = case (eval ctx arg n c) of
  (DBool True) -> eval ctx arg n t
  (DBool False) -> eval ctx arg n f
  _ -> error "If condintion must evaluate to a boolean value."

eval ctx arg n (Func name e) =
  let arg' = eval ctx arg n e in
    case ctx.func name of
      Just expr -> eval ctx arg' n expr
      _ -> case ctx.builtinFunc name of
        Just f -> f arg'
        Nothing -> error $ "No builtin function " ++ name

eval ctx argb n (Attr b attr arg) =
  let arg' = eval ctx argb n arg
      b' = eval ctx argb n b in
    case b' of
      DNode b'' -> case ctx.builtin attr of
        Just f -> f b'' arg'
        Nothing -> eval ctx arg' b'' (ctx.lookup attr).equation
      _ -> error "Only AST nodes have attributes."
