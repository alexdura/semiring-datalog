module ParentTree (singleton, addChild, ParentTree(..)) where

import Data.List ( intercalate )

data ParentTree a = ParentTree {
  parent :: Maybe (ParentTree a, Int),
  children :: [ParentTree a],
  datum :: a
  } deriving (Eq)

instance Show a => Show (ParentTree a) where
  show (ParentTree pi cs d) = "(" ++ intercalate " " (show d : map show cs) ++ ")"

singleton :: a -> ParentTree a
singleton v =
  let r = ParentTree Nothing [] v in r

addChild :: ParentTree a -> ParentTree a -> ParentTree a
addChild c (ParentTree pi cs d) =
  let incIndex pt = pt {parent = pt.parent>>= \(p, i) -> Just (p, i + 1)}
      r = ParentTree pi (c {parent = Just (r, 0)} : map incIndex cs) d
  in r

data Expr a b = IntValue Int
              | StringValue String
              | BoolValue Bool
              | LetExpr String (Expr a b) (Expr a b)
              | IfExpr (Expr a b) (Expr a b) (Expr a b)
              | AppExpr (Expr a b) (Expr a b)
              | AttrExpr (Expr a b) b (Expr a b)
              | FuncValue (Expr a b -> Expr a b)
              | TreeValue (ParentTree a)

type Context a b = ParentTree a -> b -> Expr a b

step :: Context a b -> Expr a b -> Expr a b

isValue :: Expr a b -> Bool
isValue (IntValue _) = True
isValue (BoolValue _) = True
isValue (StringValue _) = True
isValue (TreeValue _) = True
isValue _ = False

step ctx v@(BoolValue _) = v
step ctx v@(StringValue _) = v
step ctx v@(IntValue _) = v
step ctx v@(TreeValue _) = v
step ctx (IfExpr (BoolValue True) t _) = t
step ctx (IfExpr (BoolValue False) _ f) = f
step ctx (IfExpr e t f) = IfExpr (step ctx e) t f

step ctx (AppExpr (FuncValue f) r) = if isValue r then f r
                                    else AppExpr (FuncValue f) (step ctx r)
step ctx (AppExpr l r) = AppExpr (step ctx l) r

step ctx (AttrExpr l attr r) = if not $ isValue l then (AttrExpr (step ctx l) attr r)
                               else if not $ isValue r then (AttrExpr l attr (step ctx r))
                                    else case l of
                                           TreeValue t -> step ctx (ctx t attr)
                                           _ -> error "Expected PTree as the first operand."
step _ e = error "Stuck!"

data Attribute = Parent
               | ChildL
               | ChildR
               | Value
