{-# LANGUAGE NoImplicitPrelude #-}

module Saiga where

import Prelude (Show, Eq, Bool(..), Int, String, ($))


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

data AttributeDef a = AttributeDef a (Expr a)

int = IVal
bool = BVal
nil = Nil
head = Head
tail = Tail
n = Node


infixr <:>
(<:>) = Cons

infixl 9 <.>
(<.>) n attr = \arg -> Attr n attr arg

infixl 9 <?>
(<?>) n attr = n <.> attr $ Nil

infixl 8 <&&>
(<&&>) l r = IfElse l (IfElse r (BVal True) (BVal False)) (BVal False)


equal = Func "eq"
infix 2 ===
(===) l r = equal $ l <:> r <:> Nil



hasKind = Node <.> Kind $ Nil
otherwise = BVal True

guard [(cond0, expr0), (BVal True, expr1)] = IfElse cond0 expr0 expr1
guard ((cond0, expr0) : gs) = IfElse cond0 expr0 (guard gs)

isUknonwn = Func "isUnknown"

ifOK tExpr fExpr = IfElse (isUknonwn tExpr) fExpr tExpr


-- attribute declarations
data PicoJavaAttr = Decl
                  | LocalLookup
                  | Lookup
                  | RemoteLookup
                  | Kind
                  | Child
                  | Parent
                  | Use
                  | Name
                  | Superclass
                  | Type
                  deriving (Eq, Show)



-- attribute definitions - Figure 15
declAttr = AttributeDef Decl $ guard [(hasKind === (SVal "Dot"), Node <.> Child $ (int 1) <?> Decl), -- L1
                                      (hasKind === (SVal "Use"), Node <.> Lookup $ (n <?> Name)), -- L2
                                      (otherwise, SVal "UnknownDecl")] -- L3

localLookupAttr = AttributeDef LocalLookup $ guard [(hasKind === (SVal "Program"), Node <.> LocalLookup $ (head Arg)), -- L4
                                                    (hasKind === (SVal "Block"), ifOK (Node <.> LocalLookup $ (head Arg)) -- L5
                                                                                 (ifOK (Node <?> Parent <?> Superclass <.> Child $ (int 2) <.> RemoteLookup $ (head Arg))
                                                                                  Node <?> Parent <.> Lookup $ (head Arg))),
                                                    (hasKind === (SVal "Use") <&&> (Node <?> Parent <?> Kind === (SVal "Dot")), Node <?> Parent <.> Child $ (int 0) <?> Decl <?> Type <.> Child $ (int 2) <.> RemoteLookup $ (head Arg)), -- L6
                                                    (isUknonwn Node, SVal "UnknownDecl"), -- L7
                                                    (otherwise, Node <?> Parent <.> Lookup $ (head Arg))] -- L8
