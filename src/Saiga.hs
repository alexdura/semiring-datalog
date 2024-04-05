{-# LANGUAGE OverloadedStrings #-}

module Saiga(declAttr, localLookupAttr, prettyAttributeDef) where

import Prelude (Show, Eq, Ord, Bool(..), Int, String, ($), (++), show, (>), flip)
import Data.String ( IsString(..) )


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


data AttributeDef a = AttributeDef a (Expr a)
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



hasKind :: Expr PicoJavaAttr
hasKind = Node <.> Kind <?> nil


otherwise :: Expr a
otherwise = BVal True

guard :: [(Expr a, Expr a)] -> Expr a
guard [(cond0, expr0), (BVal True, expr1)] = IfElse cond0 expr0 expr1
guard ((cond0, expr0) : gs) = IfElse cond0 expr0 (guard gs)

isUnknown = Func "isUnknown"
ifOK tExpr fExpr = IfElse (isUnknown tExpr) fExpr tExpr


-- attribute declarations
data PicoJavaAttr = Decl
                  | LocalLookup
                  | Lookup
                  | RemoteLookup
                  | Kind
                  | Child
                  | Parent
                  | Name
                  | Superclass
                  | Type
                  deriving (Eq, Show)

predefs = Func "predefs" Nil

-- attribute definitions - Figure 15
declAttr = AttributeDef Decl $
  let use = Child <?> (int 1)
      name = Name <?> nil
      decl = Decl <?> nil
  in
    guard [(hasKind === (SVal "Dot"), Node <.> use <.> decl), -- L1
           (hasKind === (SVal "Use"), Node <.> Lookup <?> (n <.> name)), -- L2
           (otherwise, SVal "UnknownDecl")] -- L3

lookupAttr = AttributeDef Lookup $
  let parent = Parent <?> nil
      superclass = Superclass <?> nil
      block = Child <?> (int 2)
      decl = Decl <?> nil
      typ = Type <?> nil
      kind = Kind <?> nil
      access = Child <?> (int 0)
  in
    guard [(hasKind === "Program", Node <.> LocalLookup <?> Arg), -- L4
            (hasKind === "Block", ifOK (Node <.> LocalLookup <?> (Arg)) -- L5
                                  (ifOK (Node <.> parent <.> superclass <.> block <.> RemoteLookup <?> (Arg))
                                   Node <.> parent <.> Lookup <?> (Arg))),
            (hasKind === "Use" <&&> (Node <.> parent <.> kind === "Dot"), Node <.> parent <.> access <.> decl <.> typ <.> block <.> RemoteLookup <?> (Arg)), -- L6
            (isUnknown Node, "UnknownDecl"), -- L7
            (otherwise, Node <.> parent <.> Lookup <?> (Arg))] -- L8


localLookupAttr = AttributeDef LocalLookup $
  let items = Child <?> (int 0)
  in
    guard [(hasKind === "Program", ifOK (Func "finddecl" Arg <:> (Node <.> items) <:> Nil)
                                   (Func "finddecl" Arg <:> predefs <:> Nil)),
           (hasKind === "Block", Func "finddecl" Arg <:> (Node <.> items) <:> Nil),
           (otherwise, "UnknownDecl")]

remoteLookup = AttributeDef RemoteLookup $
    let parent = Parent <?> nil
        superclass = Superclass <?> nil
        block = Child <?> (int 2)
    in
      guard [(hasKind === "Block", ifOK (Node <.> LocalLookup <?> (Arg))
                                   (IfElse (isUnknown (Node <.> parent <.> superclass)) "UnknownDecl"
                                     (ifOK (Node <.> parent <.> superclass <.> block <.> RemoteLookup <?> (Arg)) "UnknownDecl"))),
             (otherwise, "UnknownDecl")]

superclassAttr = AttributeDef Superclass $
  let use = Child <?> (int 1)
      kind = Kind <?> (nil)
      decl = Decl <?> (nil)
  in
    guard [(hasKind === "ClassDecl" <&&> not (isUnknown (Node <.> use)), IfElse (Node <.> use <.> decl <.> kind === "ClassDecl") (Node <.> use <.> decl) "UknownClass"),
           (otherwise, "UnknownClass")]

typeAttr = AttributeDef Type $
  let decl = Decl <?> (nil)
      access = Child <?> (int 0)
      kind = Kind <?> (nil)
  in
    guard [(Node <.> kind === "ClassDecl", Node),
           (Node <.> kind === "VarDecl", IfElse (Node <.> access <.> decl <.> kind === "ClassDecl") (Node <.> access <.> decl) ("UnknownClass")),
           (otherwise, "UnknownClass")]
