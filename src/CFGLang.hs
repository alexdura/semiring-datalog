{-# LANGUAGE OverloadedStrings #-}

module CFGLang where

import AST
import Saiga
import Prelude hiding (otherwise)
import Control.Exception (PatternMatchFail)



terminal s = AST "Terminal" s []

nUse s = AST "NUse" s []

nonEmptySymbolList h t = AST "NonEmptySymbolList" "" [h, t]

emptySymbolList = AST "EmptySymbolList" "" []

nonEmptyProdList h t = AST "NonEmptyProdList" "" [h, t]

emptyProdList = AST "EmptyProdList" "" []

nDecl s = AST "NDecl" s []

rule d ps = AST "Rule" "" [d, ps]

cfg = AST "CFG" ""

data CFGAttr = Child
             | Children
             | Parent
             | Name
             | Kind
             | First
             | Follow
             | Nullable
             deriving (Eq, Show, Enum)

instance SaigaAttribute CFGAttr


firstAttr :: SaigaElement CFGAttr a
firstAttr =  Attribute First 0 $
  let kind = Kind <?> []
      first  = First <?> []
  in guard [
    (
      Node <.> kind === "EmptySymbolList",
      Nil
    ),
    (
      Node <.> kind === "NonEmptySymbolList",
      let symbol = Child <?> [int 0]
          symbolList = Child <?> [int 1]
          nullable = Nullable <?> []
      in
        IfElse (Node <.> symbol <.> nullable)
        (Func "listUnion" [Node <.> symbol <.> first, Node <.> symbolList <.> first])
        (Node <.> symbol <.> first)
    ),
    (
      Node <.> kind === "NonEmptyProdList",
      let prod = Child <?> [int 0]
          prodList = Child <?> [int 1]
      in
        Func "listUnion" [Node <.> prod <.> first, Node <.> prodList <.> first]
    ),
    (
      Node <.> kind === "EmptyProdList",
      Nil
    ),
    (otherwise, error "First attribute not defined for this node.")
    ]


nullableAttr :: SaigaElement CFGAttr a
nullableAttr = Attribute Nullable 0 $
  let kind = Kind <?> []
      nullable  = Nullable <?> []
  in guard [
    (
      Node <.> kind === "EmptyProdList",
      BVal False
    ),
    (
      Node <.> kind === "NonEmptyProdList",
      let prod = Child <?> [int 0]
          prodList = Child <?> [int 1]
      in
        Node <.> prod <.> nullable <||> Node <.> prodList <.> nullable
    ),
    (
      Node <.> kind === "EmptySymbolList",
      BVal True
    ),
    (
      Node <.> kind === "NonEmptySymbolList",
      let symbol = Child <?> [int 0]
          symbolList = Child <?> [int 1]
      in
        Node <.> symbol <.> nullable <&&> Node <.> symbolList <.> nullable
    ),
    (otherwise, error "Nullable attribute not defined for this node.")
    ]


isElemOfExpr :: Expr attr
isElemOfExpr =
  let e = Arg 0
      l = Arg 1
  in
    IfEq l Nil (BVal False) (
    -- l is not empty
    let h = Head l
        t = Tail l
    in IfEq l h (BVal True) (Func "isElemOf" [e, t]))


listUnionExpr :: Expr attr
listUnionExpr =
  let l1 = Arg 0
      l2 = Arg 1
  in
    IfEq l1 Nil l2 (IfEq l2 Nil l1 (
                       -- l1 and l2 are not nil
                       let h1 = Head l1
                           t1 = Tail l1
                       in
                         IfElse (Func "isElemOf" [h1, l2])
                         (Func "listUnion" [t1, l2])
                         (Func "listUnion" [t1, h1 <:> l2])))
