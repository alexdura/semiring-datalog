{-# LANGUAGE OverloadedStrings #-}

module CFGLang where

import AST
import Saiga
import Prelude hiding (otherwise)
import qualified Data.Map.Strict as Map
import Data.Maybe



terminal s = AST "Terminal" s []

nUse s = AST "NUse" s []

nonEmptySymbolList h t = AST "NonEmptySymbolList" "" [h, t]

emptySymbolList = AST "EmptySymbolList" "" []

nonEmptyProdList h t = AST "NonEmptyProdList" "" [h, t]

emptyProdList = AST "EmptyProdList" "" []

prods :: [AST String] -> AST String
prods = foldr nonEmptyProdList emptyProdList

symbols :: [AST String] -> AST String
symbols = foldr nonEmptySymbolList emptySymbolList


nDecl s = AST "NDecl" s []

rule d ps = AST "Rule" "" [d, ps]

cfg = AST "CFG" ""

data CFGAttr = Child
             | Children
             | Parent
             | Name
             | Kind
             | First
             | Decl
             | FindDecl
            -- | Follow
             | Nullable
             deriving (Eq, Show, Enum)

instance SaigaAttribute CFGAttr


firstAttr :: SaigaElement CFGAttr a
firstAttr =  Attribute First 0 $
  let kind = Kind <?> []
      first  = First <?> []
      decl = Decl <?> []
  in guard [
    (
      Node <.> kind === "NUse",
      Node <.> decl <.> first
    ),
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
    (otherwise, SVal "Missing case")
    ]


nullableAttr :: SaigaElement CFGAttr a
nullableAttr = CircularAttribute Nullable 0 (
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
    (
      Node <.> kind === "NDecl",
      let parent = Parent <?> []
      in
        Node <.> parent <.> nullable
    ),
    (
      Node <.> kind === "Rule",
      let prodList = Child <?> [int 1]
      in
        Node <.> prodList <.> nullable
    ),
    (
      Node <.> kind === "NUse",
      let decl = Decl <?> []
      in
        Node <.> decl <.> nullable
    ),
    (
      Node <.> kind === "Terminal",
      BVal False
    ),
    (otherwise, SVal "Missing case")
    ])
  (BVal False) "NotImplemented"

findDeclAttr :: SaigaElement CFGAttr a
findDeclAttr = Attribute FindDecl 1 $
  let kind = Kind <?> []
      name = Name <?> []
      children = Children <?> []
      parent = Parent <?> []
  in guard [
    (
      Node <.> kind === "CFG",
      Func "findDeclInList" [Arg 0, Node <.> children]
    ),
    (
      otherwise,
      Node <.> parent <.> FindDecl <?> [Arg 0]
    )
    ]

declAttr :: SaigaElement CFGAttr a
declAttr = Attribute Decl 0 $
  let kind = Kind <?> []
      name = Name <?> []
  in guard [
    (
      Node <.> kind === "NUse",
      Node <.> FindDecl <?> [Node <.> name]
    ),
    (
      otherwise,
      mkUnknownDecl
    )
    ]
unknownDecl :: AST (String, Int)
unknownDecl = AST ("unknown", -1) "_unknown_" []

mkUnknownDecl = Func "mkUnknownDecl" []

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

findDeclInList :: Expr CFGAttr
findDeclInList =
  let n = Arg 0
      l = Arg 1
      h = Head l
  in
    IfEq l Nil
    mkUnknownDecl
    (IfEq n (h <.> Child <?> [IVal 0] <.> Name <?> [])
      h
      (Func "findDeclInList" [n, Tail l])
    )


cfgProgram :: AST (String, Int) -> SaigaProgram CFGAttr (String, Int)

cfgProgram ast =
  let pm = parentMap ast in [
    -- attributes
    nullableAttr,
    firstAttr,
    findDeclAttr,
    declAttr,


    -- builtin attributes
    BuiltinAttribute Parent 0 $
      \(DNode n) -> let p = Map.lookup n pm in
                      if isJust p then const (DNode $ fromJust p)
                      else const $ DNode unknownDecl,

    BuiltinAttribute Child 1 $
      \(DNode n) [DInt i] -> if i >= (length n.children)
                     then error $ "Index " ++ show i ++ " for node " ++ show n
                     else DNode $ n.children !! i,

    BuiltinAttribute Kind 0 $
      \(DNode n) -> const $ DString $ fst n.kind,

    BuiltinAttribute Name 0 $
      \(DNode n) -> const $ DString $ n.token,

    BuiltinAttribute Children 0 $
      \(DNode n) -> const $ DList (DNode <$> n.children),

    -- functions
    Function "isElemOf" 2 isElemOfExpr,

    Function "listUnion" 2 listUnionExpr,

    Function "findDeclInList" 2 findDeclInList,

    -- built-in functions
    BuiltinFunction "mkUnknownDecl" 0 $
      const $ DNode unknownDecl
    ]
