{-# LANGUAGE OverloadedStrings #-}

module CFGLang where

import AST
import Saiga
import Prelude hiding (otherwise)



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
  let parent = Parent <?> []
      kind = Kind <?> []
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
        (Func "union" [Node <.> symbol <.> first, Node <.> symbolList <.> first])
        (Node <.> symbol <.> first)
    ),
    (otherwise, Nil)
    ]
