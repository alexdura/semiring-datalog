{-# LANGUAGE OverloadedStrings #-}

module SaigaPicoJava where

import Saiga
import PicoJava
import Prelude hiding (otherwise, not)
import Data.Maybe
import qualified Data.Map.Strict as Map

-- attribute declarations
data PicoJavaAttr = Decl
                  | LocalLookup
                  | Lookup
                  | RemoteLookup
                  | Kind
                  | Child
                  | Children
                  | Parent
                  | Name
                  | Superclass
                  | Type
                  deriving (Eq, Show, Enum)

instance SaigaAttribute PicoJavaAttr

hasKind :: Expr PicoJavaAttr
hasKind = Node <.> Kind <?> []

predefs = Func "predefs" []

-- attribute definitions - Figure 15
declAttr = Attribute Decl 0 $
  let use = Child <?> [int 1]
      name = Name <?> []
      decl = Decl <?> []
  in
    guard [(hasKind === (SVal "Dot"), Node <.> use <.> decl), -- L1
           (hasKind === (SVal "Use"), Node <.> Lookup <?> [Node <.> name]), -- L2
           (otherwise, mkUnknownDecl)] -- L3


lookupAttr = Attribute Lookup 1 $
  let parent = Parent <?> []
      superclass = Superclass <?> []
      block = Child <?> [int 1]
      decl = Decl <?> []
      typ = Type <?> []
      kind = Kind <?> []
      access = Child <?> [int 0]
      rhs = Child <?> [int 1]
  in
    guard [(hasKind === "Program", Node <.> LocalLookup <?> [Arg 0]), -- L4
            (hasKind === "Block", ifOK (Node <.> LocalLookup <?> [Arg 0]) -- L5
                                  (ifOK (Node <.> parent <.> superclass <.> block <.> RemoteLookup <?> [Arg 0])
                                   Node <.> parent <.> Lookup <?> [Arg 0])),
            (hasKind === "Use" <&&>
             (Node <.> parent <.> kind === "Dot") <&&>
             (Node <.> parent <.> rhs === Node), Node <.> parent <.> access <.> decl <.> typ <.> block <.> RemoteLookup <?> [Arg 0]), -- L6
            (isUnknown [Node], mkUnknownDecl), -- L7
            (otherwise, Node <.> parent <.> Lookup <?> [Arg 0])] -- L8


localLookupAttr = Attribute LocalLookup 1 $
  let items = Children <?> []
  in
    guard [(hasKind === "Program", ifOK (Func "finddecl" [Arg 0, Node <.> items])
                                   (Func "finddecl" [Arg 0, predefs])),
           (hasKind === "Block", Func "finddecl" [Arg 0, Node <.> items]),
           (otherwise, mkUnknownDecl)]


remoteLookup = Attribute RemoteLookup 1 $
    let parent = Parent <?> []
        superclass = Superclass <?> []
        block = Child <?> [int 1]
    in
      guard [(hasKind === "Block", ifOK (Node <.> LocalLookup <?> [Arg 0])
                                   (IfElse (isUnknown [Node <.> parent <.> superclass]) mkUnknownDecl
                                     (ifOK (Node <.> parent <.> superclass <.> block <.> RemoteLookup <?> [Arg 0]) mkUnknownDecl))),
             (otherwise, mkUnknownDecl)]


superclassAttr = Attribute Superclass 0 $
  let use = Child <?> [int 1]
      kind = Kind <?> []
      decl = Decl <?> []
  in
    guard [(hasKind === "ClassDecl" <&&> not (isUnknown [Node <.> use]), IfElse (Node <.> use <.> decl <.> kind === "ClassDecl") (Node <.> use <.> decl) mkUnknownClass),
           (otherwise, mkUnknownClass)]


typeAttr = Attribute Type 0 $
  let decl = Decl <?> []
      access = Child <?> [int 0]
      kind = Kind <?> [nil]
  in
    guard [(Node <.> kind === "ClassDecl", Node),
           (Node <.> kind === "VarDecl", IfElse (Node <.> access <.> decl <.> kind === "ClassDecl") (Node <.> access <.> decl) mkUnknownClass),
           (otherwise, mkUnknownClass)]

-- picoJavaAttrLookup :: PicoJavaAttr -> AttributeDef PicoJavaAttr
-- picoJavaAttrLookup attr = case attr of
--   Decl -> declAttr
--   Type -> typeAttr
--   Lookup -> lookupAttr
--   RemoteLookup -> remoteLookup
--   LocalLookup -> localLookupAttr
--   Superclass -> superclassAttr
--   ua -> error $ "Missing attribute definition for " ++ show ua


type PicoJavaAST = AST (String, Int)

unknownDecl :: AST (String, Int)
unknownDecl = AST ("unknown", 0) "_unknown_" []

unknownClass :: AST (String, Int)
unknownClass = AST ("ClassDecl", -3) "_unknown_" [unknownDecl, AST ("Block", -4) "" []]

boolDecl :: AST (String, Int)
boolDecl = AST ("ClassDecl", -1) "bool" [AST ("Block", -2) "" []]

mkUnknownDecl = Func "mkUnknownDecl" []
mkUnknownClass = Func "mkUnknownClass" []


findDeclExpr :: Expr PicoJavaAttr
findDeclExpr =
  let arg0 = Arg 0
      arg1 = Arg 1
  in IfElse (arg1 === Nil) mkUnknownDecl
     (IfElse (arg0 === ((Head arg1) <.> Name <?> [])) (Head arg1) (Func "finddecl" [arg0, Tail arg1]))

picoJavaProgram ast =
  let pm = parentMap ast in [
    declAttr,
    typeAttr,
    lookupAttr,
    remoteLookup,
    localLookupAttr,
    superclassAttr,

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

    Function "finddecl" 2 findDeclExpr,

    BuiltinFunction "predefs" 0 $
      \_ -> DList [DNode boolDecl],

    BuiltinFunction "isUnknown" 1 $
      \case
        [DNode n] -> DBool $ n.token == "_unknown_"
        e -> error $ "Unexpected argument " ++ show e,

    BuiltinFunction "mkUnknownDecl" 0 $
      const $ DNode unknownDecl,

    BuiltinFunction "mkUnknownClass" 0 $
      const $ DNode unknownClass,

    BuiltinFunction "eq" 2 $
      \[x, y] -> DBool $ x == y
    ]
