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
hasKind = Node <.> Kind <?> nil

predefs = Func "predefs" Nil

-- attribute definitions - Figure 15
declAttr = Attribute Decl $
  let use = Child <?> (int 1)
      name = Name <?> nil
      decl = Decl <?> nil
  in
    guard [(hasKind === (SVal "Dot"), Node <.> use <.> decl), -- L1
           (hasKind === (SVal "Use"), Node <.> Lookup <?> (Node <.> name)), -- L2
           (otherwise, mkUnknownDecl)] -- L3


lookupAttr = Attribute Lookup $
  let parent = Parent <?> nil
      superclass = Superclass <?> nil
      block = Child <?> (int 1)
      decl = Decl <?> nil
      typ = Type <?> nil
      kind = Kind <?> nil
      access = Child <?> (int 0)
      rhs = Child <?> (int 1)
  in
    guard [(hasKind === "Program", Node <.> LocalLookup <?> Arg), -- L4
            (hasKind === "Block", ifOK (Node <.> LocalLookup <?> (Arg)) -- L5
                                  (ifOK (Node <.> parent <.> superclass <.> block <.> RemoteLookup <?> (Arg))
                                   Node <.> parent <.> Lookup <?> (Arg))),
            (hasKind === "Use" <&&>
             (Node <.> parent <.> kind === "Dot") <&&>
             (Node <.> parent <.> rhs === Node), Node <.> parent <.> access <.> decl <.> typ <.> block <.> RemoteLookup <?> (Arg)), -- L6
            (isUnknown Node, mkUnknownDecl), -- L7
            (otherwise, Node <.> parent <.> Lookup <?> (Arg))] -- L8


localLookupAttr = Attribute LocalLookup $
  let items = Children <?> (Nil)
  in
    guard [(hasKind === "Program", ifOK (Func "finddecl" (Arg <:> (Node <.> items) <:> Nil))
                                   (Func "finddecl" (Arg <:> predefs <:> Nil))),
           (hasKind === "Block", Func "finddecl" (Arg <:> (Node <.> items) <:> Nil)),
           (otherwise, mkUnknownDecl)]


remoteLookup = Attribute RemoteLookup $
    let parent = Parent <?> nil
        superclass = Superclass <?> nil
        block = Child <?> (int 1)
    in
      guard [(hasKind === "Block", ifOK (Node <.> LocalLookup <?> (Arg))
                                   (IfElse (isUnknown (Node <.> parent <.> superclass)) mkUnknownDecl
                                     (ifOK (Node <.> parent <.> superclass <.> block <.> RemoteLookup <?> (Arg)) mkUnknownDecl))),
             (otherwise, mkUnknownDecl)]


superclassAttr = Attribute Superclass $
  let use = Child <?> (int 1)
      kind = Kind <?> (nil)
      decl = Decl <?> (nil)
  in
    guard [(hasKind === "ClassDecl" <&&> not (isUnknown (Node <.> use)), IfElse (Node <.> use <.> decl <.> kind === "ClassDecl") (Node <.> use <.> decl) mkUnknownClass),
           (otherwise, mkUnknownClass)]


typeAttr = Attribute Type $
  let decl = Decl <?> (nil)
      access = Child <?> (int 0)
      kind = Kind <?> (nil)
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

mkUnknownDecl = Func "mkUnknownDecl" Nil
mkUnknownClass = Func "mkUnknownClass" Nil

-- picoJavaBuiltinAttrLookup :: PicoJavaAST
--   -> PicoJavaAttr
--   -> Maybe (PicoJavaAST -> Domain (String, Int) -> Domain (String, Int))

-- picoJavaBuiltinAttrLookup ast attr =
--   let pm = parentMap ast in
--     case attr of
--       Parent -> Just (\n -> let p = Map.lookup n pm in
--                               if isJust p then const (DNode $ fromJust p)
--                               else const $ DNode unknownDecl)
--       Child -> Just (\n (DInt i) -> if i >= (length n.children)
--                                     then error $ "Index " ++ show i ++ " for node " ++ show n
--                                     else DNode $ n.children !! i)
--       Kind -> Just (\n -> const $ DString $ fst n.kind)
--       Name -> Just (\n -> const $ DString $ n.token)
--       Children -> Just (\n -> const $ DList (DNode <$> n.children))
--       _ -> Nothing


findDeclExpr :: Expr PicoJavaAttr
findDeclExpr =
  let arg0 = Head Arg
      arg1 = Head (Tail Arg)
  in IfElse (arg1 === Nil) mkUnknownDecl
     (IfElse (arg0 === ((Head arg1) <.> Name <?> Nil)) (Head arg1) (Func "finddecl" (arg0 <:> (Tail arg1) <:> Nil)))

-- picoJavaFunc :: String -> Maybe (Expr PicoJavaAttr)
-- picoJavaFunc name = case name of
--   "finddecl" -> Just findDeclExpr
--   _ -> Nothing

-- picoJavaBuiltinFunc ::  String -> Maybe (Domain (String, Int) -> Domain (String, Int))
-- picoJavaBuiltinFunc name = case name of
--   "predefs" -> Just (\_ -> DList [DNode boolDecl])
--   "isUnknown" -> Just $ \case (DNode n) -> DBool $ n.token == "_unknown_"
--                               e -> error $ "Unexpected argument " ++ show e
--   "mkUnknownDecl" -> Just $ const $ DNode unknownDecl
--   "mkUnknownClass" -> Just $ const $ DNode unknownClass
--   "eq" -> Just (\(DList [x, y]) -> DBool $ x == y)
--   _ -> Nothing


picoJavaProgram ast =
  let pm = parentMap ast in [
    declAttr,
    typeAttr,
    lookupAttr,
    remoteLookup,
    localLookupAttr,
    superclassAttr,

    BuiltinAttribute Parent $
      \(DNode n) -> let p = Map.lookup n pm in
                      if isJust p then const (DNode $ fromJust p)
                      else const $ DNode unknownDecl,

    BuiltinAttribute Child $
      \(DNode n) (DInt i) -> if i >= (length n.children)
                     then error $ "Index " ++ show i ++ " for node " ++ show n
                     else DNode $ n.children !! i,

    BuiltinAttribute Kind $
      \(DNode n) -> const $ DString $ fst n.kind,

    BuiltinAttribute Name $
      \(DNode n) -> const $ DString $ n.token,

    BuiltinAttribute Children $
      \(DNode n) -> const $ DList (DNode <$> n.children),

    Function "finddecl" findDeclExpr,

    BuiltinFunction "predefs" $
      \_ -> DList [DNode boolDecl],

    BuiltinFunction "isUnknown" $
      \case
        (DNode n) -> DBool $ n.token == "_unknown_"
        e -> error $ "Unexpected argument " ++ show e,

    BuiltinFunction "mkUnknownDecl" $
      const $ DNode unknownDecl,

    BuiltinFunction "mkUnknownClass" $
      const $ DNode unknownClass,

    BuiltinFunction "eq" $
      \(DList [x, y]) -> DBool $ x == y
    ]
