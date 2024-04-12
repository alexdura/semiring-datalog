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
                  deriving (Eq, Show)

hasKind :: Expr PicoJavaAttr
hasKind = Node <.> Kind <?> nil

predefs = Func "predefs" Nil

-- attribute definitions - Figure 15
declAttr = AttributeDef Decl $
  let use = Child <?> (int 1)
      name = Name <?> nil
      decl = Decl <?> nil
  in
    guard [(hasKind === (SVal "Dot"), Node <.> use <.> decl), -- L1
           (hasKind === (SVal "Use"), Node <.> Lookup <?> (Node <.> name)), -- L2
           (otherwise, mkUnknown)] -- L3


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
            (isUnknown Node, mkUnknown), -- L7
            (otherwise, Node <.> parent <.> Lookup <?> (Arg))] -- L8


localLookupAttr = AttributeDef LocalLookup $
  let items = Child <?> (int 0)
  in
    guard [(hasKind === "Program", ifOK (Func "finddecl" Arg <:> (Node <.> items) <:> Nil)
                                   (Func "finddecl" Arg <:> predefs <:> Nil)),
           (hasKind === "Block", Func "finddecl" Arg <:> (Node <.> items) <:> Nil),
           (otherwise, mkUnknown)]


remoteLookup = AttributeDef RemoteLookup $
    let parent = Parent <?> nil
        superclass = Superclass <?> nil
        block = Child <?> (int 2)
    in
      guard [(hasKind === "Block", ifOK (Node <.> LocalLookup <?> (Arg))
                                   (IfElse (isUnknown (Node <.> parent <.> superclass)) mkUnknown
                                     (ifOK (Node <.> parent <.> superclass <.> block <.> RemoteLookup <?> (Arg)) mkUnknown))),
             (otherwise, mkUnknown)]


superclassAttr = AttributeDef Superclass $
  let use = Child <?> (int 1)
      kind = Kind <?> (nil)
      decl = Decl <?> (nil)
  in
    guard [(hasKind === "ClassDecl" <&&> not (isUnknown (Node <.> use)), IfElse (Node <.> use <.> decl <.> kind === "ClassDecl") (Node <.> use <.> decl) mkUnknown),
           (otherwise, mkUnknown)]


typeAttr = AttributeDef Type $
  let decl = Decl <?> (nil)
      access = Child <?> (int 0)
      kind = Kind <?> (nil)
  in
    guard [(Node <.> kind === "ClassDecl", Node),
           (Node <.> kind === "VarDecl", IfElse (Node <.> access <.> decl <.> kind === "ClassDecl") (Node <.> access <.> decl) mkUnknown),
           (otherwise, mkUnknown)]

picoJavaAttrLookup :: PicoJavaAttr -> AttributeDef PicoJavaAttr
picoJavaAttrLookup attr = case attr of
  Decl -> declAttr
  Type -> typeAttr
  Lookup -> lookupAttr
  RemoteLookup -> remoteLookup
  LocalLookup -> localLookupAttr
  Superclass -> superclassAttr
  ua -> error $ "Missing attribute definition for " ++ show ua


type PicoJavaAST = AST (String, Int)

unknown = AST ("unknown", 0) "_unknown_" []
boolDecl = AST ("Class", -1) "bool" [AST ("Block", -2) "" []]

mkUnknown = Func "mkUnknown" Nil

picoJavaBuiltinAttrLookup :: PicoJavaAST
  -> PicoJavaAttr
  -> Maybe (PicoJavaAST -> Domain (String, Int) -> Domain (String, Int))

picoJavaBuiltinAttrLookup ast attr =
  let pm = parentMap ast in
    case attr of
      Parent -> Just (\n -> let p = Map.lookup n pm in
                              if isJust p then const (DNode $ fromJust p)
                              else const $ DNode unknown)
      Child -> Just (\n (DInt i) -> DNode $ n.children !! i)
      Kind -> Just (\n -> const $ DString $ fst n.kind)
      Name -> Just (\n -> const $ DString $ n.token)
      Children -> Just (\n -> const $ DList (DNode <$> n.children))
      _ -> Nothing

picoJavaBuiltinFunc ::  String -> Maybe (Domain (String, Int) -> Domain (String, Int))
picoJavaBuiltinFunc name = case name of
  "finddecl" -> Just (\(DList [e, DList l]) -> if elem e l then e
                                               else DNode unknown)
  "predefs" -> Just (\_ -> DList [DNode boolDecl])
  "isUnknown" -> Just (\(DNode n) -> DBool $ n.token == "_unknown_")
  "mkUnknown" -> Just (\_ -> DNode unknown)
  _ -> Nothing
