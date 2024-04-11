{-# LANGUAGE OverloadedStrings #-}

module SaigaPicoJava where

import Saiga
import Prelude hiding (otherwise, not)

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
