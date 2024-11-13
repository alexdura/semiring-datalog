module SaigaPicoJavaSpec (saigaPicoJavaTests, program3Ast) where

import PicoJava
import Saiga
import SaigaPicoJava
    ( picoJavaProgram,
      PicoJavaAttr(Type, Parent, IsUnknown, LocalLookup, Decl, Lookup,
                   Name),
      unknownDecl )
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit

import Util ( findNodeByPath, nodeId, evalExpr, evalExpr1, parseAndNumber )

saigaPicoJavaTests = testGroup "Saiga attributes for PicoJava tests" [
  testCase "Parse program 1" $ Util.parseAndNumber program1 @?=
    AST {kind = ("Program",7), token = "", children = [
            AST {kind = ("ClassDecl",6), token = "A", children = [
                    AST {kind = ("UnknownClass",0), token = "_unknown_", children = []},
                    AST {kind = ("Block",5), token = "", children = [
                            AST {kind = ("VarDecl",2), token = "c", children = [
                                    AST {kind = ("Use",1), token = "boolean", children = []}]},
                            AST {kind = ("VarDecl",4), token = "next", children = [
                                    AST {kind = ("Use",3), token = "A", children = []}]}]}]}]},

  testCase "Compute parent attribute 1" $
    let ast = Util.parseAndNumber program1
        path = [0, 1, 0, 0] in
      (nodeId $ evalExpr picoJavaProgram ast path Node) @?= 1,


  testCase "Compute parent attribute 2" $
    let ast = Util.parseAndNumber program1
        path = [0, 1, 0, 0]
        expr = Node <.> Parent <?> []
    in
      (nodeId $ evalExpr picoJavaProgram ast path expr) @?= 2,


  testCase "Compute parent attribute 3" $
    let ast = parseAndNumber program1
        path = [0, 1, 0, 0]
        parent = Parent <?> []
        isUnknown = IsUnknown <?> []
        expr = Node <.> parent <.> parent <.> parent <.> parent <.> parent <.> isUnknown
    in
      evalExpr picoJavaProgram ast path expr @?= DBool True,

  testCase "Compute parent attribute 4" $
    let ast = parseAndNumber program1
        path = [0, 1, 0, 0]
        parent = Parent <?> []
        isUnknown = IsUnknown <?> []
        expr = Node <.> parent <.> parent <.> parent <.> parent <.> isUnknown
    in
      evalExpr picoJavaProgram ast path expr @?= DBool False,

  testCase "Parse program 2" $ parseAndNumber program2 @?= program2Ast,

  testCase "Local lookup 1" $
    let ast = parseAndNumber program2
        path = []
        expr = Node <.> LocalLookup <?> [SVal "A"]
    in
      evalExpr picoJavaProgram ast path expr @?= (DNode $ findNodeByPath ast [0]),

  testCase "Local lookup 2" $
    let ast = parseAndNumber program2
        path = []
        expr = Node <.> LocalLookup <?> [SVal "C"]
    in
      evalExpr picoJavaProgram ast path expr @?= DNode unknownDecl,


  testCase "Parse program 3" $ parseAndNumber program3 @?= program3Ast,

  testCase "Lookup_1" $
    let expr = Node <.> Lookup <?> [Node <.> Name <?> []] in
      evalExpr1 picoJavaProgram program3Ast 6 expr @?= DNode
      AST {kind = ("VarDecl",2), token = "x", children = [
              AST {kind = ("Use",1), token = "bool", children = []}]},

  testCase "Lookup_2" $
    let expr = Node <.> Decl <?> [] in
      (nodeId $ evalExpr1 picoJavaProgram program3Ast 15 expr) @?= 5,

  testCase "Parse program 4" $ parseAndNumber program4 @?= program4Ast,

  testCase "Lookup_3" $
    let expr = Node <.> Lookup <?> [Node <.> Name <?> []] in
      evalExpr1 picoJavaProgram program4Ast 3 expr @?= DNode
      AST {kind = ("VarDecl",2), token = "x", children = [
              AST {kind = ("Use",1), token = "A", children = []}]},

  testCase "Type_1" $
    let expr = Node <.> Lookup <?> [Node <.> Name <?> []] <.> Type <?> [] in
      (nodeId $ evalExpr1 picoJavaProgram program4Ast 3 expr) @?= 7
  ]



program1 = "class A { \n\
  \ boolean c; \n\
  \ A next; \n\
  \}"


program2 = "class A {} \n\
           \class B {}"

program2Ast =
  AST {kind = ("Program",6), token = "", children = [
            AST {kind = ("ClassDecl",2), token = "A", children = [
                    AST {kind = ("UnknownClass",0), token = "_unknown_", children = []},
                    AST {kind = ("Block",1), token = "", children = []}]},
            AST {kind = ("ClassDecl",5), token = "B", children = [
                    AST {kind = ("UnknownClass",3), token = "_unknown_", children = []},
                    AST {kind = ("Block",4), token = "", children = []}]}]}

program3 = "class A { \n\
           \  bool x; \n\
           \  class B { \n\
           \    bool y; \n\
           \    x = y; \n\
           \  } \n\
           \  B z; \n\
           \  z.y = false;}"

program3Ast =
  AST {kind = ("Program",20), token = "", children = [
          AST {kind = ("ClassDecl",19), token = "A", children = [
                  AST {kind = ("UnknownClass",0), token = "_unknown_", children = []},
                  AST {kind = ("Block",18), token = "", children = [
                          AST {kind = ("VarDecl",2), token = "x", children = [
                                  AST {kind = ("Use",1), token = "bool", children = []}]},
                          AST {kind = ("ClassDecl",10), token = "B", children = [
                                  AST {kind = ("UnknownClass",3), token = "_unknown_", children = []},
                                  AST {kind = ("Block",9), token = "", children = [
                                          AST {kind = ("VarDecl",5), token = "y", children = [
                                                  AST {kind = ("Use",4), token = "bool", children = []}]},
                                          AST {kind = ("Stmt",8), token = "", children = [
                                                  AST {kind = ("Use",6), token = "x", children = []},
                                                  AST {kind = ("Use",7), token = "y", children = []}]}]}]},
                          AST {kind = ("VarDecl",12), token = "z", children = [
                                  AST {kind = ("Use",11), token = "B", children = []}]},
                          AST {kind = ("Stmt",17), token = "", children = [
                                  AST {kind = ("Dot",15), token = ".", children = [
                                          AST {kind = ("Use",13), token = "z", children = []},
                                          AST {kind = ("Use",14), token = "y", children = []}]},
                                  AST {kind = ("BoolLit",16), token = "false", children = []}]}]}]}]}

program4 = "class A { \n\
           \ A x; \n\
           \ x = x; \n\
           \}"

program4Ast =
  AST {kind = ("Program",8), token = "", children = [
          AST {kind = ("ClassDecl",7), token = "A", children = [
                  AST {kind = ("UnknownClass",0), token = "_unknown_", children = []},
                  AST {kind = ("Block",6), token = "", children = [
                          AST {kind = ("VarDecl",2), token = "x", children = [
                                  AST {kind = ("Use",1), token = "A", children = []}]},
                          AST {kind = ("Stmt",5), token = "", children = [
                                  AST {kind = ("Use",3), token = "x", children = []},
                                  AST {kind = ("Use",4), token = "x", children = []}]}]}]}]}
