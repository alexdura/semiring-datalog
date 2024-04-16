module SaigaPicoJavaSpec (saigaPicoJavaTests) where

import PicoJava
import Saiga
import SaigaPicoJava
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import Data.Maybe
import Data.List
import Control.Monad

saigaPicoJavaTests = testGroup "Saiga attributes for PicoJava tests" [
  testCase "Parse program 1" $ parseAndNumber program1 @?=
    AST {kind = ("Program",7), token = "", children = [
            AST {kind = ("Class",6), token = "A", children = [
                    AST {kind = ("UnknownClass",0), token = "_unknown_", children = []},
                    AST {kind = ("Block",5), token = "", children = [
                            AST {kind = ("VarDecl",2), token = "c", children = [
                                    AST {kind = ("Use",1), token = "boolean", children = []}]},
                            AST {kind = ("VarDecl",4), token = "next", children = [
                                    AST {kind = ("Use",3), token = "A", children = []}]}]}]}]},

  testCase "Compute parent attribute 1" $
    let ast = parseAndNumber program1
        path = [0, 1, 0, 0] in
      (nodeId $ evalExpr ast path Node) @?= 1,


  testCase "Compute parent attribute 2" $
    let ast = parseAndNumber program1
        path = [0, 1, 0, 0]
        expr = Node <.> Parent <?> (Nil)
    in
      (nodeId $ evalExpr ast path expr) @?= 2,


  testCase "Compute parent attribute 3" $
    let ast = parseAndNumber program1
        path = [0, 1, 0, 0]
        parent = Parent <?> Nil
        expr = Func "isUnknown" (Node <.> parent <.> parent <.> parent <.> parent <.> parent)
    in
      evalExpr ast path expr @?= DBool True,

  testCase "Compute parent attribute 4" $
    let ast = parseAndNumber program1
        path = [0, 1, 0, 0]
        parent = Parent <?> Nil
        expr = Func "isUnknown" (Node <.> parent <.> parent <.> parent <.> parent)
    in
      evalExpr ast path expr @?= DBool False,

  testCase "Parse program 2" $ parseAndNumber program2 @?= program2Ast,

  testCase "Local lookup 1" $
    let ast = parseAndNumber program2
        path = []
        expr = Node <.> LocalLookup <?> (SVal "A")
    in
      evalExpr ast path expr @?= (DNode $ findNodeByPath ast [0]),

  testCase "Local lookup 2" $
    let ast = parseAndNumber program2
        path = []
        expr = Node <.> LocalLookup <?> (SVal "C")
    in
      evalExpr ast path expr @?= DNode unknown,


  testCase "Parse program 3" $ parseAndNumber program3 @?= program3Ast,

  testCase "Type 1" $
    let expr = Node <.> Lookup <?> (Node <.> Name <?> Nil) in
      evalExpr1 program3Ast 6 expr @?= DNode unknown
  ]


evalExpr :: PicoJavaAST -> [Int] -> Expr PicoJavaAttr -> Domain (String, Int)

findNodeByPath ast [] = ast
findNodeByPath ast (n:ns) = findNodeByPath ((children ast) !! n) ns

findNodeById ast nid = if (snd $ kind ast) == nid then Just ast
                       else join $ find isJust ((\ast' -> findNodeById ast' nid) <$> ast.children)

nodeId (DNode (AST (_, i) _ _)) = i

evalExpr ast path expr =
  let n = findNodeByPath ast path
      ctx = AttributeCtx picoJavaAttrLookup (picoJavaBuiltinAttrLookup ast) picoJavaFunc picoJavaBuiltinFunc
  in eval ctx (DList []) n expr

evalExpr1 ast nid expr =
  let n = fromJust $ findNodeById ast nid
      ctx = AttributeCtx picoJavaAttrLookup (picoJavaBuiltinAttrLookup ast) picoJavaFunc picoJavaBuiltinFunc
  in eval ctx (DList []) n expr

parseAndNumber s = case parseProgram s >>= (return . numberNodes) of
  Left e -> error $ "Error parsing program: " ++ show e
  Right ast -> ast


program1 = "class A { \n\
  \ boolean c; \n\
  \ A next; \n\
  \}"


program2 = "class A {} \n\
           \class B {}"

program2Ast =
  AST {kind = ("Program",6), token = "", children = [
            AST {kind = ("Class",2), token = "A", children = [
                    AST {kind = ("UnknownClass",0), token = "_unknown_", children = []},
                    AST {kind = ("Block",1), token = "", children = []}]},
            AST {kind = ("Class",5), token = "B", children = [
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
          AST {kind = ("Class",19), token = "A", children = [
                  AST {kind = ("UnknownClass",0), token = "_unknown_", children = []},
                  AST {kind = ("Block",18), token = "", children = [
                          AST {kind = ("VarDecl",2), token = "x", children = [
                                  AST {kind = ("Use",1), token = "bool", children = []}]},
                          AST {kind = ("Class",10), token = "B", children = [
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
