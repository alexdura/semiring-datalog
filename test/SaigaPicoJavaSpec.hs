module SaigaPicoJavaSpec (saigaPicoJavaTests) where

import PicoJava
import Saiga
import SaigaPicoJava
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit

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
      evalExpr ast path expr @?= (DNode $ findNode ast [0])
  ]


evalExpr :: PicoJavaAST -> [Int] -> Expr PicoJavaAttr -> Domain (String, Int)

findNode ast [] = ast
findNode ast (n:ns) = findNode ((children ast) !! n) ns

nodeId (DNode (AST (_, i) _ _)) = i

evalExpr ast path expr =
  let n = findNode ast path
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
