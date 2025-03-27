module PlaygroundLangSpec where

import Saiga
import PlaygroundLang
import Test.Tasty
import Test.Tasty.HUnit
import AST
import Util
import qualified CFGLang

import Util (findNodeByPath, nodeId, parseAndNumber)

dummyAST = CFGLang.unknownDecl
dummyNodeId = snd $ kind $ dummyAST

playgroundLangTests = testGroup "Saiga attributes for the Playground language" [
  testCase "Compute sqrt attribute " $
    let expr = Node <.> Sqrt1 <?> [IVal 20]
    in evalExpr1 playProgram dummyAST dummyNodeId expr @?= (DInt 4),

  testCase "Compute sqrt1 attribute " $
    let expr = Node <.> Sqrt1 <?> [IVal 11]
    in evalExpr1 playProgram dummyAST dummyNodeId expr @?= (DInt 3),

  testCase "Compute sqrt2 attribute " $
    let expr = Node <.> Sqrt2 <?> [IVal 11]
    in evalExpr1 playProgram dummyAST dummyNodeId expr @?= (DInt 3),

  testCase "Compute sqrt4 attribute " $
    let expr = Node <.> Sqrt4 <?> [IVal 11]
    in evalExpr1 playProgram dummyAST dummyNodeId expr @?= (DInt 3),

  testCase "Compute sqrt5 attribute " $
    let expr = Node <.> Sqrt5 <?> [IVal 29]
    in evalExpr1 playProgram dummyAST dummyNodeId expr @?= (DInt 5),

  testCase "Compute sqrt6 attribute " $
    let expr = Node <.> Sqrt6 <?> [IVal 64]
    in evalExpr1 playProgram dummyAST dummyNodeId expr @?= (DInt 8)
  ]
