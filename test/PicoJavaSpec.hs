module PicoJavaSpec (picoJavaTests) where

import PicoJava
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit


resultToString = \case Left err -> show err
                       Right ast -> pretty ast


picoJavaTests = testGroup "PicoJava tests" [
  testCase "Parse Stmt" $ parseStmt "a = b;" @?=  Right (AST {kind = "Stmt", token = "",
                                                           children = [AST {kind = "Use", token = "a", children = []},
                                                                        AST {kind = "Use", token = "b", children = []}]}),
    testCase "Parse Access" $ parseAccess "x.y" @?= Right (AST {kind = "Dot", token = ".",
                                                                children = [AST {kind = "Use", token = "x",
                                                                                 children = []},
                                                                            AST {kind = "Use", token = "y",
                                                                                 children = []}]}),
    testCase "Parse Access" $ parseAccess "t.u.v" @?= Right (AST {kind = "Dot", token = ".",
                                                                  children = [AST {kind = "Dot", token = ".",
                                                                                   children = [AST {kind = "Use", token = "t", children = []},
                                                                                               AST {kind = "Use", token = "u", children = []}]},
                                                                               AST {kind = "Use", token = "v", children = []}]}),

    testCase "Parse Access" $ parseAccess "a" @?= Right (AST {kind = "Use", token = "a", children = []}),

    testCase "Parse Block" $ parseBlock "{ X x; x = y; }" @?= Right (AST {kind = "Use", token = "a", children = []}),


    testCase "Parse Program" $  resultToString (parseProgram program1) @?= ""
  ]

program1 =
  "class A {\n\
  \ boolean y;\n\
  \ AA a;\n\
  \ y = a.x;\n\
  \ class AA {\n\
  \  boolean x;\n\
  \ }\n\
  \ class BB extends AA {\n\
  \   BB b; \n\
  \   b.y = b.x; \n\
  \ }\n\
  \ }"
