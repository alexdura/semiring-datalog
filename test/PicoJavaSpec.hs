module PicoJavaSpec (picoJavaTests) where

import PicoJava
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit


-- resultToString = \case Left err -> show err
--                        Right ast -> pretty ast


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

    testCase "Parse Block" $ parseBlock "{ X x; x = y; }" @?= Right (AST {kind = "Block", token = "",
                                                                          children = [AST {kind = "VarDecl", token = "x",
                                                                                           children = [AST {kind = "Use", token = "X", children = []}]},
                                                                                      AST {kind = "Stmt", token = "",
                                                                                           children = [AST {kind = "Use", token = "x", children = []},
                                                                                                       AST {kind = "Use", token = "y", children = []}]}]}),



    testCase "Parse Program" $  parseProgram program1 @?= expectedProgram1

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

expectedProgram1 = Right (AST {kind = "Program", token = "",
                               children = [AST {kind = "Class", token = "A",
                                                children = [AST {kind = "UnknownClass", token = "", children = []},
                                                            AST {kind = "Block", token = "",
                                                                 children = [AST {kind = "VarDecl", token = "y",
                                                                                  children = [AST {kind = "Use", token = "boolean", children = []}]},
                                                                             AST {kind = "VarDecl", token = "a",
                                                                                  children = [AST {kind = "Use", token = "AA", children = []}]},
                                                                             AST {kind = "Stmt", token = "",
                                                                                  children = [AST {kind = "Use", token = "y", children = []},
                                                                                              AST {kind = "Dot", token = ".",
                                                                                                   children = [AST {kind = "Use", token = "a", children = []},
                                                                                                               AST {kind = "Use", token = "x", children = []}]}]},
                                                                             AST {kind = "Class", token = "AA",
                                                                                  children = [AST {kind = "UnknownClass", token = "", children = []},
                                                                                              AST {kind = "Block", token = "",
                                                                                                   children = [AST {kind = "VarDecl", token = "x",
                                                                                                                    children = [AST {kind = "Use", token = "boolean", children = []}]}]}]},
                                                                             AST {kind = "Class", token = "BB",
                                                                                  children = [AST {kind = "Use", token = "AA", children = []},
                                                                                              AST {kind = "Block", token = "",
                                                                                                   children = [AST {kind = "VarDecl", token = "b",
                                                                                                                    children = [AST {kind = "Use", token = "BB", children = []}]},
                                                                                                               AST {kind = "Stmt", token = "",
                                                                                                                    children = [AST {kind = "Dot", token = ".",
                                                                                                                                     children = [AST {kind = "Use", token = "b", children = []},
                                                                                                                                                 AST {kind = "Use", token = "y", children = []}]},
                                                                                                                                AST {kind = "Dot", token = ".",
                                                                                                                                     children = [AST {kind = "Use", token = "b", children = []},
                                                                                                                                                 AST {kind = "Use", token = "x", children = []}]}]}]}]}]}]}]})
