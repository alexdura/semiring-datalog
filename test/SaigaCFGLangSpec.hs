module SaigaCFGLangSpec where

import Saiga
import CFGLang
import Test.Tasty
import Test.Tasty.HUnit
import AST
import Util

import Util (findNodeByPath, nodeId, parseAndNumber)
import GHC.Num (Natural(NB))
import Text.Printf (FormatAdjustment(ZeroPad))

saigaCFGLangTests = testGroup "Saiga attributes for the CFG language" [
  testCase "Number nodes" $
    AST.numberNodes cfg1 @?= cfg1',

  testCase "Compute decl attributre" $
    let nid = 35 -- "Y"
        expr = Node <.> FindDecl <?> [SVal "Y"]
    in nodeId (evalExpr1 cfgProgram cfg1' nid expr) @?= 19,

  testCase "Compute decl attributre" $
    let nid = 25 -- "Y"
        expr = Node <.> Decl <?> []
    in nodeId (evalExpr1 cfgProgram cfg1' nid expr) @?= 19,

  testCase "Compute nullable attribute - non circular" $
    let nid = 0 -- "NDecl X"
        expr = Node <.> Nullable <?> []
    in evalExpr1 cfgProgram cfg1' nid expr @?= DBool True,

  localOption (mkTimeout $ 5 * 10^6) $ testCase "Compute nullable attribute - circular" $
    let nid = 20 -- "NDecl X"
        expr = Node <.> Nullable <?> []
    in evalExpr1 cfgProgram cfg1' nid expr @?= DBool True

  ]


a = terminal "a"
c = terminal "c"
d = terminal "d"

cfg1 = cfg [
  --  X ::= Y
  --      | a
  --  Y ::= c
  --      | empty
  --  Z ::= XYZ
  --      | d
  rule (nDecl "X") (prods [symbols [nUse "Y"], symbols [a]]),
  rule (nDecl "Y") (prods [symbols [c], symbols []]),
  rule (nDecl "Z") (prods [symbols [nUse "X", nUse "Y", nUse "Z"], symbols [d]])
  ]


cfg1' = AST {kind = ("CFG",35), token = "", children = [AST {kind = ("Rule",10), token = "", children = [AST {kind = ("NDecl",0), token = "X", children = []},AST {kind = ("NonEmptyProdList",9), token = "", children = [AST {kind = ("NonEmptyProdList",5), token = "", children = [AST {kind = ("EmptyProdList",1), token = "", children = []},AST {kind = ("NonEmptySymbolList",4), token = "", children = [AST {kind = ("EmptySymbolList",2), token = "", children = []},AST {kind = ("NUse",3), token = "Y", children = []}]}]},AST {kind = ("NonEmptySymbolList",8), token = "", children = [AST {kind = ("EmptySymbolList",6), token = "", children = []},AST {kind = ("Terminal",7), token = "a", children = []}]}]}]},AST {kind = ("Rule",19), token = "", children = [AST {kind = ("NDecl",11), token = "Y", children = []},AST {kind = ("NonEmptyProdList",18), token = "", children = [AST {kind = ("NonEmptyProdList",16), token = "", children = [AST {kind = ("EmptyProdList",12), token = "", children = []},AST {kind = ("NonEmptySymbolList",15), token = "", children = [AST {kind = ("EmptySymbolList",13), token = "", children = []},AST {kind = ("Terminal",14), token = "c", children = []}]}]},AST {kind = ("EmptySymbolList",17), token = "", children = []}]}]},AST {kind = ("Rule",34), token = "", children = [AST {kind = ("NDecl",20), token = "Z", children = []},AST {kind = ("NonEmptyProdList",33), token = "", children = [AST {kind = ("NonEmptyProdList",29), token = "", children = [AST {kind = ("EmptyProdList",21), token = "", children = []},AST {kind = ("NonEmptySymbolList",28), token = "", children = [AST {kind = ("NonEmptySymbolList",26), token = "", children = [AST {kind = ("NonEmptySymbolList",24), token = "", children = [AST {kind = ("EmptySymbolList",22), token = "", children = []},AST {kind = ("NUse",23), token = "X", children = []}]},AST {kind = ("NUse",25), token = "Y", children = []}]},AST {kind = ("NUse",27), token = "Z", children = []}]}]},AST {kind = ("NonEmptySymbolList",32), token = "", children = [AST {kind = ("EmptySymbolList",30), token = "", children = []},AST {kind = ("Terminal",31), token = "d", children = []}]}]}]}]}
