{-# LANGUAGE DataKinds #-}

import Test.Tasty
import Test.Tasty.HUnit

import Datalog
import Eval
import Data.Semiring.Tropical
import Data.Semiring
import Data.Monoid

x = var("x")
y = var("y")
z = var("z")

edge = lit "edge"
path = lit "path"

p1 = Program [
  [edge [cst 1, cst 2], edge [cst 2, cst 3]] |- [val True],
  [path [x, y]] |- [edge [x, y]],
  [path [x, z]] |- [path [x, y], path [y, z]]
  ]

r1 = eval p1 emptyContext

p2 = Program [
  [edge [cst 1, cst 2], edge [cst 2, cst 3]] |- [val False],
  [path [x, y]] |- [edge [x, y]],
  [path [x, z]] |- [path [x, y], path [y, z]]
  ]

r2 = eval p2 emptyContext

p3 = Program [
  [edge [cst 1, cst 2], edge [cst 2, cst 3]] |- [val ((Tropical (Sum 1))::Tropical 'Minima (Sum Int))],
  [path [x, y]] |- [edge [x, y]],
  [path [x, z]] |- [path [x, y], path [y, z]]
  ]

r3 = eval p3 emptyContext


main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests" [
  testCase "pretty print" $ (prettyProgram p1) @?= "edge(1, 2), edge(2, 3) <- True\npath(\"x\", \"y\") <- edge(\"x\", \"y\")\npath(\"x\", \"z\") <- path(\"x\", \"y\"), path(\"y\", \"z\")",
  testCase "eval 1" $ (query "edge" r1) @?= [([1, 2], True), ([2, 3], True)],
  testCase "eval 2" $ (query "path" r1) @?= [([1, 2], True), ([1, 3], True), ([2, 3], True)],
  testCase "eval 3" $ (query "edge" r2) @?= [],
  testCase "eval 4" $ (query "path" r2) @?= [],
  testCase "eval 5" $ (query "path" r3) @?= [([1,2],Tropical (Sum {getSum = 1})),([1,3],Tropical (Sum {getSum = 2})),([2,3],Tropical (Sum {getSum = 1}))]
  ]
