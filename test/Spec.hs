import Test.Tasty
import Test.Tasty.HUnit

import Datalog

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

main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests" [
  testCase "pretty print" $ (prettyProgram p1) @?= ""
                               ]
