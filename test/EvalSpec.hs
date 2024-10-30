{-# LANGUAGE DataKinds #-}

module EvalSpec(evalTests, csvTests) where

import Control.Monad
import Data.Monoid
import Data.Semiring
import Data.Semiring.Tropical
import Datalog
import Eval
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import qualified Programs
import Context


evalTests :: TestTree
evalTests = testGroup "Unit tests" [
  testCase "pretty print" $ (prettyProgram p1) @?= "edge(1, 2), edge(2, 3) <- True\npath(x, y) <- edge(x, y)\npath(x, z) <- path(x, y), path(y, z)",
  testCase "eval 1.1" $ (query "edge" r1) @?= [([1, 2], True), ([2, 3], True)],
  testCase "eval 1.2" $ (query "path" r1) @?= [([1, 2], True), ([1, 3], True), ([2, 3], True)],
  testCase "eval 2.1" $ (query "edge" r2) @?= [],
  testCase "eval 2.2" $ (query "path" r2) @?= [],
  testCase "eval 3" $ (query "path" r3) @?= [([1,2],Tropical (Sum {getSum = 1})),([1,3],Tropical (Sum {getSum = 2})),([2,3],Tropical (Sum {getSum = 1}))],
  testCase "eval 4" $ (query "pred2" r4) @?= [([1], (ctx2 1 2) Data.Semiring.+ (ctx2 3 4))],
  testCase "eval 5" $ (query "E" r5) @?= [([2],True),([3],True),([4],True)],
  testCase "eval 6.1" $ (query "P" r6) @?= [([0], True), ([1], True)],
  testCase "eval 6.2" $ (query "Q" r6) @?= [([1], True)],
  testCase "eval 6.3" $ (query "R" r6) @?= [([10], True)],
  testCase "eval 7" $ (query "P" r7) @?= [([1], True)]
  ]


csvTestHelper :: (DatalogGroundTerm a, Semiring s, Show s, Eq s) => Program a s -> [(String, FilePath)] -> [(String, FilePath)] -> IO ()
csvTestHelper p files outs = do
  ctx <- foldM (\ctx (name, path) -> loadFromCSV ctx name (const one) path) emptyContext files
  let ctx' = eval p ctx
  mapM_ (\(outrel, outpath) -> storeToCSV ctx' outrel outpath) outs


csvTest tname p golden files (outrel, outpath) =
  goldenVsFile tname golden outpath (csvTestHelper p files [(outrel, outpath)])


csvTests = testGroup "CSV tests" [
  csvTest "pointsto" (Programs.andersen::Program Int Bool) "testfiles/andersen/PointsTo.csv.golden" [("Alloc", "testfiles/andersen/Alloc.csv"),
                                                                        ("Move", "testfiles/andersen/Move.csv"),
                                                                        ("Load", "testfiles/andersen/Load.csv"),
                                                                        ("Store", "testfiles/andersen/Store.csv"),
                                                                        ("Call", "testfiles/andersen/Call.csv"),
                                                                        ("VCall", "testfiles/andersen/VCall.csv"),
                                                                        ("FormalArg", "testfiles/andersen/FormalArg.csv"),
                                                                        ("ActualArg", "testfiles/andersen/ActualArg.csv"),
                                                                        ("FormalReturn", "testfiles/andersen/FormalReturn.csv"),
                                                                        ("ActualReturn", "testfiles/andersen/ActualReturn.csv"),
                                                                        ("Reachable", "testfiles/andersen/Reachable.csv")]
    ("VarPointsTo", "testfiles/andersen/PointsTo.csv")
  ]





x = var("x")
y = var("y")
z = var("z")

edge = lit "edge"
path = lit "path"

p1 :: Program Integer Bool
p1 = Program [
  [edge [cst 1, cst 2], edge [cst 2, cst 3]] += [val True],
  [path [x, y]] += [edge [x, y]],
  [path [x, z]] += [path [x, y], path [y, z]]
  ]

r1 = eval p1 emptyContext

p2 :: Program Integer Bool
p2 = Program [
  [edge [cst 1, cst 2], edge [cst 2, cst 3]] += [val False],
  [path [x, y]] += [edge [x, y]],
  [path [x, z]] += [path [x, y], path [y, z]]
  ]

r2 = eval p2 emptyContext

p3 :: Program Integer (Tropical 'Minima (Sum Int))
p3 = Program [
  [edge [cst 1, cst 2], edge [cst 2, cst 3]] += [val ((Tropical (Sum 1))::Tropical 'Minima (Sum Int))],
  [path [x, y]] += [edge [x, y]],
  [path [x, z]] += [path [x, y], path [y, z]]
  ]

r3 = eval p3 emptyContext

pred1 = lit "pred1"
pred2 = lit "pred2"

p4 :: Program Integer (ContextSemiring2 Int)
p4 = Program [
  [pred1 [cst 1, cst 2]] += [val $ ctx2 1 2],
  [pred1 [cst 1, cst 3]] += [val $ ctx2 3 4],
  [pred2 [x]] += [pred1 [x, y]]]

r4 = eval p4 emptyContext

p5 :: Program Integer Bool
p5 =
  let k = lit "K"
      e = lit "E"
  in
  Program [
    [k[cst 1], k[cst 2]] += [val True],
    [e[Expr "+" [x, y] (\[x', y'] -> x' Prelude.+ y')]] += [k[x], k[y]]]

r5 = eval p5 emptyContext


p6 :: Program Integer Bool
p6 =
  let p = lit "P"
      q = lit "Q"
      r = lit "R"
      x = var "x"
  in
    Program [
    [p[Fresh [cst 2, cst 3]], p[Fresh [cst 4, cst 5]]] += [val True],
    [q[Fresh [cst 4, cst 5]]] += [val True],
    [r[cst 10]] += [q[Fresh [cst 4, cst 5]]]
    ]

r6 = eval p6 emptyContext


p7 :: Program Integer Bool
p7 =
  let p = lit "P"
      x = var "x"
  in
    Program [
    [p[x]] += [bind (cst 1) x]
    ]
r7 = eval p7 emptyContext
