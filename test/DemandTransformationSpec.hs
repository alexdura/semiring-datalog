module DemandTransformationSpec (demandTransformationTests) where

import Datalog
import DemandTransformation

import Test.Tasty (testGroup)
import Test.Tasty.HUnit ( testCase, (@?=) )


import qualified Data.Map as Map
import qualified Data.Set as Set


edge = lit "edge"
path = lit "path"
x = var "x"
y = var "y"
z = var "z"
isEDB p | p == "edge" = True
        | otherwise = False

demandTransformationTests = testGroup "Demand Transformation" [
  testCase "Compute demand of transitive closure program" $ programPredicateDemand p1 (initialDemand "path" (Set.fromList [0])) @?=
    Map.fromList [("edge",Set.fromList [Set.fromList [0]]),
                  ("path",Set.fromList [Set.fromList [0]])],

  testCase "Compute demand of transitive closure program" $ programPredicateDemand p1 (initialDemand "path" (Set.fromList [1])) @?=
    Map.fromList [("edge",Set.fromList [Set.fromList [],Set.fromList [0],Set.fromList [0,1],Set.fromList [1]]),
                  ("path",Set.fromList [Set.fromList [],Set.fromList [0],Set.fromList [0,1],Set.fromList [1]])],

  testCase "Generate demand rules 1" $
    prettyClause <$> (genDemandRules (path[x, z] :: Atom Int Bool) (Set.fromList [0]) [path[x, y], edge[y, z]]) isEDB @?=
    [
      "path(x, z) <- d_path_bf(x), path(x, y), edge(y, z)",
      "d_path_bf(x) <- d_path_bf(x)"
    ],

  testCase "Demand transformation for the whole program" $
    prettyProgram (transformProgram p2 (initialDemand "path" (Set.fromList [0]))) @?=
    "path(x, y) <- d_path_bf(x), edge(x, y)\n\
    \path(x, y) <- d_path_bf(x), path(x, z), edge(z, y)\n\
    \d_path_bf(x) <- d_path_bf(x)",

  testCase "Demand transformation for the whole program" $
    prettyProgram (transformProgram p2 (initialDemand "path" (Set.fromList [1]))) @?=
    "path(x, y) <- d_path_ff(), edge(x, y)\n\
    \path(x, y) <- d_path_fb(y), edge(x, y)\n\
    \path(x, y) <- d_path_ff(), path(x, z), edge(z, y)\n\
    \d_path_ff() <- d_path_ff()\n\
    \path(x, y) <- d_path_fb(y), path(x, z), edge(z, y)\n\
    \d_path_ff() <- d_path_fb(y)"
  ]


p1 :: Program Integer Bool
p1 =
  let
    x = var("x")
    y = var("y")
    z = var("z")

    edge = lit "edge"
    path = lit "path"
  in

    Program [
    [edge [cst 1, cst 2], edge [cst 2, cst 3]] += [],
    [path [x, y]] += [edge [x, y]],
    [path [x, z]] += [path [x, y], path [y, z]]
    ]


p2 :: Program Integer Bool
p2 =
  let
    x = var("x")
    y = var("y")
    z = var("z")

    edge = lit "edge"
    path = lit "path"
  in

    Program [
    [path [x, y]] += [edge [x, y]],
    [path [x, y]] += [path [x, z], edge [z, y]]
    ]



initialDemand :: Predicate -> DemandPattern -> PredicateDemand
initialDemand p d = Map.fromList [(p, Set.fromList [d])]
