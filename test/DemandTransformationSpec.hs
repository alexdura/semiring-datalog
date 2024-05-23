module DemandTransformationSpec (demandTransformationTests) where

import Datalog
import DemandTransformation

import Test.Tasty (testGroup)
import Test.Tasty.HUnit ( testCase, (@?=) )


import qualified Data.Map as Map
import qualified Data.Set as Set

demandTransformationTests = testGroup "Demand Transformation" [
  testCase "Compute demand of transitive closure program" $ programPredicateDemand p1 (initialDemand "path" (Set.fromList [0])) @?=
    Map.fromList [("edge",Set.fromList [Set.fromList [0]]),
                  ("path",Set.fromList [Set.fromList [0]])],
  testCase "Compute demand of transitive closure program" $ programPredicateDemand p1 (initialDemand "path" (Set.fromList [1])) @?=
    Map.fromList [("edge",Set.fromList [Set.fromList [],Set.fromList [0],Set.fromList [0,1],Set.fromList [1]]),
                  ("path",Set.fromList [Set.fromList [],Set.fromList [0],Set.fromList [0,1],Set.fromList [1]])]
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

initialDemand :: Predicate -> DemandPattern -> PredicateDemand
initialDemand p d = Map.fromList [(p, Set.fromList [d])]
