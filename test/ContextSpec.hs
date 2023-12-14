module ContextSpec(contextTests) where

import Prelude hiding ((*), (+))
import Data.Semiring
import Context
import Test.Tasty
import Test.Tasty.HUnit



contextTests = testGroup "Context tests" [
  testCase "one" $ show (one::ContextSemiring2 Int) @?= "fromList [Context2 Any Any]",
  testCase "1*1=1" $ (one * one) @?= (one::ContextSemiring2 Int),
  testCase "1+1=1" $ (one + one) @?= (one::ContextSemiring2 Int),
  testCase "1*[x, y]=[x,y]" $ (one::ContextSemiring2 Int) * (ctx2 1 2) @?= (ctx2 1 2)
  ]
