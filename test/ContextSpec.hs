module ContextSpec(contextTests) where

import Prelude hiding ((*), (+))
import Data.Semiring
import Context
import Test.Tasty
import Test.Tasty.HUnit

_0 = (zero::ContextSemiring2 Int)
_1 = (one::ContextSemiring2 Int)

xy = ctx2 1 2
xz = ctx2 1 3
zw = ctx2 3 4

xa = push (ContextValue 1) _1

contextTests = testGroup "Context tests" [
  testCase "one" $ show (one::ContextSemiring2 Int) @?= "fromList [Context2 Any Any]",
  testCase "1*1=1" $ (one * one) @?= (one::ContextSemiring2 Int),
  testCase "1+1=1" $ (one + one) @?= (one::ContextSemiring2 Int),
  testCase "1*[x, y]=[x,y]" $ (one::ContextSemiring2 Int) * (ctx2 1 2) @?= (ctx2 1 2),

  testCase "0*[x, y]=[x,y]" $ (zero::ContextSemiring2 Int) * (ctx2 1 2) @?= zero,
  testCase "0+[x, y]=[x,y]" $ (zero::ContextSemiring2 Int) + (ctx2 1 2) @?= (ctx2 1 2),
  testCase "[x, y] + [x, y] = [x, y]" $ xy + xy @?= xy,
  testCase "[x, y] + [z, w] = [z, w] + [x, y]" $ xy + zw @?= zw + xy,
  testCase "[x, y] * [z, w] = 0" $ xy * zw @?= _0,
  testCase "[x, y] * ([z, w] + [x, y]) = [x, y] * [z, w] + [x, y] * [x, y]" $ xy * (zw + xy) @?= xy * zw + xy * xy,
  testCase "[x, Any] * [x, z] = [x, z]" $ xa * xz @?= xz,
  testCase "push x into 0 = 0" $ push (ContextValue 1) _0 @?= _0
  ]
