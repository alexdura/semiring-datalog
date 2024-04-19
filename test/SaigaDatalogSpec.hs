module SaigaDatalogSpec (saigaDatalogTests) where

import Saiga
import Datalog
import SaigaDatalog

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit


-- Eq instance to be used for testing; does not properly handle Expr!
instance (Eq a, Eq b) => (Eq (Term a b)) where
  (==) (Variable s1) (Variable s2) = s1 == s2
  (==) (Constant v1) (Constant v2) = v1 == v2
  (==) (Expr ts1 _) (Expr ts2 _) = ts1 == ts2
  (==) _ _ = False

-- Eq instance to be used for testing; does not properly handle Literal and Function
instance (Eq a, Eq b) => (Eq (Atom a b)) where
  (==) (Literal p1 ts1 _) (Literal p2 ts2 _) = p1 == p2 && ts1 == ts2
  (==) (Value v1) (Value v2) = v1 == v2
  (==) (Function ts1 _) (Function ts2 _) = ts1 == ts2
  (==) _ _ = False


-- instance Show
instance (Show a, Show b) => (Show (Term a b)) where
  show = prettyTerm

instance (Show a, Show b) => (Show (Atom a b)) where
  show = prettyAtom

data TestAttribute = Attr1
                   | Attr2
                   deriving (Show, Enum, Eq)

test1 :: Expr TestAttribute
test1 = IfElse (Arg) (BVal True) (BVal False)

type TestDomain = Domain String

translateToTerm' :: Expr TestAttribute -> [(SaigaTerm TestDomain, [SaigaAtom TestDomain])]
translateToTerm' = translateToTerm

saigaDatalogTests = testGroup "Saiga to Datalog translation" [
  -- testCase "Translate If Expression" $ translateToTerm' test1 @?= [
  --     (DBool True,[__builtin_function__("arg", DBool True)]),
  --       (DBool False,[__builtin_function__("arg", DBool False)])]

  ]
