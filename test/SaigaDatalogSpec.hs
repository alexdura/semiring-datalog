module SaigaDatalogSpec where

import Saiga
import Datalog
import SaigaDatalog
import SaigaPicoJava

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
  (==) (Datalog.Function ts1 _) (Datalog.Function ts2 _) = ts1 == ts2
  (==) _ _ = False


-- instance Show
instance (Show a, Show b) => (Show (Term a b)) where
  show = prettyTerm

instance (Show a, Show b) => (Show (Atom a b)) where
  show = prettyAtom

data TestAttribute = Attr1
                   | Attr2
                   deriving (Show, Enum, Eq)

instance SaigaAttribute TestAttribute

test1 :: Expr TestAttribute
test1 = IfElse (Arg 0) (BVal True) (BVal False)

type TestDomain = Domain String

translateToTerm' :: Expr TestAttribute -> [(SaigaTerm TestDomain, [SaigaAtom TestDomain])]
translateToTerm' = translateToTerm

translateToClause' :: SaigaElement PicoJavaAttr TestDomain -> [SaigaClause TestDomain]
translateToClause' = translateToClause

saigaDatalogTests = testGroup "Saiga to Datalog translation" [
  testCase "Translate If Expression" $ (show $ translateToTerm' test1) @?= "[(DBool True,[_builtin_eq(_arg_0, DBool True)]),(DBool False,[_builtin_eq(_arg_0, DBool False)])]",

  testCase "Translate function definition" $ (prettyProgram $ Program $ translateToClause'
                                               (Saiga.Function "finddecl" 2 SaigaPicoJava.findDeclExpr)) @?=
    "finddecl(_arg_0, _arg_1, _c) <- _builtin_eq(_b, DBool True), eq(_arg_1, _a, _b), _nil(_a), mkUnknownDecl(_c)\n\
    \finddecl(_arg_0, _arg_1, _g) <- _builtin_eq(_b, DBool False), eq(_arg_1, _a, _b), _nil(_a), _builtin_eq(_f, DBool True), eq(_arg_0, _d, _f), Name(_e, _d), _head(_arg_1, _e), _head(_arg_1, _g)\n\
    \finddecl(_arg_0, _arg_1, _c) <- _builtin_eq(_b, DBool True), eq(_arg_1, _a, _b), _nil(_a), mkUnknownDecl(_c)\n\
    \finddecl(_arg_0, _arg_1, _i) <- _builtin_eq(_b, DBool False), eq(_arg_1, _a, _b), _nil(_a), _builtin_eq(_f, DBool False), eq(_arg_0, _d, _f), Name(_e, _d), _head(_arg_1, _e), finddecl(_arg_0, _h, _i), _tail(_arg_1, _h)"
  ]
