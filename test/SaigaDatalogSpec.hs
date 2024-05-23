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
  testCase "Translate If Expression" $ (show $ translateToTerm' test1) @?= "[(DBool True,[eq(_arg_0, DBool True)]),(DBool False,[eq(_arg_0, DBool False)])]",

  testCase "Translate function definition" $ (prettyProgram $ Program $ translateToClause'
                                               (Saiga.Function "finddecl" 1 SaigaPicoJava.findDeclExpr)) @?=
    "finddecl(_arg_0, _h) <- eq(_g, DBool True), eq(_f, _g), _cons(_b, _e, _f), _head(_a, _b), _tail(_arg_0, _a), _cons(_c, _d, _e), _nil(_c), _nil(_d), mkUnknownDecl(_h)\nfinddecl(_arg_0, _t) <- eq(_g, DBool False), eq(_f, _g), _cons(_b, _e, _f), _head(_a, _b), _tail(_arg_0, _a), _cons(_c, _d, _e), _nil(_c), _nil(_d), eq(_q, DBool True), eq(_p, _q), _cons(_i, _o, _p), _head(_arg_0, _i), _cons(_j, _n, _o), Name(_m, _j), _head(_l, _m), _head(_k, _l), _tail(_arg_0, _k), _nil(_n), _head(_s, _t), _head(_r, _s), _tail(_arg_0, _r)\nfinddecl(_arg_0, _h) <- eq(_g, DBool True), eq(_f, _g), _cons(_b, _e, _f), _head(_a, _b), _tail(_arg_0, _a), _cons(_c, _d, _e), _nil(_c), _nil(_d), mkUnknownDecl(_h)\nfinddecl(_arg_0, _b_a) <- eq(_g, DBool False), eq(_f, _g), _cons(_b, _e, _f), _head(_a, _b), _tail(_arg_0, _a), _cons(_c, _d, _e), _nil(_c), _nil(_d), eq(_q, DBool False), eq(_p, _q), _cons(_i, _o, _p), _head(_arg_0, _i), _cons(_j, _n, _o), Name(_m, _j), _head(_l, _m), _head(_k, _l), _tail(_arg_0, _k), _nil(_n), finddecl(_a_a, _b_a), _cons(_u, _z, _a_a), _head(_arg_0, _u), _cons(_x, _y, _z), _tail(_w, _x), _head(_v, _w), _tail(_arg_0, _v), _nil(_y)"
  ]
