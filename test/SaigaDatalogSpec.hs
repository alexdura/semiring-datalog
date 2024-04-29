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
test1 = IfElse (Arg) (BVal True) (BVal False)

type TestDomain = Domain String

translateToTerm' :: Expr TestAttribute -> [(SaigaTerm TestDomain, [SaigaAtom TestDomain])]
translateToTerm' = translateToTerm

translateToClause' :: SaigaElement PicoJavaAttr TestDomain -> [SaigaClause TestDomain]
translateToClause' = translateToClause

saigaDatalogTests = testGroup "Saiga to Datalog translation" [
  testCase "Translate If Expression" $ (show $ translateToTerm' test1) @?= "[(DBool True,[eq(_arg, DBool True)]),(DBool False,[eq(_arg, DBool False)])]",

  testCase "Translate function definition" $ (prettyProgram $ Program $ translateToClause'
                                               (Saiga.Function "finddecl" SaigaPicoJava.findDeclExpr)) @?=
    "finddecl(_arg, _i) <- eq(_g, DBool True), eq(_f, _g), _cons(_b, _e, _f), _head(_a, _b), _tail(_arg, _a), _cons(_c, _d, _e), _nil(_c), _nil(_d), mkUnknownDecl(_h, _i), _nil(_h)\nfinddecl(_arg, _v) <- eq(_g, DBool False), eq(_f, _g), _cons(_b, _e, _f), _head(_a, _b), _tail(_arg, _a), _cons(_c, _d, _e), _nil(_c), _nil(_d), eq(_s, DBool True), eq(_r, _s), _cons(_j, _q, _r), _head(_arg, _j), _cons(_k, _p, _q), Name(_o, _o, _k), _nil(_o), _head(_m, _n), _head(_l, _m), _tail(_arg, _l), _nil(_p), _head(_u, _v), _head(_t, _u), _tail(_arg, _t)\nfinddecl(_arg, _i) <- eq(_g, DBool True), eq(_f, _g), _cons(_b, _e, _f), _head(_a, _b), _tail(_arg, _a), _cons(_c, _d, _e), _nil(_c), _nil(_d), mkUnknownDecl(_h, _i), _nil(_h)\nfinddecl(_arg, _d_a) <- eq(_g, DBool False), eq(_f, _g), _cons(_b, _e, _f), _head(_a, _b), _tail(_arg, _a), _cons(_c, _d, _e), _nil(_c), _nil(_d), eq(_s, DBool False), eq(_r, _s), _cons(_j, _q, _r), _head(_arg, _j), _cons(_k, _p, _q), Name(_o, _o, _k), _nil(_o), _head(_m, _n), _head(_l, _m), _tail(_arg, _l), _nil(_p), finddecl(_c_a, _d_a), _cons(_w, _b_a, _c_a), _head(_arg, _w), _cons(_z, _a_a, _b_a), _tail(_y, _z), _head(_x, _y), _tail(_arg, _x), _nil(_a_a)"
  ]
