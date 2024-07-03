module SaigaDatalogSpec where

import Saiga
import Datalog
import SaigaDatalog
import SaigaPicoJava
import DemandTransformation

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.Golden
import Test.Tasty.HUnit

import qualified Data.Set as Set

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
  (==) (Datalog.Function n1 ts1 _) (Datalog.Function n2 ts2 _) = n1 == n2 && ts1 == ts2
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

dumpProgramToFile :: (Show a, Show s) => Program a s -> FilePath -> IO ()
dumpProgramToFile p f = writeFile f (prettyProgram p)

goldenProgramTest tname p goldenFile outputFile =
  goldenVsFile tname goldenFile outputFile (dumpProgramToFile p outputFile)

saigaDatalogTests = testGroup "Saiga to Datalog translation" [
  --
  testCase "Translate If Expression" $ (show $ translateToTerm' test1) @?= "[(DBool True,[_builtin_eq(_arg_0, DBool True)]),(DBool False,[_builtin_eq(_arg_0, DBool False)])]",

  --
  goldenProgramTest "Translate function definition"
    (Program $ translateToClause' (Saiga.Function "finddecl" 2 SaigaPicoJava.findDeclExpr))
    "testfiles/SaigaDatalog/function_def.dl.golden"
    "testfiles/SaigaDatalog/function_def.dl.out",

  --
  goldenProgramTest "Translate attribute definition"
    (Program $ translateToClause' SaigaPicoJava.declAttr)
    "testfiles/SaigaDatalog/attribute.dl.golden"
    "testfiles/SaigaDatalog/attribute.dl.out",

  --
  goldenProgramTest "Translate whole PicoJava program"
    (translateProgram (SaigaPicoJava.picoJavaProgram SaigaPicoJava.boolDecl))
    "testfiles/SaigaDatalog/picojava.dl.golden"
    "testfiles/SaigaDatalog/picojava.dl.out",

  -- demand transformation of the whole program
  let dlPicoJava = translateProgram $ SaigaPicoJava.picoJavaProgram SaigaPicoJava.boolDecl
      dlPicoJavaDemand = transformProgram dlPicoJava (initialDemand "Type" (Set.fromList [0]))
  in
    goldenProgramTest "Translate whole PicoJava program and apply the demand transformation" dlPicoJavaDemand
    "testfiles/SaigaDatalog/picojava-demand.dl.golden"
    "testfiles/SaigaDatalog/picojava-demand.dl.out",

  -- translate the type attribute
  goldenProgramTest "Translate the type attribute"
    (Program $ translateToClause' SaigaPicoJava.typeAttr)
    "testfiles/SaigaDatalog/type.dl.golden"
    "testfiles/SaigaDatalog/type.dl.out",

  -- demand transformation of the type attribute
  let dlTypeAttr = Program $ translateToClause' SaigaPicoJava.typeAttr
      dlTypeAttrDemand = transformProgram dlTypeAttr (initialDemand "Type" (Set.fromList [0]))
  in
    goldenProgramTest "Translate the type attribute and apply the demand transformation" dlTypeAttrDemand
    "testfiles/SaigaDatalog/type-demand.dl.golden"
    "testfiles/SaigaDatalog/type-demand.dl.out",

  -- list predicates not defined in the file
  let dlPicoJava = translateProgram $ SaigaPicoJava.picoJavaProgram SaigaPicoJava.boolDecl
      preds = predicates dlPicoJava
      idbPreds = idbPredicates dlPicoJava
      edbPreds = Set.difference preds idbPreds
  in
    testCase "List EDB predicates" $ edbPreds @?= Set.fromList ["Child","Children","Kind","Name",
                                                                "Parent","_head","_nil","_tail",
                                                                "mkUnknownClass","mkUnknownDecl","predefs"]
  ]
