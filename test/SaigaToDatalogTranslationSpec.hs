module SaigaToDatalogTranslationSpec where

import Saiga
import Datalog
import SaigaToDatalogTranslation
import SaigaPicoJava
import DemandTransformation
import CFGLang
import PlaygroundLang

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.Golden
import Test.Tasty.HUnit

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

-- Eq instance to be used for testing; does not properly handle Expr!
instance (Eq a) => (Eq (Term a)) where
  (==) (Variable s1) (Variable s2) = s1 == s2
  (==) (Constant v1) (Constant v2) = v1 == v2
  (==) (Expr name1 ts1 _) (Expr name2 ts2 _) = name1 == name2 && ts1 == ts2
  (==) _ _ = False

-- Eq instance to be used for testing; does not properly handle Literal and Function
instance (Eq a, Eq b) => (Eq (Atom a b)) where
  (==) (Literal p1 ts1 _) (Literal p2 ts2 _) = p1 == p2 && ts1 == ts2
  (==) (Value v1) (Value v2) = v1 == v2
  (==) (Datalog.Function n1 ts1 _) (Datalog.Function n2 ts2 _) = n1 == n2 && ts1 == ts2
  (==) _ _ = False


-- instance Show
instance (Show a) => (Show (Term a)) where
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
  goldenProgramTest "Translate LocalLookup"
    (Program $ translateToClause' SaigaPicoJava.localLookupAttr)
    "testfiles/SaigaDatalog/localLookup.dl.golden"
    "testfiles/SaigaDatalog/localLookup.dl.out",

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

  -- translate the localLookup program
  let dlLocalLookup = translateProgram $ SaigaPicoJava.localLookupProgram SaigaPicoJava.boolDecl
  in goldenProgramTest "Translate the localLookup program" dlLocalLookup
    "testfiles/SaigaDatalog/localLookupProgram.dl.golden"
    "testfiles/SaigaDatalog/localLookupProgram.dl.out",

  -- translate the localLookup program
  let dlLocalLookup = translateProgram $ SaigaPicoJava.localLookupProgram SaigaPicoJava.boolDecl
      dlLocalLookupDemand = transformProgram dlLocalLookup (initialDemand "LocalLookup" (Set.fromList [0, 1]))
  in goldenProgramTest "Translate the demand-transformed localLookup program" dlLocalLookupDemand
    "testfiles/SaigaDatalog/localLookupProgram-demand.dl.golden"
    "testfiles/SaigaDatalog/localLookupProgram-demand.dl.out",

  -- translate the findDecl program
  let dlFindDecl = translateProgram $ SaigaPicoJava.findDeclProgram SaigaPicoJava.boolDecl
      dlFindDeclDemand = transformProgram dlFindDecl (initialDemand "finddecl" (Set.fromList [0, 1]))
  in goldenProgramTest "Translate the demand-transformed finddecl program" dlFindDeclDemand
    "testfiles/SaigaDatalog/finddeclProgram-demand.dl.golden"
    "testfiles/SaigaDatalog/finddeclProgram-demand.dl.out",

  -- translate the CFG program
  let dlCFG = translateProgram $ CFGLang.cfgProgram CFGLang.unknownDecl
  in goldenProgramTest "Translate the demand-transformed CFG program" dlCFG
    "testfiles/SaigaDatalog/cfg.dl.golden"
    "testfiles/SaigaDatalog/cfg.dl.out",

  -- translate and transform the CFG program
  let dlCFG = translateProgram $ CFGLang.cfgProgram CFGLang.unknownDecl
      dlCFGDemand = transformProgram dlCFG (initialDemand "Nullable" (Set.fromList [0]))
  in goldenProgramTest "Translate the demand-transformed CFG program" dlCFGDemand
    "testfiles/SaigaDatalog/cfg-demand.dl.golden"
    "testfiles/SaigaDatalog/cfg-demand.dl.out",

  -- list predicates not defined in the Saiga/PicoJava program (i.e. EDB predicates)
  let dlPicoJava = flattenProgram $ translateProgram $ SaigaPicoJava.picoJavaProgram SaigaPicoJava.boolDecl
      preds = predicates dlPicoJava
      idbPreds = idbPredicates dlPicoJava
      edbPreds = Set.difference preds idbPreds
  in
    testCase "List EDB predicates" $ edbPreds @?= edbPredsPicoJava,

  -- list predicates not defined in the transformed Saiga/PicoJava program (i.e. EDB predicates); they
  -- should be the same as for the original program
  let dlPicoJava = translateProgram $ SaigaPicoJava.picoJavaProgram SaigaPicoJava.boolDecl
      dlPicoJavaDemand = transformProgram dlPicoJava (initialDemand "Type" (Set.fromList [0]))
      preds = predicates dlPicoJavaDemand
      idbPreds = idbPredicates dlPicoJavaDemand
      edbPreds = Set.difference preds idbPreds
  in
    testCase "List EDB predicates in demand-transformed program" $ edbPreds @?= edbPredsPicoJava,

  --
  let dlPicoJava = translateProgram $ SaigaPicoJava.picoJavaProgram SaigaPicoJava.boolDecl
      dlPicoJavaDemand = transformProgram dlPicoJava (initialDemand "Decl" (Set.fromList [0]))
      preds = predicates dlPicoJavaDemand
      idbPreds = idbPredicates dlPicoJavaDemand
      edbPreds = Set.difference preds idbPreds
  in
    testCase "List EDB predicates in demand-transformed program" $ edbPreds @?= edbPredsPicoJava,

  --
  let dlLocalLookup = flattenProgram $ translateProgram $ SaigaPicoJava.localLookupProgram SaigaPicoJava.boolDecl
      preds = predicates dlLocalLookup
      idbPreds = idbPredicates dlLocalLookup
      edbPreds = Set.difference preds idbPreds
  in
    testCase "List EDB predicates is LocalLookup program" $ edbPreds @?=
    Set.fromList ["Children","Kind","Name",
                  "mkUnknownDecl","predefs"],

  --
  let dlLocalLookup = translateProgram $ SaigaPicoJava.localLookupProgram SaigaPicoJava.boolDecl
      dlLocalLookupDemand = transformProgram dlLocalLookup (initialDemand "LocalLookup" (Set.fromList [0, 1]))
      preds = predicates dlLocalLookupDemand
      idbPreds = idbPredicates dlLocalLookupDemand
      edbPreds = Set.difference preds idbPreds
  in
    testCase "List EDB predicates is demand-transformed LocalLookup program" $ edbPreds @?=
    Set.fromList ["Children","Kind","Name",
                  "mkUnknownDecl","predefs", "d_LocalLookup_bbf"],

  -- toy examples
  let dlToySqrt = translateProgram $ PlaygroundLang.playProgram SaigaPicoJava.boolDecl
      demand = Map.fromList [
        ("Sqrt", Set.singleton $ Set.fromList [0, 1]),
        ("Sqrt2", Set.singleton $ Set.fromList [0, 1]),
        ("Sqrt3", Set.singleton $ Set.fromList [0, 1]),
        ("Sqrt5", Set.singleton $ Set.fromList [0, 1])]
      dlToySqrtDemand = transformProgram dlToySqrt demand
  in goldenProgramTest "Translate sqrt and demand-transform" dlToySqrtDemand
    "testfiles/SaigaDatalog/sqrt-demand.dl.golden"
    "testfiles/SaigaDatalog/sqrt-demand.dl.out",

  let dlToySqrt = translateProgram $ PlaygroundLang.playProgram SaigaPicoJava.boolDecl
  in goldenProgramTest "Translate sqrt" dlToySqrt
    "testfiles/SaigaDatalog/sqrt.dl.golden"
    "testfiles/SaigaDatalog/sqrt.dl.out"
  ]

edbPredsPicoJava = Set.fromList ["Child","Children","Kind","Name",
                                  "Parent",
                                  "mkUnknownClass","mkUnknownDecl","predefs"]
