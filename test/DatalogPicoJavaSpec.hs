module DatalogPicoJavaSpec (datalogPicoJavaTests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified PicoJava
import qualified AST
import qualified Saiga
import qualified Eval
import qualified SaigaPicoJava
import Util
import Control.Monad.State.Strict
import qualified PicoJava
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified DemandTransformation
import qualified SaigaToDatalogTranslation
import Data.Maybe
import SaigaPicoJavaSpec
import qualified CFGLang
import qualified SaigaCFGLangSpec

type Relation = Eval.Relation (Saiga.Domain (String, Int)) Bool
type Context = Eval.Context (Saiga.Domain (String, Int)) Bool



collect' :: (PicoJava.AST a -> s -> s) -> PicoJava.AST a -> State s ()
collect' update n = do
  forM_ n.children (collect' update)
  s <- get
  put $ update n s


collect :: (PicoJava.AST a -> b -> b) -> PicoJava.AST a -> b -> b
collect f ast = execState (collect' f ast)

parent :: PicoJava.AST (String, Int) -> Relation -> Relation
parent n r =
  let tpl c = [Saiga.DNode c, Saiga.DNode n] in
    foldr (\c r' -> Map.insert (tpl c) True r') r (PicoJava.children n)

child :: PicoJava.AST (String, Int) -> Relation -> Relation
child n r =
  let tpl c = [Saiga.DNode n, Saiga.DInt $ fst c, Saiga.DNode $ snd c] in
    foldr (\c r' -> Map.insert (tpl c) True r') r (zip [0..] n.children)

children :: PicoJava.AST (String, Int) -> Relation -> Relation
children n r =
  let tpl = [Saiga.DNode n, Saiga.DList $ Saiga.DNode <$> n.children] in
    Map.insert tpl True r

name :: PicoJava.AST (String, Int) -> Relation -> Relation
name n r =
  let tpl = [Saiga.DNode n, Saiga.DString $ n.token] in
    Map.insert tpl True r

kind :: PicoJava.AST (String, Int) -> Relation -> Relation
kind n r =
  let tpl = [Saiga.DNode n, Saiga.DString $ fst n.kind] in
    Map.insert tpl True r


unknownDeclRel :: Relation
unknownDeclRel = Map.singleton [Saiga.DNode SaigaPicoJava.unknownDecl] True

unknownClassRel :: Relation
unknownClassRel = Map.singleton [Saiga.DNode SaigaPicoJava.unknownClass] True

predefs :: Relation
predefs = Map.singleton [Saiga.DList [Saiga.DNode SaigaPicoJava.boolDecl,
                                      Saiga.DNode SaigaPicoJava.intDecl]] True


builtins = [SaigaPicoJava.intDecl,
            SaigaPicoJava.boolDecl,
            SaigaPicoJava.unknownClass,
            SaigaPicoJava.unknownDecl]

contextFromBuiltins f = (foldr (.) id $ (map f builtins)) Map.empty

contextFromAST :: AST.AST (String, Int) -> Context
contextFromAST ast =
  Eval.addRelation "Parent" (collect parent ast $
                             contextFromBuiltins parent) $
  Eval.addRelation "Child" (collect child ast $
                            contextFromBuiltins child) $
  Eval.addRelation "Children" (collect children ast $
                               contextFromBuiltins children) $
  Eval.addRelation "Name" (collect name ast $
                           contextFromBuiltins name) $
  Eval.addRelation "Kind" (collect kind ast $
                           contextFromBuiltins kind) $
  Eval.addRelation "mkUnknownDecl" unknownDeclRel $
  Eval.addRelation "mkUnknownClass" unknownClassRel $
  Eval.addRelation "predefs" predefs $
  Eval.emptyContext

instance Read (Saiga.Domain (String, Int)) where
  readsPrec = undefined

instance Eval.DatalogGroundTerm (Saiga.Domain (String, Int)) where
  parse = undefined
  unparse = show
  firstIndex = Saiga.DInt 0
  nextIndex = \(Saiga.DInt n) -> Saiga.DInt $ n + 1


dlPicoJava = SaigaToDatalogTranslation.translateProgram $ SaigaPicoJava.picoJavaProgram SaigaPicoJava.boolDecl

dlLocalLookup = SaigaToDatalogTranslation.translateProgram $ SaigaPicoJava.localLookupProgram SaigaPicoJava.boolDecl

dlCFGProgram = SaigaToDatalogTranslation.translateProgram $ CFGLang.cfgProgram CFGLang.unknownDecl

datalogPicoJavaTests = testGroup "Tests for the Datalog version of PicoJava" [
  let nodeId = 0
      dlEvalCtx = Eval.addRelation "d_Nullable_bf" (Map.singleton [node] True) $
                  contextFromAST SaigaCFGLangSpec.cfg1'
      node = Saiga.DNode $ fromJust $ findNodeById SaigaCFGLangSpec.cfg1' nodeId
      demand = DemandTransformation.initialDemand "Nullable" (Set.fromList [0])
      dlCFGProgramDemand = DemandTransformation.transformProgram dlCFGProgram demand
      dlEvalCtx' = Eval.eval dlCFGProgramDemand dlEvalCtx
  in testCase "Nullable 1" $ do
     lookup [node, Saiga.DBool True] (Eval.query "Nullable" dlEvalCtx') @?= Just True
     lookup [node, Saiga.DBool False] (Eval.query "Nullable" dlEvalCtx') @?= Just True,
     -- (Eval.query "Nullable" dlEvalCtx') @?= [],

  let nodeId = 11
      dlEvalCtx = Eval.addRelation "d_Nullable_bf" (Map.singleton [node] True) $
                  contextFromAST SaigaCFGLangSpec.cfg1'
      node = Saiga.DNode $ fromJust $ findNodeById SaigaCFGLangSpec.cfg1' nodeId
      demand = DemandTransformation.initialDemand "Nullable" (Set.fromList [0])
      dlCFGProgramDemand = DemandTransformation.transformProgram dlCFGProgram demand
      dlEvalCtx' = Eval.eval dlCFGProgramDemand dlEvalCtx
  in testCase "Nullable 2" $ do
     lookup [node, Saiga.DBool False] (Eval.query "Nullable" dlEvalCtx') @?= Just True
     lookup [node, Saiga.DBool True] (Eval.query "Nullable" dlEvalCtx') @?= Just True,
     -- (Eval.query "Nullable" dlEvalCtx') @?= [],

  let nodeId = 20
      dlEvalCtx = Eval.addRelation "d_Nullable_bf" (Map.singleton [node] True) $
                  contextFromAST SaigaCFGLangSpec.cfg1'
      node = Saiga.DNode $ fromJust $ findNodeById SaigaCFGLangSpec.cfg1' nodeId
      demand = DemandTransformation.initialDemand "Nullable" (Set.fromList [0])
      dlCFGProgramDemand = DemandTransformation.transformProgram dlCFGProgram demand
      dlEvalCtx' = Eval.eval dlCFGProgramDemand dlEvalCtx
  in testCase "Nullable 3" $ do
     lookup [node, Saiga.DBool False] (Eval.query "Nullable" dlEvalCtx') @?= Just True
     lookup [node, Saiga.DBool True] (Eval.query "Nullable" dlEvalCtx') @?= Nothing,

  let nodeId = 15
      dlEvalCtx = Eval.addRelation "d_Decl_bf" (Map.singleton ([Saiga.DNode $ fromJust $ findNodeById program3Ast nodeId]) True) $
                  contextFromAST program3Ast
      demand = DemandTransformation.initialDemand "Decl" (Set.fromList [0])
      dlPicoJavaDemand = DemandTransformation.transformProgram dlPicoJava demand
      dlEvalCtx' = Eval.eval dlPicoJavaDemand dlEvalCtx
  in testCase "Decl 1" $ show (Eval.query "Decl" dlEvalCtx') @?=
  "[([DNode (AST {kind = (\"Dot\",15), token = \".\", children = [AST {kind = (\"Use\",13), token = \"z\", children = []},AST {kind = (\"Use\",14), token = \"y\", children = []}]}),DNode (AST {kind = (\"VarDecl\",5), token = \"y\", children = [AST {kind = (\"Use\",4), token = \"bool\", children = []}]})],True),([DNode (AST {kind = (\"Use\",11), token = \"B\", children = []}),DNode (AST {kind = (\"ClassDecl\",10), token = \"B\", children = [AST {kind = (\"UnknownClass\",3), token = \"_unknown_\", children = []},AST {kind = (\"Block\",9), token = \"\", children = [AST {kind = (\"VarDecl\",5), token = \"y\", children = [AST {kind = (\"Use\",4), token = \"bool\", children = []}]},AST {kind = (\"Stmt\",8), token = \"\", children = [AST {kind = (\"Use\",6), token = \"x\", children = []},AST {kind = (\"Use\",7), token = \"y\", children = []}]}]}]})],True),([DNode (AST {kind = (\"Use\",13), token = \"z\", children = []}),DNode (AST {kind = (\"VarDecl\",12), token = \"z\", children = [AST {kind = (\"Use\",11), token = \"B\", children = []}]})],True),([DNode (AST {kind = (\"Use\",14), token = \"y\", children = []}),DNode (AST {kind = (\"VarDecl\",5), token = \"y\", children = [AST {kind = (\"Use\",4), token = \"bool\", children = []}]})],True)]",


  -- finddecl(_arg_0, _arg_1, _e) <- d_finddecl_bbf(_arg_0, _arg_1),
  -- _a := DList [], _builtin_neq(_arg_1, _a),
  -- _d := _head(_arg_1), Name(_d, _c),
  -- _builtin_eq(_arg_0, _c),
  -- _e := _head(_arg_1)

  let nodeId = 6
      demand = DemandTransformation.initialDemand "LocalLookup" (Set.fromList [0, 1])
      dlEvalCtx = Eval.addRelation "d_LocalLookup_bbf"
                  (Map.singleton ([Saiga.DNode $ fromJust $ findNodeById program2Ast nodeId, Saiga.DString "B"]) True) $
                  contextFromAST program2Ast
      dlLocalLookupDemand = DemandTransformation.transformProgram dlLocalLookup demand
      dlEvalCtx' = Eval.eval dlLocalLookupDemand dlEvalCtx
  in testCase "Local lookup 1" $ show (Eval.query "LocalLookup" dlEvalCtx') @?=  "[([DNode (AST {kind = (\"Program\",6), token = \"\", children = [AST {kind = (\"ClassDecl\",2), token = \"A\", children = [AST {kind = (\"UnknownClass\",0), token = \"_unknown_\", children = []},AST {kind = (\"Block\",1), token = \"\", children = []}]},AST {kind = (\"ClassDecl\",5), token = \"B\", children = [AST {kind = (\"UnknownClass\",3), token = \"_unknown_\", children = []},AST {kind = (\"Block\",4), token = \"\", children = []}]}]}),DString \"B\",DNode (AST {kind = (\"ClassDecl\",5), token = \"B\", children = [AST {kind = (\"UnknownClass\",3), token = \"_unknown_\", children = []},AST {kind = (\"Block\",4), token = \"\", children = []}]})],True)]",


  let demand = DemandTransformation.initialDemand "finddecl" (Set.fromList [0, 1])
      dlEvalCtx = Eval.addRelation "d_finddecl_bbf" (Map.singleton ([Saiga.DString "bool", Saiga.DList [Saiga.DNode SaigaPicoJava.intDecl,
                                                                                                        Saiga.DNode SaigaPicoJava.boolDecl]]) True) $
                  contextFromAST program2Ast
      dlFindDeclDemand = DemandTransformation.transformProgram dlLocalLookup demand
      dlEvalCtx' = Eval.eval dlFindDeclDemand dlEvalCtx
  in testCase "finddecl 1" $ (show $ (Eval.query "d_finddecl_bbf" dlEvalCtx')) @?= "[([DString \"bool\",DList [DNode (AST {kind = (\"ClassDecl\",-11), token = \"int\", children = [AST {kind = (\"Block\",-12), token = \"\", children = []}]}),DNode (AST {kind = (\"ClassDecl\",-1), token = \"bool\", children = [AST {kind = (\"Block\",-2), token = \"\", children = []}]})]],True),([DString \"bool\",DList [DNode (AST {kind = (\"ClassDecl\",-1), token = \"bool\", children = [AST {kind = (\"Block\",-2), token = \"\", children = []}]})]],True)]"

  -- let demand = DemandTransformation.initialDemand "finddecl" (Set.fromList [0, 1])
  --     dlEvalCtx = Eval.addRelation "d_finddecl_bbf" (Map.singleton ([Saiga.DString "bool", Saiga.DList [Saiga.DNode SaigaPicoJava.intDecl,
  --                                                                                                       Saiga.DNode SaigaPicoJava.boolDecl]]) True) $
  --                 contextFromAST program2Ast
  --     dlFindDeclDemand = DemandTransformation.transformProgram dlLocalLookup demand
  -- in testCase "finddecl 1" $ do
  --   Eval.evalStep dlFindDeclDemand dlEvalCtx
  --   return ()
  ]
