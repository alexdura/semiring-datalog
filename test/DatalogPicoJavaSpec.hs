module DatalogPicoJavaSpec (datalogPicoJavaTests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified PicoJava
import qualified Saiga
import qualified Eval
import qualified SaigaPicoJava
import Util
import Control.Monad.State.Strict
import qualified PicoJava
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified DemandTransformation
import qualified SaigaDatalog
import Data.Maybe
import SaigaPicoJavaSpec

type Relation = Eval.Relation (Saiga.Domain (String, Int)) Bool
type Context = Eval.Context (Saiga.Domain (String, Int)) Bool



collect' :: (PicoJava.AST a -> s -> s) -> PicoJava.AST a -> State s ()
collect' update n = do
  forM_ n.children (collect' update)
  s <- get
  put $ update n s

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
predefs = Map.singleton [Saiga.DList [Saiga.DNode SaigaPicoJava.boolDecl]] True


contextFromAST :: PicoJava.AST (String, Int) -> Context
contextFromAST ast =
  Eval.addRelation "Parent" (parent ast Map.empty) $
  Eval.addRelation "Child" (child ast Map.empty) $
  Eval.addRelation "Child" (children ast Map.empty) $
  Eval.addRelation "Name" (name ast Map.empty) $
  Eval.addRelation "Kind" (kind ast Map.empty) $
  Eval.addRelation "mkUnknownDecl" unknownClassRel $
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


dlPicoJava = SaigaDatalog.translateProgram $ SaigaPicoJava.picoJavaProgram SaigaPicoJava.boolDecl

datalogPicoJavaTests = testGroup "Tests for the Datalog version of PicoJava" [
  let nodeId = 15
      dlEvalCtx = Eval.addRelation "Decl_bf" (Map.singleton ([Saiga.DNode $ fromJust $ findNodeById program3Ast nodeId]) True) $
                  contextFromAST program3Ast
      demand = DemandTransformation.initialDemand "Decl" (Set.fromList [0])
      dlPicoJavaDemand = DemandTransformation.transformProgram dlPicoJava demand
      dlEvalCtx' = Eval.eval dlPicoJavaDemand dlEvalCtx
  in testCase "Decl 1" $ (Eval.query "Decl" dlEvalCtx') @?= []
  ]
