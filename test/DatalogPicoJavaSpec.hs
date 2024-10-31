module DatalogPicoJavaSpec (datalogPicoJavaTests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified PicoJava
import qualified Saiga
import qualified Eval
import qualified SaigaPicoJava
import Util
import Control.Monad.State.Strict
import PicoJava
import qualified Data.Map.Strict as Map

type Relation = Eval.Relation (Saiga.Domain (String, Int)) Bool
type Context = Eval.Context (Saiga.Domain (String, Int)) Bool


datalogPicoJavaTests = testGroup "Tests for the Datalog version of PicoJava" [
                                                                             ]

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

predefs :: Relation
predefs = Map.singleton [Saiga.DList [Saiga.DNode SaigaPicoJava.boolDecl]] True


contextFromAST :: PicoJava.AST (String, Int) -> Context
contextFromAST ast = undefined
