module Eval(Context, query, eval, evalStep, emptyContext, loadFromCSV, storeToCSV, GroundTerm(..)) where

import qualified Data.Map.Strict as Map
import Data.Semiring (Semiring)
import qualified Data.Semiring as Semiring
import Data.Maybe
import qualified Text.CSV as CSV
import Control.Monad ( foldM )
import System.IO
import Debug.Trace (trace)

import Datalog

type Relation a b = Map.Map [a] b

type Context a b = Map.Map Predicate (Relation a b)

type Binding a = Map.Map String a


emptyBinding :: Binding a
emptyBinding = Map.empty


immediateConsequence :: (Ord a, Eq s, Semiring s) => Clause a s -> Context a s -> Context a s
immediateConsequence c ctx =
  let bindings = immediateConsequence' ctx c.body (emptyBinding, Semiring.one)
      bindings' = (\b -> (fst b, applyTrace c.trace b)) <$> bindings
  in foldr (\at -> insertAtoms at bindings') ctx c.heads

applyTrace :: Trace a s -> (Binding a, s) -> s
applyTrace (Trace ts f) (b, v) =
  let args = map (\case (Variable name) -> fromMaybe (error $ "Undefined variable '" ++ name ++ "'.") $ b Map.!? name
                        (Constant c) -> c) ts
  in f args v

insertAtom :: (Ord a, Semiring s) => Atom a s -> (Binding a, s) -> Context a s -> Context a s
insertAtom (Literal p ts _) (b, v) ctx =
  let rel = Map.findWithDefault Map.empty p ctx
      tpl = map (\case (Variable name) -> fromMaybe (error $ "Undefined variable '" ++ name ++ "'.") $ b Map.!? name
                       (Constant c) -> c
                       (Expr opds f) -> evalExpr b opds f) ts
      rel' = Map.insertWith (Semiring.+) tpl v rel
  in Map.insert p rel' ctx
insertAtom at _ _ = error "Unexpected head "


insertAtoms :: (Ord a, Semiring s) => Atom a s -> [(Binding a, s)] -> Context a s -> Context a s
insertAtoms at bs ctx = foldr (insertAtom at) ctx bs

immediateConsequence' :: (Eq a, Eq s, Semiring s) => Context a s -> [Atom a s] -> (Binding a, s) -> [(Binding a, s)]
immediateConsequence' _ [] (b, v) = [(b, v)]
immediateConsequence' ctx (a:as) (b, v) =
  let bindingsNext = case a of Literal {} -> lookupLiteral ctx a b
                               Value s -> [(b, s)]
                               f@(Function _ _) -> [(b, applyFunction b f)]
  in concatMap (immediateConsequence' ctx as) [(b', v Semiring.* v') | (b', v') <- bindingsNext, v Semiring.* v' /= Semiring.zero]

applyFunction :: Binding a -> Atom a s -> s
applyFunction b (Function ts f) =
  let args = map (\case (Variable name) -> fromMaybe (error $ "Undefined variable '" ++ name ++ "'.") $ b Map.!? name
                        (Constant c) -> c
                        (Expr _ _) -> error "No Expr in Function") ts

  in f args

lookupTerm :: Eq a => Binding a -> (Term a s, a) -> Maybe (Binding a)
lookupTerm b (t, k) = case t of (Variable name) -> case Map.lookup name b of
                                                     Just v -> if v == k then Just b else Nothing -- name already bound, check equality
                                                     Nothing -> Just $ Map.insert name k b -- name not bound, bind now
                                (Constant c) -> if c == k then Just b else Nothing
                                (Expr opds f) -> if evalExpr b opds f == k then Just b else Nothing

evalExpr :: Binding a -> [Term a b] -> ([a] -> a) -> a
evalExpr b opds f = f $ map (\case (Variable name) -> fromMaybe (error $ "Undefined variable '" ++ name ++ "'.") $ b Map.!? name
                                   (Constant c) -> c
                                   (Expr opds' f') -> evalExpr b opds' f') opds

lookupLiteral :: Eq a => Context a s -> Atom a s -> Binding a -> [(Binding a, s)]
lookupLiteral ctx (Literal p ts f) b =
  let rel = Map.findWithDefault Map.empty p ctx
  in  Map.foldrWithKey (\ks v bs -> case foldM lookupTerm b (zip ts ks) of
                                      Just b' -> (b', f v):bs
                                      Nothing -> bs) [] rel

eval :: (Ord a, Semiring s, Eq s) => Program a s -> Context a s -> Context a s
eval p@(Program cs) ctx =
  let ctx' = foldr immediateConsequence ctx cs
  in if ctx == ctx' then ctx
     else eval p ctx'

evalStep :: (Ord a, Show a, Semiring s, Eq s, Show s) => Program a s -> Context a s -> IO (Context a s)
evalStep p@(Program cs) ctx = do
  print $ show ctx
  _ <- getChar
  let ctx' = foldr immediateConsequence ctx cs
  if ctx == ctx' then return ctx
    else evalStep p ctx'


emptyContext :: Context a s
emptyContext = Map.empty

query :: String -> Context a s -> [([a], s)]
query pred ctx =
  case Map.lookup pred ctx of
    Just rel -> Map.toList rel
    Nothing -> []


class GroundTerm a where
  parse :: String -> a
  unparse :: a -> String

instance GroundTerm Int where
  parse = read
  unparse = show

instance GroundTerm String where
  parse = read
  unparse = show

loadFromCSV :: (GroundTerm a, Ord a) => Context a s -> String -> ([a] -> s) -> FilePath -> IO (Context a s)
loadFromCSV ctx name dflt path = do
  Right csv <- CSV.parseCSVFromFile path
  let rel = Map.findWithDefault Map.empty name ctx
      rel' = foldr (\row rel -> Map.insert (parse <$> row) (dflt (parse <$> row)) rel) rel (filter ([""] /=) csv)
  return $ Map.insert name rel' ctx

storeToCSV :: (GroundTerm a, Show s) => Context a s -> String -> FilePath -> IO ()
storeToCSV ctx name path =
  let csv =  (\(t, v) -> (unparse <$> t) ++ [show v])  <$> (Map.toList (Map.findWithDefault Map.empty name ctx)) in
    writeFile path (CSV.printCSV csv)
