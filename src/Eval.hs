module Eval(Context, query, eval, emptyContext, loadFromCSV, storeToCSV, GroundTerm(..)) where

import qualified Data.Map.Strict as Map
import qualified Data.Semiring as Semiring
import Data.Maybe
import qualified Text.CSV as CSV
import System.IO
import Debug.Trace (trace)

import Datalog

type Relation a b = Map.Map [a] b

type Context a b = Map.Map Predicate (Relation a b)

type Binding a = Map.Map String a


emptyBinding :: Binding a
emptyBinding = Map.empty


immediateConsequence :: (Ord a, Eq s, Semiring.Semiring s) => Clause a s -> Context a s -> Context a s
immediateConsequence c ctx =
  let bindings = immediateConsequence' ctx c.body (emptyBinding, Semiring.one)
  in foldr (\at -> insertAtoms at bindings) ctx c.heads

applyTrace :: Trace a s -> (Binding a, s) -> s
applyTrace (Trace ts f) (b, v) =
  let args = map (\case (Variable name) -> b Map.! name
                        (Constant c) -> c) ts
  in f args v

insertAtom :: Ord a => Atom a s -> (Binding a, s) -> Context a s -> Context a s
insertAtom (Literal p ts) (b, v) ctx =
  let rel = Map.findWithDefault Map.empty p ctx
      tpl = map (\case (Variable name) -> b Map.! name
                       (Constant c) -> c) ts
      rel' = Map.insert tpl v rel
  in Map.insert p rel' ctx
insertAtom at _ _ = error "Unexpected head "


insertAtoms :: Ord a => Atom a s -> [(Binding a, s)] -> Context a s -> Context a s
insertAtoms at bs ctx = foldr (insertAtom at) ctx bs

immediateConsequence' :: (Eq a, Eq s, Semiring.Semiring s) => Context a s -> [Atom a s] -> (Binding a, s) -> [(Binding a, s)]
immediateConsequence' _ [] (b, v) = [(b, v)]
immediateConsequence' ctx (a:as) (b, v) =
  let bindingsNext = filterBindings (b, v) $ case a of Literal _ _ -> (lookupLiteral ctx a)
                                                       Value s -> [(emptyBinding, s)]
                                                       f@(Function _ _) -> [(emptyBinding, applyFunction b f)]
  in concatMap (immediateConsequence' ctx as) bindingsNext

applyFunction :: Binding a -> Atom a s -> s
applyFunction b (Function ts f) =
  let args = map (\case (Variable name) -> b Map.! name
                        (Constant c) -> c) ts
  in f args

lookupLiteral :: Context a s -> Atom a s -> [(Binding a, s)]
lookupLiteral ctx (Literal p ts) =
  let rel = Map.findWithDefault Map.empty p ctx
      toBinding ks s = (foldr (\(t, k) b -> case t of (Variable name) -> Map.insert name k b
                                                      (Constant _) -> error "No constants in literals!")
                        emptyBinding (zip ts ks),
                        s)
  in Map.foldrWithKey (\ks v bs -> (toBinding ks v) : bs) [] rel


joinBindings :: Eq a => Binding a -> Binding a -> Maybe (Binding a)
joinBindings b1 b2 =
  let common1 = Map.intersection b1 b2
      common2 = Map.intersection b2 b1
  in
    if common1 == common2 then Just $ Map.union b1 b2
    else Nothing

filterBindings :: (Semiring.Semiring s, Eq s, Eq a) => (Binding a, s) -> [(Binding a, s)] -> [(Binding a, s)]

filterBindings (b, s) bs = [(fromJust $ joinBindings b b', Semiring.times s s') | (b', s') <- bs,
                             isJust $ joinBindings b b',
                             Semiring.times s s' /= Semiring.zero]

eval :: (Ord a, Semiring.Semiring s, Eq s) => Program a s -> Context a s -> Context a s
eval p@(Program cs) ctx =
  let ctx' = foldr immediateConsequence ctx cs
  in if ctx == ctx' then ctx
     else eval p ctx'

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
