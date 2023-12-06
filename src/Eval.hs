module Eval(Context, query, eval, emptyContext) where

import qualified Data.Map.Strict as Map
import qualified Data.Semiring as Semiring
import Data.Maybe

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
  let bindingsNext = filterBindings (b, v) (lookupAtom ctx a)
  in concatMap (immediateConsequence' ctx as) bindingsNext


-- bindVars :: Semiring b => Context a b -> [(Binding a, b)] -> Atom a b -> [(Binding a, b)]

lookupAtom :: Context a s -> Atom a s -> [(Binding a, s)]
lookupAtom _ (Value s) = [(emptyBinding, s)]
lookupAtom ctx (Literal p ts) =
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
