module Datalog where

import qualified Data.Map.Strict as Map
import Data.Semiring

data Program a b = Program [Clause a b]

data Clause a b = Clause [Atom a b] [Atom a b]

data Atom a b = Literal Predicate [Term a]
              | Value b

data Term a = Variable String
            | Constant a


type Predicate = String

type Relation a b = Map.Map [a] b

type Context a b = Map.Map Predicate (Relation a b)

type Binding a = Map.Map String a

emptyBinding :: Binding a
emptyBinding = Map.empty

immediateConsequence :: Context a b -> Clause a b -> Context a b
immediateConsequence = undefined

-- bindVars :: Semiring b => Context a b -> [(Binding a, b)] -> Atom a b -> [(Binding a, b)]

lookup :: Context a s -> Atom a s -> [(Binding a, s)]
lookup _ (Value s) = [(emptyBinding, s)]
lookup ctx (Literal p ts) =
  let rel = ctx Map.! p
      toBinding ks s = (foldr (\(t, k) b -> case t of (Variable name) -> Map.insert name k b
                                                      (Constant _) -> b)
                        emptyBinding (zip ts ks),
                        s)
  in Map.foldrWithKey (\ks v bs -> (toBinding ks v) : bs) [] rel
