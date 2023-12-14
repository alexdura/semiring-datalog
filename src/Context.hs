module Context(ContextValue(..), ContextSemiring2, push, ctx2) where

import Data.Semiring
import Data.Set (Set)
import qualified Data.Set as Set

data ContextValue a = ContextValue a
                    | Any
                    | None
                    deriving (Ord, Eq, Show)

data Context2 a = Context2 (ContextValue a) (ContextValue a)
                | Empty2
                deriving (Ord, Eq, Show)


-- instance Eq a => Monoid (Context2 a) where

contextValuePlus :: Eq a => ContextValue a -> ContextValue a -> ContextValue a
contextValuePlus Any v = v
contextValuePlus v Any = v
contextValuePlus None _ = None
contextValuePlus _ None = None
contextValuePlus (ContextValue v1) (ContextValue v2) = if v1 == v2 then (ContextValue v1) else None

instance Eq a => Semigroup (Context2 a) where
  (<>) Empty2 _ = Empty2
  (<>) _ Empty2 = Empty2
  (<>) (Context2 l0 l1) (Context2 r0 r1) = let e0 = contextValuePlus l0 l1
                                               e1 = contextValuePlus r0 r1
                                           in if e0 == None || e1 == None then Empty2
                                              else Context2 e0 e1

instance Eq a => Monoid (Context2 a) where
  mempty = Context2 Any Any

class Monoid a => AbsorbingMonoid a where
  mabsorb :: a

instance Eq a => AbsorbingMonoid (Context2 a) where
  mabsorb = Empty2

newtype Monoid a => MonoidSemiring a = MonoidSemiring {toSet::Set a}
fromSet :: AbsorbingMonoid a => Set a -> MonoidSemiring a
fromSet = MonoidSemiring

instance (Eq a, Monoid a) => Eq (MonoidSemiring a) where
  (==) m1 m2 = m1.toSet == m2.toSet

instance (Show a, Monoid a) => Show (MonoidSemiring a) where
  show = show . toSet

instance (AbsorbingMonoid a, Ord a) => Semiring (MonoidSemiring a) where
  plus (MonoidSemiring l) (MonoidSemiring r) = MonoidSemiring $ Set.union l r
  times (MonoidSemiring l) (MonoidSemiring r) = MonoidSemiring $ Set.filter (mabsorb /= ) $ Set.map (uncurry (<>)) (Set.cartesianProduct l r)
  zero = MonoidSemiring Set.empty
  one = MonoidSemiring $ Set.singleton mempty

type ContextSemiring2 a = MonoidSemiring (Context2 a)

push' :: ContextValue a -> Context2 a -> Context2 a
push' None _ = Empty2
push' _ Empty2 = Empty2
push' v (Context2 v1 _) = Context2 v v1

push :: Ord a => ContextValue a -> ContextSemiring2 a -> ContextSemiring2 a
push v = fromSet . Set.filter (Empty2 /= ) . Set.map (push' v) . toSet

ctx2 :: Eq a => a -> a -> ContextSemiring2 a
ctx2 x y = fromSet $ Set.singleton $ Context2 (ContextValue x) (ContextValue y)
