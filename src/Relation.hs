{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Relation (Relation) where

import qualified Data.Map.Strict as Map
import qualified Data.Semiring as Semiring

data Relation a b = Relation {
  arity :: Int,
  indexToMap :: Map.Map [Int] (Map.Map [a] b)
  } deriving (Show)

insert :: Ord a => Semiring.Semiring b => Relation a b -> ([a], b) -> Relation a b
insert rel t = rel {indexToMap = Map.mapWithKey (insertWithIndex t) rel.indexToMap}

lookup :: Ord a => [a] -> [Int] -> Relation a b -> [([a], b)] -> Relation a b

lookup ks idx r =
  let fullIndex = idx ++ [i | i <- [0 .. r.arity - 1], not (i `elem` idx)] in
    case r.indexToMap Map.!? fullIndex of
      Just m -> error "Not implemented"
      Nothing -> error "Not implemented"


insertWithIndex :: Ord a => Semiring.Semiring b => ([a], b) -> [Int] ->  Map.Map [a] b -> Map.Map [a] b
insertWithIndex (ks, v) idx m  =
  let key = (ks !!) <$> idx in Map.insertWith Semiring.plus key v m
