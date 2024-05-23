module DemandTransformation (programPredicateDemand, PredicateDemand, DemandPattern) where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Datalog

import Data.Set (Set)
import Data.Map (Map)
import Data.List ( findIndices, inits, tails )


type DemandPattern = Set Int -- bound indices

type PredicateDemand = Map Predicate (Set DemandPattern)

addDemand :: Predicate -> DemandPattern -> PredicateDemand -> PredicateDemand
addDemand p pat d = let pd = Map.findWithDefault Set.empty p d in
                      Map.insert p (Set.insert pat pd) d

getDemand :: Predicate -> PredicateDemand -> Set DemandPattern
getDemand = Map.findWithDefault Set.empty


headDemandedVars :: Atom a b -- head atom
                 -> DemandPattern -- demand pattern for the predicate
                 -> Set String -- demanded variables

headDemandedVars (Literal pred ts _) pat =
  let boundTs = (ts !!) <$> Set.toList pat in
    foldr (\case
              Variable v -> Set.insert v
              _ -> id
          ) Set.empty boundTs
headDemandedVars _ _ = Set.empty


bodyDemandedVars :: Atom a b -- body atom
                 -> Set String -- literals bound by the atom
bodyDemandedVars (Literal pred ts _) = Set.fromList $
  concatMap (\case Variable v -> [v]
                   _ -> []) ts
bodyDemandedVars _ = Set.empty

bodyDemand :: Atom a s -> Set String -> DemandPattern
bodyDemand (Literal _ ts _) boundVars = Set.fromList $
  findIndices (\case
                  Variable v -> Set.member v boundVars
                  _ -> False) ts
bodyDemand _ _ = Set.empty

bodyPredicateDemand :: Set String -- bound variables
                   -> [Atom a s] -- list of body atoms
                   -> PredicateDemand -- initial predicate demand
                   -> PredicateDemand -- updated predicate demand
bodyPredicateDemand boundVars (a : as) pd =
  let demandPattern = bodyDemand a boundVars
      boundVars' = Set.union (bodyDemandedVars a) boundVars
      pd' = case a of (Literal p _ _ ) -> addDemand p demandPattern pd
                      _ -> pd
  in bodyPredicateDemand boundVars' as pd'
bodyPredicateDemand _ [] pd = pd

clausePredicateDemand :: Clause a s -> PredicateDemand -> PredicateDemand
clausePredicateDemand (Clause h []) pd = pd
clausePredicateDemand (Clause [l@(Literal p _ _)] bs) pd =
  let hDemand = getDemand p pd
      demandUpdates = (\dp -> bodyPredicateDemand (headDemandedVars l dp) bs) <$> Set.toList hDemand in
    foldr (.) id demandUpdates $ pd
clausePredicateDemand _ _ = error "Predicate demand defined only for rules with a single atom in the head."

programPredicateDemand' :: Program a s -> PredicateDemand -> PredicateDemand
programPredicateDemand' (Program cs) =
  foldr (.) id $ map clausePredicateDemand cs

programPredicateDemand :: Program a s -> PredicateDemand -> PredicateDemand
programPredicateDemand p d =
  let d' = programPredicateDemand' p d in
    if d' == d then d
    else programPredicateDemand' p d'

bodyDemandPatterns :: Set String -- bound variables
                   -> [Atom a s] -- body atoms
                   -> [DemandPattern] -- body bound variables

bodyDemandPatterns _ [] = []
bodyDemandPatterns boundVars (a : as) =
  case a of (Literal _ ts _) -> let boundVars' = Set.union (bodyDemandedVars a) boundVars
                                in bodyDemand a boundVars : bodyDemandPatterns boundVars' as
            _ -> Set.empty : bodyDemandPatterns boundVars as

genDemandLiteral :: Atom a s -- original atom
                 -> DemandPattern -- demand pattern
                 -> Atom a s -- demand atom

genDemandLiteral (Literal p ts f) dp =
  let name = "d_" ++ p ++ "_" ++ [if i `Set.member` dp then 'b' else 'f' | i <- [0..length ts - 1]]
      ts' = (ts !! ) <$> Set.toAscList dp in
    Literal p ts' f
genDemandLiteral a _ = a

genDemandRules :: Atom a s -- head atom
               -> DemandPattern -- head demand
               -> [Atom a s] -- body atoms
               -> [Clause a s]

genDemandRules _ _ [] = []
genDemandRules h@(Literal p ts _) hdp bs =
  let boundVars = headDemandedVars h hdp
      bodyDemand = zip3 (tails bs) (inits bs) (bodyDemandPatterns boundVars bs) in
    concatMap (\((h_i:_), hs, dp) -> case h_i of Literal {} -> [Clause [(genDemandLiteral h_i dp)] (genDemandLiteral h hdp : hs)]
                                                 _ -> []) bodyDemand
