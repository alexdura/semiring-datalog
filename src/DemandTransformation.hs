module DemandTransformation (programPredicateDemand, PredicateDemand, DemandPattern, genDemandRules,
                             transformProgram, flattenProgram,
                             initialDemand, predicates, idbPredicates) where

import qualified Debug.Trace as Trace

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
bodyDemandedVars (Bind _ (Variable v)) = Set.singleton v
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
    else programPredicateDemand p d'

bodyDemandPatterns :: Set String -- bound variables
                   -> [Atom a s] -- body atoms
                   -> [DemandPattern] -- body bound variables

bodyDemandPatterns _ [] = []
bodyDemandPatterns boundVars (a : as) =
  case a of (Literal _ ts _) -> let boundVars' = Set.union (bodyDemandedVars a) boundVars
                                in bodyDemand a boundVars : bodyDemandPatterns boundVars' as
            (Bind _ (Variable v)) -> Set.empty : bodyDemandPatterns (Set.insert v boundVars) as
            _ -> Set.empty : bodyDemandPatterns boundVars as

genDemandLiteral :: Atom a s -- original atom
                 -> DemandPattern -- demand pattern
                 -> Atom a s -- demand atom

genDemandLiteral (Literal p ts f) dp =
  let name = "d_" ++ p ++ "_" ++ [if i `Set.member` dp then 'b' else 'f' | i <- [0..length ts - 1]]
      ts' = (ts !! ) <$> Set.toAscList dp in
    Literal name ts' f
genDemandLiteral a _ = a

genDemandRules :: Atom a s -- head atom
               -> DemandPattern -- head demand
               -> [Atom a s] -- body atoms
               -> (Predicate -> Bool) -- is EDB predicate?
               -> [Clause a s]

--genDemandRules _ _ [] _ = []
genDemandRules h@(Literal p ts _) hdp bs isEDB =
  let boundVars = headDemandedVars h hdp
      bodyDemand = zip3 (tails bs) (inits bs) (bodyDemandPatterns boundVars bs) in
    Clause [h] (genDemandLiteral h hdp : bs) :
    concatMap (\((h_i:_), hs, dp) -> case h_i of Literal p _ _  -> if (isEDB p) then [] -- no demand for EDB predicates
                                                                   else [Clause [(genDemandLiteral h_i dp)] (genDemandLiteral h hdp : hs)]
                                                 _ -> []) bodyDemand

flattenProgram :: Program a s
               -> Program a s -- a program where all the clauses have a single literal in the head
flattenProgram (Program cs) = Program $ concatMap flattenClause cs

flattenClause :: Clause a s -> [Clause a s]
flattenClause (Clause hs bs) = map (\h -> Clause [h] bs) hs

idbPredicates :: Program a s -> Set Predicate
idbPredicates (Program cs) =
  let idbPredicatesForClause (Clause hs bs) = if null bs then []
                                              else concatMap (\(Literal p _ _) -> [p]) hs
  in Set.fromList $ concatMap idbPredicatesForClause cs

predicates :: Program a s -> Set Predicate
predicates (Program cs) =
  let predicatesForClause (Clause hs bs) = concatMap (\case
                                                         Literal p _ _  -> [p]
                                                         _ -> [])
                                           (hs ++ bs)
  in Set.fromList $ concatMap predicatesForClause cs

transformProgram :: Program a s
                 -> PredicateDemand -- initial demand
                 -> Program a s

transformProgram prog ipd =
  let flatp@(Program cs) = flattenProgram prog
      pd = programPredicateDemand prog ipd
      isEDB pred = not $ Set.member pred (idbPredicates flatp)
      transformClause c@(Clause [h@(Literal p ts _)] bs) =
        if False && null bs then [c] -- this is a fact, nothing to do here
        else let demandPatterns = Set.toAscList (Map.findWithDefault Set.empty p pd) in
               concatMap (\hdp -> genDemandRules h hdp bs isEDB) demandPatterns
  in Program $ concatMap transformClause cs

initialDemand :: Predicate -> DemandPattern -> PredicateDemand
initialDemand p d = Map.fromList [(p, Set.fromList [d])]
