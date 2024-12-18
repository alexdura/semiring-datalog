module Souffle where

import Datalog
import DemandTransformation

import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Saiga (Domain (..))

class Souffle a where
  soufflePrint :: a -> String


instance Souffle a => Souffle (Term a) where
  soufflePrint (Variable s) = s
  soufflePrint (Constant c) = soufflePrint c
  soufflePrint (Expr "_add" [l, r] _) = "(" ++ soufflePrint l ++ " + " ++ soufflePrint r ++ ")"
  soufflePrint (Expr "_mul" [l, r] _) = "(" ++ soufflePrint l ++ " * " ++ soufflePrint r ++ ")"
  soufflePrint _ = error "Print not implemented for term."

instance Souffle a => Souffle (Atom a Bool) where
  soufflePrint (Literal p ts _) = p ++ "(" ++ intercalate ", " (soufflePrint <$> ts) ++ ")"
  soufflePrint (Bind src dst) = soufflePrint dst ++ " = " ++ soufflePrint src
  soufflePrint (Function "_builtin_neq" [l, r] _) = soufflePrint l ++ " != " ++ soufflePrint r
  soufflePrint (Function "_builtin_eq" [l, r] _) = soufflePrint l ++ " = " ++ soufflePrint r
  soufflePrint (Function "_builtin_lt" [l, r] _) = soufflePrint l ++ " < " ++ soufflePrint r
  soufflePrint (Function "_builtin_gte" [l, r] _) = soufflePrint l ++ " >= " ++ soufflePrint r
  soufflePrint _ = error "Print not implemented for atom."

instance Souffle a => Souffle (Clause a Bool) where
  soufflePrint (Clause [] _) = ""
  soufflePrint (Clause hs []) = intercalate ", " (soufflePrint <$> hs) ++ "."
  soufflePrint (Clause hs bs) = intercalate ", " (soufflePrint <$> hs) ++ " :- " ++
                                intercalate ", " (soufflePrint <$> bs) ++ "."


arities :: Program a s -> Map Predicate Int
arities (Program cs) = (foldr (.) id (clauseArities <$> cs)) Map.empty

clauseArities :: Clause a s -> Map Predicate Int -> Map Predicate Int
clauseArities (Clause hs bs) = (foldr (.) id (predArity <$> hs)) .
                               (foldr (.) id (predArity <$> bs))

predArity :: Atom a s -> Map Predicate Int -> Map Predicate Int
predArity (Literal p ts _) m =  Map.insert p (length ts) m
predArity _ m = m

souffleHeader :: Program a s -> String
souffleHeader prog =
  let preds = Map.toList $ arities prog
      predDecl p = ".decl " ++ fst p ++ "(" ++ intercalate ", " ((\n -> "_" ++ show n ++ ":number") <$> [0 .. snd p - 1]) ++ ")"
  in
    intercalate "\n" $  predDecl <$> preds

souffleFooter :: Program a s -> String
souffleFooter p =
  let outPreds = Set.toList $ (predicates p) in
    intercalate "\n" $ (\p -> ".output " ++ p) <$> outPreds


instance Souffle a => Souffle (Program a Bool) where
  soufflePrint prog@(Program cs) = souffleHeader prog ++ "\n\n" ++
                                (intercalate "\n" $ soufflePrint <$> cs)  ++ "\n\n" ++
                                souffleFooter prog


instance Souffle (Domain (String, Int))  where
  soufflePrint (DInt n) = show n
  soufflePrint (DString s) = s
  soufflePrint (DBool True) = "true"
  soufflePrint (DBool False) = "false"
  soufflePrint _ = error "Not implemented"
