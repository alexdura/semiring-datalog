module SaigaDatalog (translateToTerm, translateToClause, translateProgram, SaigaAtom, SaigaTerm, SaigaClause) where

import Datalog
import Saiga (Domain (..), Expr (..), SaigaElement(..), SaigaAttribute)
import Control.Monad.State
import Data.List (isPrefixOf)

type SaigaTerm a = Term (Domain a) Bool
type SaigaAtom a = Atom (Domain a) Bool
type SaigaClause a = Clause (Domain a) Bool
type SaigaDatalogProgram a = Program (Domain a) Bool

equals :: Eq a => Term a Bool -> Term a Bool -> Atom a Bool
equals x y = Datalog.Function "_builtin_eq" [x, y] (\[x', y'] -> x' == y')

notEquals :: Eq a => Term a Bool -> Term a Bool -> Atom a Bool
notEquals x y = Datalog.Function "_builtin_neq" [x, y] (\[x', y'] -> x' /= y')

translateToTermS :: (SaigaAttribute attr, Eq a) => Expr attr -> State [String] [(SaigaTerm a, [SaigaAtom a])]
translateToTermS (IVal n) = return [(Constant $ DInt n, [])]
translateToTermS (BVal b) = return [(Constant $ DBool b, [])]
translateToTermS (SVal s) = return  [(Constant $ DString s, [])]
translateToTermS Node = return [(Variable "_node", [])]
translateToTermS (Arg n) = return [(Variable $ "_arg_" ++ show n, [])]
translateToTermS (IfElse c t f) = do
  -- compute translation within the State monad
  cs <- translateToTermS c
  ts <- translateToTermS t
  fs <- translateToTermS f
  let trueTranslation = do
        (cvar, cs') <- cs
        (tvar, ts') <- ts
        return (tvar, cs' ++ [equals cvar (Constant $ DBool True)] ++ ts')
      falseTranslation = do
        (cvar, cs') <- cs
        (fvar, fs') <- fs
        return (fvar, cs' ++ [equals cvar (Constant $ DBool False)] ++ fs')
  return $ trueTranslation ++ falseTranslation

translateToTermS (IfEq e1 e2 t f) = do
  e1s <- translateToTermS e1
  e2s <- translateToTermS e2
  ts <- translateToTermS t
  fs <- translateToTermS f
  let trueTranslation = do
        (e1var, e1s') <- e1s
        (e2var, e2s') <- e2s
        (tvar, ts') <- ts
        return (tvar,  e1s' ++ e2s' ++ [equals e1var e2var] ++ ts')
      falseTranslation = do
        (e1var, e1s') <- e1s
        (e2var, e2s') <- e2s
        (fvar, fs') <- fs
        return (fvar,  e1s' ++ e2s' ++ [notEquals e1var e2var] ++ fs')
  return $ trueTranslation ++ falseTranslation

translateToTermS (Func name args) = do
  args'  <- mapM translateToTermS args -- :: [[(SaigaTerm a, [SaigaAtom a])]]
  names <- get
  put $ tail names
  let fvar = Variable $ head names
  return $ map (\arg -> (fvar, (concatMap snd arg) ++ [lit name (map fst arg ++ [fvar])])) (sequence args')

translateToTermS (Attr n attr args) = do
  names <- get
  put $ tail names
  let fvar = Variable $ head names
  ns <- translateToTermS n
  args' <- mapM translateToTermS args
  return $ map (\arg -> (fvar, (concatMap snd arg) ++ [lit (show attr) (map fst arg ++ [fvar])])) (sequence (ns : args'))

translateToTermS (Head e) = do
  es <- translateToTermS e
  names <- get
  let fvar = Variable $ head names
  put $ tail names
  return $ [(fvar, es' ++ [Literal "_head" [evar, fvar] id]) | (evar, es') <- es]

translateToTermS (Tail e) = do
  es <- translateToTermS e
  names <- get
  let fvar = Variable $ head names
  put $ tail names
  return $ [(fvar, es' ++ [Literal "_tail" [evar, fvar] id]) | (evar, es') <- es]

translateToTermS (Cons h t) = do
  hs <- translateToTermS h
  ts <- translateToTermS t
  names <- get
  let fvar = Variable $ head names
  put $ tail names
  return $ do
    (hvar, hs') <- hs
    (tvar, ts') <- ts
    return (fvar, hs' ++ ts' ++ [Literal "_cons" [hvar,  tvar, fvar] id])

translateToTermS Nil = do
  names <- get
  put $ tail names
  let fvar = Variable $ head names
  return [(fvar, [Literal "_nil" [fvar] id])]


translateToTerm :: (SaigaAttribute attr, Eq a) =>
                   Expr attr ->
                   [(SaigaTerm a, [SaigaAtom a])]
translateToTerm expr = evalState (translateToTermS expr) freshVarNames

reservedName :: String -> Bool
reservedName n = any (`isPrefixOf` n) ["_node", "_arg", "_nil", "_head", "_tail", "_cons"]

freshVarNames :: [String]
freshVarNames = ['_' : p : v | v <- "":freshVarNames, p <- ['a'..'z'], not $ reservedName ('_' : p : v)]

translateToClauseS :: (SaigaAttribute attr, Eq a) => SaigaElement attr a -> State [String] [SaigaClause a]
translateToClauseS (Saiga.Function name nargs e) =  do
  es <- translateToTermS e
  return [[lit name $ [var $ "_arg_" ++ show i | i <- [0..nargs - 1]] ++ [v]] += t | (v, t) <- es]

translateToClauseS (Attribute attr nargs e)= do
  es <- translateToTermS e
  return [[lit (show attr) $ [var "_node"] ++ [var $ "_arg_" ++ show i | i <- [0..nargs - 1]] ++ [v]] += t | (v, t) <- es]

translateToClauseS _ = return []

translateToClause :: (SaigaAttribute attr, Eq a) => SaigaElement attr a -> [SaigaClause a]
translateToClause el = evalState (translateToClauseS el) freshVarNames

translateProgram :: (SaigaAttribute attr, Eq a) => [SaigaElement attr a] -> SaigaDatalogProgram a
translateProgram = Program . concatMap translateToClause
