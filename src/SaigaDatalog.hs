module SaigaDatalog (translateToTerm, translateToClause, SaigaAtom, SaigaTerm, SaigaClause) where

import Datalog
import Saiga (Domain (..), Expr (..), SaigaElement(..), SaigaAttribute)
import Control.Monad.State

type SaigaTerm a = Term (Domain a) Bool
type SaigaAtom a = Atom (Domain a) Bool
type SaigaClause a = Clause (Domain a) Bool

equals :: Eq a => Term a Bool -> Term a Bool -> Atom a Bool
-- equals x y = Datalog.Function [x, y] (\[x', y']-> x' == y')
equals x y = Literal "eq" [x, y] id

translateToTermS :: (SaigaAttribute attr, Eq a) => Expr attr -> State [String] [(SaigaTerm a, [SaigaAtom a])]
translateToTermS (IVal n) = return [(Constant $ DInt n, [])]
translateToTermS (BVal b) = return [(Constant $ DBool b, [])]
translateToTermS (SVal s) = return  [(Constant $ DString s, [])]
translateToTermS Node = return [(Variable "_node", [])]
translateToTermS Arg = return [(Variable "_arg", [])]
translateToTermS (IfElse c t f) = do
  -- compute translation within the State monad
  cs <- translateToTermS c
  ts <- translateToTermS t
  fs <- translateToTermS f
  return $ do
    -- combine the results in the List monad
    (cvar, cs') <- cs
    (tvar, ts') <- ts
    (fvar, fs') <- fs
    [(tvar, [equals cvar (Constant $ DBool True)] ++ cs' ++ ts'),
     (fvar, [equals cvar (Constant $ DBool False)] ++ cs' ++ fs')]

translateToTermS (Func name arg) = do
  args <- translateToTermS arg
  mapM (\arg' -> do
           names <- get
           put $ tail names
           let fvar = Variable $ head names
           return (fvar, Literal name [fst arg', fvar] id : snd arg'))
    args

translateToTermS (Head e) = do
  es <- translateToTermS e
  names <- get
  let fvar = Variable $ head names
  put $ tail names
  return $ [(fvar, Literal "_head" [evar, fvar] id : es') | (evar, es') <- es]

translateToTermS (Tail e) = do
  es <- translateToTermS e
  names <- get
  let fvar = Variable $ head names
  put $ tail names
  return $ [(fvar, Literal "_tail" [evar, fvar] id : es') | (evar, es') <- es]

translateToTermS (Cons h t) = do
  hs <- translateToTermS h
  ts <- translateToTermS t
  names <- get
  let fvar = Variable $ head names
  put $ tail names
  return $ do
    (hvar, hs') <- hs
    (tvar, ts') <- ts
    return (fvar, [Literal "_cons" [hvar,  tvar, fvar] id] ++ hs' ++ ts')

translateToTermS Nil = do
  names <- get
  put $ tail names
  let fvar = Variable $ head names
  return [(fvar, [Literal "_nil" [fvar] id])]

translateToTermS (Attr n attr arg) = do
  names <- get
  put $ tail names
  let fvar = Variable $ head names
  ns <- translateToTermS n
  args <- translateToTermS arg
  return [(fvar, lit (show attr) [fst args', fst args', fvar] : (snd args') ++ (snd ns')) | args' <- args, ns' <- ns]

translateToTerm :: (SaigaAttribute attr, Eq a) => Expr attr -> [(SaigaTerm a, [SaigaAtom a])]
translateToTerm expr = evalState (translateToTermS expr) freshVarNames

reservedNames :: [String]
reservedNames = ["_node", "_arg", "_nil", "_head", "_tail", "_cons"]

freshVarNames :: [String]
freshVarNames = ['_' : p : v | v <- "":freshVarNames, p <- ['a'..'z'], ('_' : p : v) `notElem` reservedNames]

translateToClauseS :: (SaigaAttribute attr, Eq a) => SaigaElement attr a -> State [String] [SaigaClause a]
translateToClauseS (Saiga.Function name e) =  do
  es <- translateToTermS e
  return [[lit name [(var "_arg"), v]] += t | (v, t) <- es]

translateToClauseS (Attribute attr e)= do
  es <- translateToTermS e
  return [[lit (show attr) [var "_node", var "_arg", v]] += t | (v, t) <- es]

translateToClauseS _ = return []

translateToClause :: (SaigaAttribute attr, Eq a) => SaigaElement attr a -> [SaigaClause a]
translateToClause el = evalState (translateToClauseS el) freshVarNames
