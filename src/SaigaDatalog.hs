module SaigaDatalog (translateToTerm, SaigaAtom, SaigaTerm) where

import Datalog
import Saiga (Domain (..), Expr (..))
import Control.Monad.State

type SaigaTerm a = Term (Domain a) Bool
type SaigaAtom a = Atom (Domain a) Bool
type SaigaClause a = Clause (Domain a) Bool

equals :: Eq a => Term a Bool -> Term a Bool -> Atom a Bool
equals x y = Function [x, y] (\[x', y']-> x' == y')

translateToTermS :: (Enum attr, Eq a) => Expr attr -> State [String] [(SaigaTerm a, [SaigaAtom a])]
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
  let fvar = Variable $ head names
  put $ tail names
  return [(fvar, [Literal "_nil" [fvar] id])]

--   fresh <- head varNames
--   put $ tail varNames
--   let fvar = Variable fresh
--   lift [(fvar, Literal name [argVar, fvar] id : argCons)]

translateToTerm :: (Enum attr, Eq a) => Expr attr -> [(SaigaTerm a, [SaigaAtom a])]
translateToTerm expr = evalState (translateToTermS expr) freshVarNames

reservedNames :: [String]
reservedNames = ["_node", "_arg", "_nil", "_head", "_tail", "_cons"]

freshVarNames :: [String]
freshVarNames = ['_' : p : v | v <- "":freshVarNames, p <- ['a'..'z'], ('_' : p : v) `notElem` reservedNames]







translateToAtom :: Enum attr => Expr attr -> [(SaigaAtom a, [SaigaAtom a])]
translateToAtom = undefined



translateToClause :: Enum attr => Expr attr -> [SaigaClause a]
translateToClause = undefined
