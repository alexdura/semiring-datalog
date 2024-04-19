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
translateToTermS Node = return [(Variable "node", [])]
translateToTermS Arg = return [(Variable "arg", [])]
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

--   fresh <- head varNames
--   put $ tail varNames
--   let fvar = Variable fresh
--   lift [(fvar, Literal name [argVar, fvar] id : argCons)]

translateToTerm :: (Enum attr, Eq a) => Expr attr -> [(SaigaTerm a, [SaigaAtom a])]
translateToTerm expr = evalState (translateToTermS expr) freshVarNames

freshVarNames = [p : v | v <- ":":freshVarNames, p <- ['a'..'z'], not $ elem (p : v) ["node", "arg"]]







translateToAtom :: Enum attr => Expr attr -> [(SaigaAtom a, [SaigaAtom a])]
translateToAtom = undefined



translateToClause :: Enum attr => Expr attr -> [SaigaClause a]
translateToClause = undefined
