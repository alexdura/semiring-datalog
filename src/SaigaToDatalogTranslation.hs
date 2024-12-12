module SaigaToDatalogTranslation (translateToTerm, translateToClause, translateProgram, SaigaAtom, SaigaTerm, SaigaClause) where

import Datalog
import Saiga (Domain (..), Expr (..), SaigaElement(..), SaigaAttribute)
import Control.Monad.State
import Data.List (isPrefixOf)

type SaigaTerm a = Term (Domain a)
type SaigaAtom a = Atom (Domain a) Bool
type SaigaClause a = Clause (Domain a) Bool
type SaigaDatalogProgram a = Program (Domain a) Bool

equals :: Eq a => Term a -> Term a -> Atom a Bool
equals x y = Datalog.Function "_builtin_eq" [x, y] (\[x', y'] -> x' == y')

notEquals :: Eq a => Term a -> Term a -> Atom a Bool
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


translateToTermS (Func f args@[l, r]) | Just dlExpr <- builtinFunctionToDatalogExpr f = do
  ls <- translateToTermS l
  rs <- translateToTermS r
  names <- get
  put $ tail names
  let fvar = Variable $ head names
  return $ [(fvar, ls' ++ rs' ++ [Bind (dlExpr lvar rvar) fvar]) |
            (lvar, ls') <- ls,
            (rvar, rs') <- rs]


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
  return $ [(fvar, es' ++ [Bind (dlHeadExpr evar) fvar]) | (evar, es') <- es]

translateToTermS (Tail e) = do
  es <- translateToTermS e
  names <- get
  let fvar = Variable $ head names
  put $ tail names
  return $ [(fvar, es' ++ [Bind (dlTailExpr evar) fvar]) | (evar, es') <- es]

translateToTermS (Cons h t) = do
  hs <- translateToTermS h
  ts <- translateToTermS t
  names <- get
  let fvar = Variable $ head names
  put $ tail names
  return $ do
    (hvar, hs') <- hs
    (tvar, ts') <- ts
    return (fvar, hs' ++ ts' ++ [Bind (dlConsExpr hvar tvar) fvar])

translateToTermS Nil = do
  names <- get
  put $ tail names
  let fvar = Variable $ head names
  return [(fvar, [Bind dlNilExpr fvar])]


translateToTerm :: (SaigaAttribute attr, Eq a) =>
                   Expr attr ->
                   [(SaigaTerm a, [SaigaAtom a])]
translateToTerm expr = evalState (translateToTermS expr) freshVarNames


builtinFunctionToDatalogExpr name =
  case name of
    "_builtin_add" -> Just $ \t0 t1 ->
      Expr "_add" [t0, t1] (\case [DInt l, DInt r] -> DInt $ l + r
                                  _ -> error "Can only add integers.")

    "_builtin_mul" -> Just $ \t0 t1 ->
      Expr "_mul" [t0, t1] (\case [DInt l, DInt r] -> DInt $ l * r
                                  _ -> error "Can only multiply integers.")

    "_builtin_lt" -> Just $ \t0 t1 ->
      Expr "_lt" [t0, t1] (\case [DInt l, DInt r] -> DBool $ l < r
                                 _ -> error "Can only compare integers.")
    _ -> Nothing


dlHeadExpr v = Expr "_head" [v] (\case [DList (h:_)] -> h
                                       [DList []] -> error "Taking the head of an empty list."
                                       [_] -> error "Taking the head of a value that is not a list."
                                       _ -> error "_head function has exactly one argument.")

dlTailExpr v = Expr "_tail" [v] (\case [DList (_:t)] -> DList t
                                       [DList []] -> error "Taking the tail of an empty list."
                                       [_] -> error "Taking the tail of a value that is not a list."
                                       _ -> error "_tail function has exactly one argument.")

dlNilExpr = Constant $ DList []

dlConsExpr h t = Expr "_cons" [h, t] (\case [v, DList l] -> DList (v:l)
                                            _ -> error "Second argument to cons must be a list.")

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

translateToClauseS (CircularAttribute attr nargs e ie _) = do
  es <- translateToTermS e
  ies <- translateToTermS ie
  return $
    [[lit (show attr) $ [var "_node"] ++ [var $ "_arg_" ++ show i | i <- [0..nargs - 1]] ++ [v]] += t | (v, t) <- es] ++
    [[lit (show attr) $ [var "_node"] ++ [var $ "_arg_" ++ show i | i <- [0..nargs - 1]] ++ [v]] += t | (v, t) <- ies]


translateToClauseS _ = return []

translateToClause :: (SaigaAttribute attr, Eq a) => SaigaElement attr a -> [SaigaClause a]
translateToClause el = evalState (translateToClauseS el) freshVarNames

translateProgram :: (SaigaAttribute attr, Eq a) => [SaigaElement attr a] -> SaigaDatalogProgram a
translateProgram = Program . concatMap translateToClause
