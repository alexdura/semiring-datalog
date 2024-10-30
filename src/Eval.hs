module Eval(Context, query, eval, evalStep, emptyContext, loadFromCSV, storeToCSV, DatalogGroundTerm(..)) where

import qualified Data.Map.Strict as Map
import Data.Semiring (Semiring)
import qualified Data.Semiring as Semiring
import Data.Maybe
import qualified Text.CSV as CSV
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Datalog

type Relation a b = Map.Map [a] b

data Context a b = Context {
  relations :: Map.Map Predicate (Relation a b),
  objects :: Map.Map [a] a,
  freshIdx :: a
  } deriving (Eq, Show)

type Binding a = Map.Map String a

class (Ord a, Read a, Show a) => DatalogGroundTerm a where
  firstIndex :: a
  nextIndex :: a -> a
  parse :: String -> a
  unparse :: a -> String


emptyBinding :: Binding a
emptyBinding = Map.empty

immediateConsequence :: (DatalogGroundTerm a, Eq s, Semiring s) => Clause a s -> State (Context a s) ()
immediateConsequence c = do
  bindings <- immediateConsequence' c.body (emptyBinding, Semiring.one)
  forM_ c.heads (\at -> insertAtoms at bindings)

insertAtom :: (DatalogGroundTerm a, Semiring s) => Atom a s -> (Binding a, s) -> State (Context a s) ()
insertAtom (Literal p ts _) (b, v) = do
  ctx <- get
  let rel = Map.findWithDefault Map.empty p ctx.relations
  tpl <- mapM (evalExpr b) ts
  let rel' = Map.insertWith (Semiring.+) tpl v rel
  modify $ \ctx -> ctx { relations = Map.insert p rel' ctx.relations }

insertAtom _ _ = error "Unexpected head "


insertAtoms :: (DatalogGroundTerm a, Semiring s) => Atom a s -> [(Binding a, s)] -> State (Context a s) ()
insertAtoms at bs = forM_ bs (insertAtom at)

immediateConsequence' :: (DatalogGroundTerm a, Eq s, Semiring s)
                      => [Atom a s]
                      -> (Binding a, s)
                      -> State (Context a s) [(Binding a, s)]
immediateConsequence' [] (b, v) = return [(b, v)]
immediateConsequence' (a:as) (b, v) = do
  bindingsNext <- case a of Literal {} -> lookupLiteral a b
                            Value s -> return [(b, s)]
                            f@(Function {}) -> return [(b, applyFunction b f)]
  r <- mapM (immediateConsequence' as) [(b', v Semiring.* v') | (b', v') <- bindingsNext, v Semiring.* v' /= Semiring.zero]
  return $ concat r


applyFunction :: Binding a -> Atom a s -> s
applyFunction b (Function _ ts f) =
  let args = map (\case (Variable name) -> fromMaybe (error $ "Undefined variable '" ++ name ++ "'.") $ b Map.!? name
                        (Constant c) -> c
                        (Expr _ _) -> error "No Expr in Function") ts

  in f args

lookupTerm :: DatalogGroundTerm a => Binding a -> (Term a, a) -> MaybeT (State (Context a s)) (Binding a)
lookupTerm b (t, k) = case t of
                        (Variable name) -> case Map.lookup name b of
                                             Just v -> if v == k then return b else mzero -- name already bound, check equality
                                             Nothing -> return $ Map.insert name k b -- name not bound, bind now
                        (Constant c) -> if c == k then return b else mzero
                        (Expr opds f) -> do
                          r <- lift $ evalExprs b opds f
                          if r == k then return b else mzero
                        (Fresh opds) -> do
                          r <- lift $ makeFreshValue b opds
                          if r == k then return b else mzero

evalExprs :: DatalogGroundTerm a => Binding a -> [Term a] -> ([a] -> a) -> State (Context a s) a
evalExprs b opds f = liftM f $ mapM (evalExpr b) opds

evalExpr :: DatalogGroundTerm a => Binding a -> Term a -> State (Context a s) a
evalExpr b e = case e of
                 (Variable name) -> return $
                   fromMaybe (error $ "Undefined variable '" ++ name ++ "'.") $ b Map.!? name
                 (Constant c) -> return c
                 (Expr opds' f') -> evalExprs b opds' f'
                 (Fresh opds) -> makeFreshValue b opds


makeFreshValue :: DatalogGroundTerm a => Binding a -> [Term a] -> State (Context a s) a
makeFreshValue b opds = do
  opds' <- mapM (evalExpr b) opds
  ctx <- get
  case Map.lookup opds' ctx.objects of
    Just v -> return v
    Nothing -> do
      put $ ctx { objects = Map.insert opds' ctx.freshIdx ctx.objects,
                  freshIdx = nextIndex ctx.freshIdx }
      return ctx.freshIdx

lookupLiteral :: DatalogGroundTerm a => Atom a s -> Binding a -> State (Context a s) [(Binding a, s)]
lookupLiteral (Literal p ts f) b = do
  ctx <- get
  let rel = Map.findWithDefault Map.empty p ctx.relations
  let lookupTpl = \(ks, v) -> runMaybeT $ foldM lookupTerm b (zip ts ks) >>= (\x -> return (x, v))
  (mapM lookupTpl (Map.toList rel)) >>= return . catMaybes



eval :: (DatalogGroundTerm a, Semiring s, Eq s) => Program a s -> Context a s -> Context a s
eval p@(Program cs) ctx =
  let ctx' = execState (forM_ cs immediateConsequence) ctx
  in if ctx == ctx' then ctx
     else eval p ctx'

evalStep :: (DatalogGroundTerm a, Semiring s, Eq s, Show s) => Program a s -> Context a s -> IO (Context a s)
evalStep p@(Program cs) ctx = do
  print $ show ctx
  _ <- getChar
  let ctx' = execState (forM_ cs immediateConsequence) ctx
  if ctx == ctx' then return ctx
    else evalStep p ctx'


emptyContext :: DatalogGroundTerm a => Context a s
emptyContext = Context Map.empty Map.empty firstIndex

query :: String -> Context a s -> [([a], s)]
query pred ctx =
  case Map.lookup pred ctx.relations of
    Just rel -> Map.toList rel
    Nothing -> []


instance DatalogGroundTerm Integer where
  parse = read
  unparse = show
  firstIndex = 0
  nextIndex = (+ 1)

instance DatalogGroundTerm Int where
  parse = read
  unparse = show
  firstIndex = 0
  nextIndex = (+ 1)

instance DatalogGroundTerm String where
  parse = read
  unparse = show
  firstIndex = "0"
  nextIndex = (++ "0")

loadFromCSV :: (DatalogGroundTerm a, Ord a) => Context a s -> String -> ([a] -> s) -> FilePath -> IO (Context a s)
loadFromCSV ctx name dflt path = do
  Right csv <- CSV.parseCSVFromFile path
  let rel = Map.findWithDefault Map.empty name ctx.relations
      rel' = foldr (\row rel -> Map.insert (parse <$> row) (dflt (parse <$> row)) rel) rel (filter ([""] /=) csv)
  return $ ctx {relations = Map.insert name rel' ctx.relations}

storeToCSV :: (DatalogGroundTerm a, Show s) => Context a s -> String -> FilePath -> IO ()
storeToCSV ctx name path =
  let csv =  (\(t, v) -> (unparse <$> t) ++ [show v])  <$> (Map.toList (Map.findWithDefault Map.empty name ctx.relations)) in
    writeFile path (CSV.printCSV csv)
