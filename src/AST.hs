{-# LANGUAGE DeriveFoldable #-}

module AST (AST(..), pretty, numberNodes, parentMap) where

import Control.Monad.State.Strict
import Data.Map.Strict hiding (foldl)
import Control.Monad


data AST a = AST {kind::a, token::String, children::[AST a]} deriving (Eq, Show, Foldable, Ord)

-- pretty printing
pretty :: Show a => AST a -> String
pretty = prettyIndent 0

prettyIndent :: Show a => Int -> AST a -> String
prettyIndent n node = replicate n '\t' ++ show node.kind ++ " \"" ++ node.token ++ "\"\n" ++ concatMap (prettyIndent (n + 1)) node.children


-- node numbering
numberNodes' :: AST a -> State Int (AST (a, Int))
numberNodes' n = do
  children' <- forM n.children numberNodes'
  c <- get
  put (c + 1)
  return $ AST (n.kind, c) n.token children'

numberNodes :: AST a -> AST (a, Int)
numberNodes n = evalState (numberNodes' n) 0


-- building the parent map
parentMap' :: Ord a => AST a -> State (Map (AST a) (AST a)) ()
parentMap' n = do
  forM_ n.children $ \c -> do
    cmap <- get
    put $ insert c n cmap
    parentMap' c

parentMap :: Ord a => AST a -> Map (AST a) (AST a)
parentMap n = execState (parentMap' n) empty
