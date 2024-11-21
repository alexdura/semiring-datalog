{-# LANGUAGE DeriveFoldable #-}

module AST where

data AST a = AST {kind::a, token::String, children::[AST a]} deriving (Eq, Show, Foldable, Ord)

-- pretty printing
pretty :: Show a => AST a -> String
pretty = prettyIndent 0

prettyIndent :: Show a => Int -> AST a -> String
prettyIndent n node = replicate n '\t' ++ show node.kind ++ " \"" ++ node.token ++ "\"\n" ++ concatMap (prettyIndent (n + 1)) node.children
