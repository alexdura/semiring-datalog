module CFGLang where

import AST

terminal s = AST "Terminal" s []

nUse s = AST "NUse" s []

nonEmptySymbolList h@(AST "Terminal" _ _) t@(AST "SymbolList" _ _) = AST "NonEmptySymbolList" "" [h, t]

emptySymbolList = AST "EmptySymbolList" "" []
