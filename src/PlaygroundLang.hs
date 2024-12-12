{-# LANGUAGE OverloadedStrings #-}

module PlaygroundLang (playProgram) where

import AST
import Saiga
import Prelude hiding (otherwise)
import qualified Data.Map.Strict as Map
import Data.Maybe


data PlayAttr = Sqrt
              deriving (Eq, Show, Enum)

instance SaigaAttribute PlayAttr


sqrtAttr :: SaigaElement PlayAttr a
sqrtAttr = CircularAttribute Sqrt 1 (
  let x = Arg 0
      old = Node <.> Sqrt <?> [x]
      new = Func "_builtin_add" [old, IVal 1]
      new2 = Func "_builtin_mul" [new, new]
      cmp = Func "_builtin_lt" [x, new2]
  in
    IfElse cmp old new
  ) (IVal 0) "NotImplemented"


playProgram :: AST (String, Int) -> SaigaProgram PlayAttr (String, Int)

playProgram ast = [
    sqrtAttr,

    BuiltinFunction "_builtin_add" 2 $ \[DInt m, DInt n] -> DInt $ m + n,

    BuiltinFunction "_builtin_mul" 2 $ \[DInt m, DInt n] -> DInt $ m * n,

    BuiltinFunction "_builtin_lt" 2 $ \[DInt m, DInt n] -> DBool $ m < n
    ]
