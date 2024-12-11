{-# LANGUAGE OverloadedStrings #-}

module PlaygroundLang where

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
      new = Func "add" [old, IVal 1]
      new2 = Func "mul" [new, new]
      cmp = Func "lt" [x, new2]
  in
    IfElse cmp old new
  ) (IVal 0) "NotImplemented"



playProgram ast = [
    sqrtAttr,

    BuiltinFunction "add" 2 $ \[DInt m, DInt n] -> DInt $ m + n,

    BuiltinFunction "mul" 2 $ \[DInt m, DInt n] -> DInt $ m * n,

    BuiltinFunction "lt" 2 $ \[DInt m, DInt n] -> DBool $ m < n
    ]
