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
  Let "old" (Node <.> Sqrt <?> [Arg 0]) (
      Let "new" (Func "_builtin_add" [Var "old", IVal 1]) (
          Let "new2" (Func "_builtin_mul" [Var "new", Var "new"]) (
              IfElse (Func "_builtin_lt" [Arg 0, Var "new2"]) (Var "old") (Var "new")
              )
          )
      )
  ) (IVal 0) "NotImplemented"


playProgram :: AST (String, Int) -> SaigaProgram PlayAttr (String, Int)

playProgram ast = [
    sqrtAttr,

    BuiltinFunction "_builtin_add" 2 $ \[DInt m, DInt n] -> DInt $ m + n,

    BuiltinFunction "_builtin_mul" 2 $ \[DInt m, DInt n] -> DInt $ m * n,

    BuiltinFunction "_builtin_lt" 2 $ \[DInt m, DInt n] -> DBool $ m < n
    ]
