{-# LANGUAGE OverloadedStrings #-}

module PlaygroundLang (playProgram, PlayAttr(..)) where

import AST
import Saiga
import Prelude hiding (otherwise, (<*>))
import qualified Data.Map.Strict as Map
import Data.Maybe


data PlayAttr = Sqrt
              | Sqrt1
              | Square
              | Sqrt2
              | Sqrt3
              | Sqrt4
              | Sqrt5Circ -- circular definition
              | Sqrt5Driver -- computation 'driver'
              | Sqrt5
              | Sqrt6Circ
              | Sqrt6Driver
              | Sqrt6
              | Sqrt6Id
              deriving (Eq, Show, Enum, Ord)

instance SaigaAttribute PlayAttr

infixl 6 <+>
(<+>) x y = Func "_builtin_add" [x, y]

infixl 7 <*>
(<*>) x y = Func "_builtin_mul" [x, y]


attrCmp :: Expr a
attrCmp =
  let l = Arg 0
      r = Arg 1
  in
    IfLt r l (BVal False) (BVal True)


sqrtAttr :: SaigaElement PlayAttr a
sqrtAttr = CircularAttribute Sqrt 1 (
  Let "old" (Node <.> Sqrt <?> [Arg 0]) (
      Let "new" (Var "old" <+> IVal 1) (
          Let "new2" (Var "new" <*> Var "new") (
              IfLt (Arg 0) (Var "new2") (Var "old") (Var "new")
              )
          )
      )
  ) (IVal 0) attrCmp

sqrt1Attr :: SaigaElement PlayAttr a
sqrt1Attr = CircularAttribute Sqrt1 1 (
  let arg = Arg 0
      old = Node <.> Sqrt1 <?> [arg]
      new = old <+> (IVal 1)
      new2 = new <*> new
  in
    IfLt arg new2 old new
  ) (IVal 0) attrCmp


squareAttr :: SaigaElement PlayAttr a
squareAttr = Attribute Square 1 $
  Func "_builtin_mul" [Arg 0, Arg 0]


sqrt2Attr :: SaigaElement PlayAttr a
sqrt2Attr = CircularAttribute Sqrt2 1 (
  Let "old" (Node <.> Sqrt2 <?> [Arg 0]) (
      Let "new" (Var "old" <+> IVal 1) (
          Let "new2" (Node <.> Square <?> [Var "new"]) (
              IfLt (Arg 0) (Var "new2") (Var "old") (Var "new")
              )
          )
      )
  ) (IVal 0) attrCmp


sqrt3Attr :: SaigaElement PlayAttr a
sqrt3Attr = CircularAttribute Sqrt3 1 (
  Let "old" (Node <.> Sqrt4 <?> [Arg 0]) (
      Let "new" (Func "_builtin_add" [Var "old", IVal 1]) (
          Let "new2" (Func "_builtin_mul" [Var "new", Var "new"]) (
              IfLt (Arg 0) (Var "new2") (Var "old") (Var "new")
              )
          )
      )
  ) (IVal 0) attrCmp

sqrt4Attr :: SaigaElement PlayAttr a
sqrt4Attr = Attribute Sqrt4 1 $
  Node <.> Sqrt3 <?> [Arg 0]


sqrt5CircAttr :: SaigaElement PlayAttr a
sqrt5CircAttr = Attribute Sqrt5Circ 2 $

  let iter = Arg 1
      oldIter = Func "_builtin_add" [iter, IVal (-1)]
      arg = Arg 0
  in
    IfEq iter (IVal 0)
    -- then
    (
      IVal 0
    )
    -- else
    (
      Let "old" (Node <.> Sqrt5Circ <?> [arg, oldIter]) $
      Let "new" (Func "_builtin_add" [Var "old", IVal 1]) $
      Let "new2" (Func "_builtin_mul" [Var "new", Var "new"]) $
      IfLt arg (Var "new2") (Var "old") (Var "new")
    )


sqrt5DriverAttr :: SaigaElement PlayAttr a
sqrt5DriverAttr = Attribute Sqrt5Driver 2 $
  let iter = Arg 1
      nextIter = Func "_builtin_add" [iter, IVal 1]
      arg = Arg 0
  in
    Let "old" (Node <.> Sqrt5Circ <?> [arg, iter]) $
    Let "new" (Node <.> Sqrt5Circ <?> [arg, nextIter]) $
    IfEq (Var "old") (Var "new") (Var "old") (Node <.> Sqrt5Driver <?> [arg, nextIter])

sqrt5Attr :: SaigaElement PlayAttr a
sqrt5Attr = Attribute Sqrt5 1 $
  Node <.> Sqrt5Driver <?> [Arg 0, IVal 0]

sqrt6CircAttr :: SaigaElement PlayAttr a
sqrt6CircAttr = Attribute Sqrt6Circ 2 $
  let iter = Arg 1
      oldIter = Func "_builtin_add" [iter, IVal (-1)]
      arg = Arg 0
  in
    IfEq iter (IVal 0)
    -- then
    (
      IVal 0
    )
    -- else
    (
      Let "old" (Node <.> Sqrt6Id <?> [arg, oldIter]) $
      Let "new" (Func "_builtin_add" [Var "old", IVal 1]) $
      Let "new2" (Func "_builtin_mul" [Var "new", Var "new"]) $
      IfLt arg (Var "new2") (Var "old") (Var "new")
    )

sqrt6DriverAttr :: SaigaElement PlayAttr a
sqrt6DriverAttr = Attribute Sqrt6Driver 2 $
  let iter = Arg 1
      nextIter = Func "_builtin_add" [iter, IVal 1]
      arg = Arg 0
  in
    Let "old" (Node <.> Sqrt6Circ <?> [arg, iter]) $
    Let "new" (Node <.> Sqrt6Circ <?> [arg, nextIter]) $
    IfEq (Var "old") (Var "new") (Var "old") (Node <.> Sqrt6Driver <?> [arg, nextIter])

sqrt6Attr :: SaigaElement PlayAttr a
sqrt6Attr = Attribute Sqrt6 1 $
  Node <.> Sqrt6Driver <?> [Arg 0, IVal 0]


sqrt6IdAttr :: SaigaElement PlayAttr a
sqrt6IdAttr = Attribute Sqrt6Id 2 $
  Node <.> Sqrt6Circ <?> [Arg 0, Arg 1]


listFunc1 :: SaigaElement PlayAttr a
listFunc1 = Function "listFunc1" 0 $
  let cons' h t = Func "_builtin_cons" [h, t]
      head' l = Func "_builtin_head" [l]
      nil' = Func "_builtin_nil" []
  in head' $ cons' (IVal 1) (cons' (IVal 2) (cons' (IVal 3) nil'))


playProgram :: AST (String, Int) -> SaigaProgram PlayAttr (String, Int)

playProgram ast = [
  -- single attribute
  sqrtAttr,
  sqrt1Attr, -- no variables
  -- circular ---dep---> non-circular
  sqrt2Attr,
  squareAttr,
  -- circular ---dep---> non-circular ---dep---> circular
  sqrt3Attr,
  sqrt4Attr,
  --
  sqrt5CircAttr,
  sqrt5DriverAttr,
  sqrt5Attr,
  --
  sqrt6CircAttr,
  sqrt6DriverAttr,
  sqrt6Attr,
  sqrt6IdAttr,
  --
  listFunc1,
  --
  BuiltinFunction "_builtin_add" 2 $ \[DInt m, DInt n] -> DInt $ m + n,

  BuiltinFunction "_builtin_mul" 2 $ \[DInt m, DInt n] -> DInt $ m * n,

  BuiltinFunction "_builtin_cons" 2 $ \[e, DList l] -> DList $ e:l,

  BuiltinFunction "_builtin_tail" 1 $ \[DList l] -> DList $ (tail l),

  BuiltinFunction "_builtin_head" 1 $ \[DList l] -> head l,

  BuiltinFunction "_builtin_nil" 0 $ \[] -> DList []
  ]
