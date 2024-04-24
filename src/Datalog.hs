module Datalog (Program(..), Clause(..), Atom(..), Term(..), Predicate, (+=),
                var, cst, lit, val, expr, prettyProgram, prettyAtom, prettyTerm) where


import Data.List

data Program a b = Program [Clause a b]

data Clause a b = Clause { heads :: [Atom a b], body :: [Atom a b]}

data Atom a b = Literal Predicate [Term a b] (b -> b)
              | Value b
              | Function [Term a b] ([a] -> b)


data Term a b = Variable String
              | Constant a
              | Expr [Term a b] ([a] -> a)
              | Fresh [Term a b]

type Predicate = String


prettyAtom (Literal p ts _) = p ++ "(" ++ (intercalate ", " (map prettyTerm ts)) ++ ")"
prettyAtom (Value b) = show b
prettyAtom (Function ts _) = "__builtin_function__" ++ "(" ++ (intercalate ", " (map prettyTerm ts)) ++ ")"

prettyTerm (Constant c) = show c
prettyTerm (Variable v) = show v
prettyTerm (Expr ts _) = "expr(" ++ intercalate ", " (map prettyTerm ts) ++ ")"

prettyClause (Clause hs ts) = intercalate ", " (map prettyAtom hs) ++ " <- " ++ intercalate ", " (map prettyAtom ts)

prettyProgram (Program cs) = intercalate "\n" (map prettyClause cs)


infix 0 +=
(+=) :: [Atom a s] -> [Atom a s] -> Clause a s
(+=) = Clause

lit :: String -> [Term a s] -> Atom a s
lit s ts  = Literal s ts id

val :: s -> Atom a s
val = Value

cst :: a -> Term a s
cst = Constant

var :: String -> Term a s
var = Variable

expr :: ([a] -> a) -> [Term a s] -> Term a s
expr f ts = Expr ts f

clause :: [Atom a s] -> ([Atom a s] -> Clause a s) -> Clause a s
clause head rest = rest head
