module Datalog (Program(..), Clause(..), Atom(..), Term(..), Predicate, (+=),
                var, cst, lit, val, expr, prettyProgram, prettyAtom, prettyTerm, prettyClause) where


import Data.List

data Program a b = Program [Clause a b]

data Clause a s = Clause { heads :: [Atom a s], body :: [Atom a s]}

data Atom a s = Literal Predicate [Term a] (s -> s)
              | Value s
              | Function String [Term a] ([a] -> s)

data Term a = Variable String
            | Constant a
            | Expr [Term a] ([a] -> a)
            | Fresh [Term a]

type Predicate = String


prettyAtom (Literal p ts _) = p ++ "(" ++ (intercalate ", " (map prettyTerm ts)) ++ ")"
prettyAtom (Value b) = show b
prettyAtom (Function name ts _) = name ++ "(" ++ (intercalate ", " (map prettyTerm ts)) ++ ")"

prettyTerm (Constant c) = show c
prettyTerm (Variable v) = v
prettyTerm (Expr ts _) = "expr(" ++ intercalate ", " (map prettyTerm ts) ++ ")"

prettyClause (Clause hs ts) = intercalate ", " (map prettyAtom hs) ++ " <- " ++ intercalate ", " (map prettyAtom ts)

prettyProgram (Program cs) = intercalate "\n" (map prettyClause cs)


infix 0 +=
(+=) :: [Atom a s] -> [Atom a s] -> Clause a s
(+=) = Clause

lit :: String -> [Term a] -> Atom a s
lit s ts  = Literal s ts id

val :: s -> Atom a s
val = Value

cst :: a -> Term a
cst = Constant

var :: String -> Term a
var = Variable

expr :: ([a] -> a) -> [Term a] -> Term a
expr f ts = Expr ts f

clause :: [Atom a s] -> ([Atom a s] -> Clause a s) -> Clause a s
clause head rest = rest head
