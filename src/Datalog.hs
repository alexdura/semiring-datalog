module Datalog (Program(..), Clause(..), Atom(..), Term(..), Predicate, (+=),
                var, cst, lit, val, expr, bind, prettyProgram, prettyAtom, prettyTerm, prettyClause) where


import Data.List

newtype Program a s = Program [Clause a s]

data Clause a s = Clause { heads :: [Atom a s], body :: [Atom a s]}
                | SubsumptionClause { headl :: Atom a s,
                                      headr :: Atom a s,
                                      sbody :: [Atom a s] }

data Atom a s = Literal Predicate [Term a] (s -> s)
              | Value s
              | Function String [Term a] ([a] -> s)
              | Bind (Term a) (Term a)

data Term a = Variable String
            | Constant a
            | Expr String [Term a] ([a] -> a)
            | Fresh [Term a]

type Predicate = String


prettyAtom (Literal p ts _) = p ++ "(" ++ (intercalate ", " (map prettyTerm ts)) ++ ")"
prettyAtom (Value b) = show b
prettyAtom (Function name ts _) = name ++ "(" ++ (intercalate ", " (map prettyTerm ts)) ++ ")"
prettyAtom (Bind s d) = prettyTerm d ++ " := " ++ prettyTerm s

prettyTerm (Constant c) = show c
prettyTerm (Variable v) = v
prettyTerm (Expr name ts _) = name ++ "(" ++ intercalate ", " (map prettyTerm ts) ++ ")"
prettyTerm (Fresh ts) = "$(" ++ intercalate ", " (map prettyTerm ts) ++ ")"

prettyClause (Clause hs ts) = intercalate ", " (map prettyAtom hs) ++ " <- " ++ intercalate ", " (map prettyAtom ts)
prettyClause (SubsumptionClause hl hr bs) = prettyAtom hl ++ " <= " ++ prettyAtom hr ++ " <- " ++ intercalate ", " (map prettyAtom bs)

prettyProgram (Program cs) = intercalate "\n" (map prettyClause cs)


infix 0 +=
(+=) :: [Atom a s] -> [Atom a s] -> Clause a s
(+=) = Clause

lit :: String -> [Term a] -> Atom a s
lit s ts  = Literal s ts id

bind :: Term a -> Term a -> Atom a s
bind = Bind

val :: s -> Atom a s
val = Value

cst :: a -> Term a
cst = Constant

var :: String -> Term a
var = Variable

expr :: String -> ([a] -> a) -> [Term a] -> Term a
expr name f ts = Expr name ts f

clause :: [Atom a s] -> ([Atom a s] -> Clause a s) -> Clause a s
clause head rest = rest head
