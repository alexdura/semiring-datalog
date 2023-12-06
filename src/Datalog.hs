module Datalog (Program(..), Clause(..), Atom(..), Term(..), Predicate, (|-), var, cst, lit, val, prettyProgram) where


import Data.List

data Program a b = Program [Clause a b] deriving Show

data Clause a b = Clause { heads :: [Atom a b], body :: [Atom a b] } deriving Show

data Atom a b = Literal Predicate [Term a]
              | Value b
              deriving Show

data Term a = Variable String
            | Constant a
            deriving Show


type Predicate = String


prettyAtom (Literal p ts) = p ++ "(" ++ (intercalate ", " (map prettyTerm ts)) ++ ")"
prettyAtom (Value b) = show b

prettyTerm (Constant c) = show c
prettyTerm (Variable v) = show v

prettyClause (Clause hs ts) = intercalate ", " (map prettyAtom hs) ++ " <- " ++ intercalate ", " (map prettyAtom ts)

prettyProgram (Program cs) = intercalate "\n" (map prettyClause cs)

infix 0 |-
(|-) :: [Atom a s] -> [Atom a s] -> Clause a s
(|-) = Clause

lit :: String -> [Term a] -> Atom a s
lit = Literal

val :: s -> Atom a s
val = Value

cst :: a -> Term a
cst = Constant

var :: String -> Term a
var = Variable
