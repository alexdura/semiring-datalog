module Datalog (Program(..), Clause(..), Atom(..), Term(..), Predicate, Trace(..), (+=),
                (+=|), (|.),
                var, cst, lit, val, prettyProgram) where


import Data.List

data Program a b = Program [Clause a b]

data Trace a b = Trace [Term a b] ([a] -> b -> b)

data Clause a b = Clause { heads :: [Atom a b], body :: [Atom a b],  trace :: Trace a b}

data Atom a b = Literal Predicate [Term a b] (b -> b)
              | Value b
              | Function [Term a b] ([a] -> b)


data Term a b = Variable String
              | Constant a
              | Expr [Term a b] ([a] -> a)


type Predicate = String


prettyAtom (Literal p ts _) = p ++ "(" ++ (intercalate ", " (map prettyTerm ts)) ++ ")"
prettyAtom (Value b) = show b

prettyTerm (Constant c) = show c
prettyTerm (Variable v) = show v

prettyClause (Clause hs ts _) = intercalate ", " (map prettyAtom hs) ++ " <- " ++ intercalate ", " (map prettyAtom ts)

prettyProgram (Program cs) = intercalate "\n" (map prettyClause cs)


infix 0 +=
(+=) :: [Atom a s] -> [Atom a s] -> Clause a s
(+=) h b = Clause h b (Trace [] (const id))

lit :: String -> [Term a s] -> Atom a s
lit s ts  = Literal s ts id

val :: s -> Atom a s
val = Value

cst :: a -> Term a s
cst = Constant

var :: String -> Term a s
var = Variable

withTrace :: Trace a s -> [Atom a s] -> [Atom a s] -> Clause a s
withTrace trace body = \head -> Clause head body trace

clause :: [Atom a s] -> ([Atom a s] -> Clause a s) -> Clause a s
clause head rest = rest head

infix 1 |.
(|.) = withTrace

infix 0 +=|
(+=|)= clause
