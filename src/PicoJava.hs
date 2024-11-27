{-# LANGUAGE DeriveFoldable #-}

module PicoJava (AST(..), parseStmt, parseProgram, parseAccess, parseBlock, pretty, numberNodes, parentMap) where

import Text.Parsec hiding (State)
import Text.Parsec.Token as P
import Text.Parsec.Language (javaStyle)
import Control.Monad.State.Strict
import Data.Map.Strict hiding (foldl)
import AST


-- parsing
unknonwnClass = AST "UnknownClass" "_unknown_" []

lexer = P.makeTokenParser javaStyle

name = P.identifier lexer

use = (\n -> AST "Use" n []) <$> name

dot = (\a u -> AST "Dot" "." [a, u])  <$> access <* lexer.dot <*> use

access = foldl1 (\u us -> AST "Dot" "." [u, us]) <$> sepBy1 use lexer.dot

boolLit = try trueLit <|> falseLit

trueLit = AST "BoolLit" "true" [] <$ lexer.reserved "true"

falseLit = AST "BoolLit" "false" [] <$ lexer.reserved "false"

exp = try boolLit <|> access

stmt = (\a e -> AST "Stmt" "" [a, e]) <$> access <* eq <*> PicoJava.exp <* lexer.semi
--stmt = access <*> exp <*>

eq = lexer.reservedOp "="

block = P.braces lexer $ AST "Block" "" <$> many item

classDecl1 = (\n b -> AST "ClassDecl" n [unknonwnClass, b]) <$ (lexer.reserved "class") <*> name <*> block

classDecl2 = (\n u b -> AST "ClassDecl" n [u, b])
  <$ lexer.reserved "class"
  <*> name
  <* lexer.reserved "extends"
  <*> use
  <*> block

classDecl = try classDecl1 <|> classDecl2

varDecl =  (\a n -> AST "VarDecl" n [a]) <$> access <*> name <* lexer.semi

decl = try classDecl <|> varDecl

item = try decl <|> stmt

program = AST "Program" "" <$> many decl

parseStmt = Text.Parsec.parse (PicoJava.stmt <* eof) "input"
parseAccess = Text.Parsec.parse (PicoJava.access <* eof) "input"
parseProgram = Text.Parsec.parse (PicoJava.program <* eof) "input"
parseBlock = Text.Parsec.parse (PicoJava.block <* eof) "input"
