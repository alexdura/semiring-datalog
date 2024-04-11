module PicoJava (AST(..), parseStmt, parseProgram, parseAccess, parseBlock, pretty) where

import Text.Parsec
import Text.Parsec.Token as P
import Text.Parsec.Language (javaStyle)

data AST a = AST {kind::a, token::String, children::[AST a]} deriving (Eq, Show)

pretty :: Show a => AST a -> String
pretty = prettyIndent 0

prettyIndent :: Show a => Int -> AST a -> String
prettyIndent n node = replicate n '\t' ++ show node.kind ++ " \"" ++ node.token ++ "\"\n" ++ concatMap (prettyIndent (n + 1)) node.children

unknonwnClass = AST "UnknownClass" "" []

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

classDecl1 = (\n b -> AST "Class" n [unknonwnClass, b]) <$ (lexer.reserved "class") <*> name <*> block

classDecl2 = (\n u b -> AST "Class" n [u, b])
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
