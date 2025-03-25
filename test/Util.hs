module Util where

import qualified PicoJava
import qualified SaigaPicoJava
import qualified Saiga

import Data.Maybe
import Control.Monad
import Data.List

type PicoJavaAST = PicoJava.AST (String, Int)


findNodeByPath :: PicoJavaAST -> [Int] -> PicoJavaAST
findNodeByPath ast [] = ast
findNodeByPath ast (n:ns) = findNodeByPath ((PicoJava.children ast) !! n) ns


findNodeById :: Eq t => PicoJava.AST (a, t) -> t -> Maybe (PicoJava.AST (a, t))
findNodeById ast nid = if (snd $ PicoJava.kind ast) == nid then Just ast
                       else join $ find isJust ((\ast' -> findNodeById ast' nid) <$> ast.children)

nodeId :: Saiga.Domain (a, b) -> b
nodeId (Saiga.DNode (PicoJava.AST (_, i) _ _)) = i

evalExpr :: Saiga.SaigaAttribute a
         => (PicoJavaAST -> Saiga.SaigaProgram a (String, Int)) -- Saiga program
         -> PicoJavaAST -- AST
         -> [Int] -- path
         -> Saiga.Expr a -- expression to evaluate
         -> Saiga.Domain (String, Int) -- result
evalExpr p ast path expr =
  let n = findNodeByPath ast path
      ctx = Saiga.makeAttributeCtx (p ast)
      (r, log) = Saiga.evalWithLog ctx [Saiga.DList []] n expr
  in
    case r of
      Left err -> error $ err ++ "\n" ++ prettyLog log
      Right r -> r



prettyLogEntry :: (Show a1, Show a2) => Saiga.LogEntry a2 a1 -> String
prettyLogEntry (Saiga.LogEntry args n e r) = "ARG=(" ++ intercalate ", " (Saiga.prettyDomain <$> args) ++ ") NODE="
                                             ++ show n.kind ++ " " ++ Saiga.prettyExpr 0 e ++ " R="
                                             ++ (case r of Saiga.DNode r' -> show r'.kind
                                                           _ -> Saiga.prettyDomain r)


prettyLog :: (Show a1, Show a2) => [Saiga.LogEntry a2 a1] -> String
prettyLog log = intercalate "\n" (prettyLogEntry <$> log)


evalExpr1 :: Saiga.SaigaAttribute a
          => (PicoJava.AST (String, Int) -> Saiga.SaigaProgram a (String, Int)) -- Saiga program
          -> PicoJava.AST (String, Int) -- AST
          -> Int -- node id
          -> Saiga.Expr a -- expression to evaluate
          -> Saiga.Domain (String, Int) -- result
evalExpr1 p ast nid expr =
  let n = fromJust $ findNodeById ast nid
      ctx = Saiga.makeAttributeCtx (p ast)
      (r, log) = Saiga.evalWithLog ctx [Saiga.DList []] n expr
  in
    case r of
      Left err -> error $ err ++ "\n" ++ prettyLog log
      Right r -> r

evalExprDebug :: Saiga.SaigaAttribute a
              => (PicoJava.AST (String, Int) -> Saiga.SaigaProgram a (String, Int)) -- Saiga program
              -> PicoJava.AST (String, Int) -- AST
              -> Int -- node id
              -> Saiga.Expr a -- expression to evaluate
              -> Int -- log size
              -> String -- result
evalExprDebug p ast nid expr logsize =
  let n = fromJust $ findNodeById ast nid
      ctx = Saiga.makeAttributeCtx (p ast)
      log = take logsize $ snd $ Saiga.evalWithLog ctx [Saiga.DList []] n expr
  in
    prettyLog log

parseAndNumber :: String -> PicoJavaAST
parseAndNumber s = case PicoJava.parseProgram s >>= (return . PicoJava.numberNodes) of
  Left e -> error $ "Error parsing program: " ++ show e
  Right ast -> ast
