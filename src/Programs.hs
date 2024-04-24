module Programs(andersen, andersenCallSiteSensitive) where

import Eval
import Datalog
import qualified Context
import Text.Read
import Text.Printf
import Data.Semiring


instance DatalogGroundTerm (Either String Int) where
  unparse = \case Left s -> s
                  Right i -> printf "%d" i
  parse = fst . head . (readPrec_to_S parseGroundTerm 0)
  firstIndex = Right 0
  nextIndex (Right n) = Right (n Prelude.+ 1)

parseAsInt :: ReadPrec (Either String Int)
parseAsInt = do
  i  <- readPrec :: ReadPrec Int
  return $ Right i

parseAsString :: ReadPrec (Either String Int)
parseAsString = Left <$> look

parseGroundTerm :: ReadPrec (Either String Int)
parseGroundTerm = parseAsInt <++ parseAsString


andersen :: Program a s
andersen =
  let alloc = lit "Alloc"
      move = lit "Move"
      load = lit "Load"
      store = lit "Store"
      call = lit "Call"
      vcall = lit "VCall"
      formalArg = lit "FormalArg"
      actualArg = lit "ActualArg"
      formalReturn = lit "FormalReturn"
      actualReturn = lit "ActualReturn"
      callGraph = lit "CallGraph"

      varPointsTo = lit "VarPointsTo"
      locPointsTo = lit "LocPointsTo"
      reachable = lit "Reachable"
      reachableInit = lit "ReachableInit"
      interProcAssign = lit "InterProcAssign"

      varPointsToLoc = lit "VarPointsToLoc"
      srcLoc = lit "SrcLoc"

      var = Datalog.var "var"
      from = Datalog.var "from"
      to = Datalog.var "to"
      f = Datalog.var "f"
      c = Datalog.var "c"
      n = Datalog.var "n"
      heap = Datalog.var "heap"
      base = Datalog.var "base"
      baseH = Datalog.var "baseH"
      caller = Datalog.var "caller"
      callee = Datalog.var "callee"

      f_to = Datalog.var "f_to"
      l_to = Datalog.var "l_to"
      c_to = Datalog.var "c_to"
      f_heap = Datalog.var "f_heap"
      l_heap = Datalog.var "l_heap"
      c_heap = Datalog.var "c_heap"



  in
    Program [
    [varPointsTo [var, heap]] += [reachable [f], alloc [var, heap, f]],
    [varPointsTo [to, heap]] += [move [to, from], varPointsTo [from, heap]],
    [locPointsTo [baseH, heap]] += [store [base, from], varPointsTo [from, heap], varPointsTo [base, baseH]],
    [varPointsTo [to, heap]] += [load [to, base], varPointsTo [base, baseH], locPointsTo [baseH, heap]],
    [reachable [callee], callGraph[c, callee]] += [vcall [base, c, caller], reachable [caller], varPointsTo[base, callee]],
    [reachable [callee], callGraph[c, callee]] += [call[callee, c, f], reachable [f]],
    [reachable [callee]] += [reachableInit [callee]],
    [interProcAssign [to, from]] += [callGraph [c, f], formalArg[f, n, to], actualArg[c, n, from]],
    [interProcAssign [to, from]] += [callGraph [c, f], formalReturn[f, from], actualReturn [c, to]],
    [varPointsTo [to, heap]] += [interProcAssign [to, from], varPointsTo[from, heap]],
    [varPointsToLoc [f_to, l_to, c_to, f_heap, l_heap, c_heap]] += [varPointsTo[to, heap],
                                                                    srcLoc[to, f_to, l_to, c_to],
                                                                    srcLoc[heap, f_heap, l_heap, c_heap]]
    ]


data GroundTerm1 = GInt Int
                 | GString String
                 | GCtx Int Int
                 | GHeapCtx
                 deriving (Show, Eq, Ord, Read)

instance DatalogGroundTerm GroundTerm1 where
  unparse = \case GInt i -> printf "%d" i
                  GString s -> s
                  GHeapCtx -> "*"
                  GCtx c0 c1 -> printf "(%d, %d)" c0 c1
  parse input = case fst . head . (readPrec_to_S parseGroundTerm 0) $ input of
                  Left s -> GString s
                  Right i -> GInt i
  firstIndex = GInt 0
  nextIndex (GInt n) = GInt $ n Prelude.+ 1

andersenCallSiteSensitive :: Program GroundTerm1 Bool
andersenCallSiteSensitive =
  let alloc = lit "Alloc"
      move = lit "Move"
      load = lit "Load"
      store = lit "Store"
      call = lit "Call"
      vcall = lit "VCall"
      formalArg = lit "FormalArg"
      actualArg = lit "ActualArg"
      formalReturn = lit "FormalReturn"
      actualReturn = lit "ActualReturn"
      callGraph = lit "CallGraph"
      funcPtr = lit "FunPtr"

      varPointsTo = lit "VarPointsTo"
      locPointsTo = lit "LocPointsTo"
      reachable = lit "Reachable"
      interProcAssign = lit "InterProcAssign"
      reachableInit = lit "ReachableInit"

      varPointsToLoc = lit "VarPointsToLoc"
      srcLoc = lit "SrcLoc"

      var = Datalog.var "var"
      from = Datalog.var "from"
      to = Datalog.var "to"
      f = Datalog.var "f"
      c = Datalog.var "c"
      n = Datalog.var "n"
      heap = Datalog.var "heap"
      base = Datalog.var "base"
      baseH = Datalog.var "baseH"
      caller = Datalog.var "caller"
      callee = Datalog.var "callee"
      func = Datalog.var "func"

      f_to = Datalog.var "f_to"
      l_to = Datalog.var "l_to"
      c_to = Datalog.var "c_to"
      f_heap = Datalog.var "f_heap"
      l_heap = Datalog.var "l_heap"
      c_heap = Datalog.var "c_heap"
      ctx = Datalog.var "ctx"
      hctx = Datalog.var "hctx"
      baseHCtx = Datalog.var "baseHCtx"
      -- calleeCtx = Datalog.var "calleeCtx"
      callerCtx = Datalog.var "callerCtx"
      calleeCtx = Datalog.var "calleeCtx"
      toCtx = Datalog.var "toCtx"
      fromCtx = Datalog.var "fromCtx"
      _1 = Datalog.var "_1"
      _2 = Datalog.var "_2"

      initialCtx = GCtx 0 0
      merge [GInt c,
             GCtx callerCtx0 callerCtx1] = GCtx c callerCtx0

      record [GInt heap, GCtx ctx0 ctx1] = GHeapCtx

  in
    Program [
    [varPointsTo [var, ctx, heap, expr record [heap, ctx]]] += [reachable [f, ctx], alloc [var, heap, f]],
    [varPointsTo [var, ctx, func, expr record [func, ctx]]] += [reachable [f, ctx], funcPtr [var, func, f]],

    [varPointsTo [to, ctx, heap, hctx]] += [move [to, from], varPointsTo [from, ctx, heap, hctx]],

    [locPointsTo [baseH, baseHCtx, heap, hctx]] += [store [base, from], varPointsTo [from, ctx, heap, hctx], varPointsTo [base, ctx, baseH, baseHCtx]],

    [varPointsTo [to, ctx, heap, hctx]] += [load [to, base], varPointsTo [base, ctx, baseH, baseHCtx], locPointsTo [baseH, baseHCtx, heap, hctx]],

    [reachable [callee, Datalog.expr merge [c, callerCtx]],
     callGraph[c, callerCtx, callee, Datalog.expr merge [c, callerCtx]]] +=
      [vcall [base, c, caller], reachable [caller, callerCtx], varPointsTo[base, callerCtx, callee, hctx], funcPtr[_1, callee, _2]],

    [reachable [callee, Datalog.expr merge [c, callerCtx]],
     callGraph[c, callerCtx, callee, Datalog.expr merge [c, callerCtx]]] += [call[callee, c, f], reachable [f, callerCtx]],

    [reachable [callee, cst initialCtx]] += [reachableInit [callee]],

    [interProcAssign [to, calleeCtx, from, callerCtx]] += [callGraph [c, callerCtx, f, calleeCtx], formalArg[f, n, to], actualArg[c, n, from]],
    [interProcAssign [to, callerCtx, from, calleeCtx]] += [callGraph [c, callerCtx, f, calleeCtx], formalReturn[f, from], actualReturn [c, to]],
    [varPointsTo [to, toCtx, heap, hctx]] += [interProcAssign [to, toCtx, from, fromCtx], varPointsTo[from, fromCtx, heap, hctx]],

    [varPointsToLoc [f_to, l_to, c_to, f_heap, l_heap, c_heap]] += [varPointsTo[to, ctx, heap, hctx],
                                                                    srcLoc[to, f_to, l_to, c_to],
                                                                    srcLoc[heap, f_heap, l_heap, c_heap]]
    ]
