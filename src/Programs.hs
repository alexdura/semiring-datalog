module Programs(andersen) where

import Datalog


andersen :: Program Int Bool
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
      interProcAssign = lit "InterProcAssign"

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


  in
    Program [
    [varPointsTo [var, heap]] += [reachable [f], alloc [var, heap, f]],
    [varPointsTo [to, heap]] += [move [to, from], varPointsTo [from, heap]],
    [locPointsTo [baseH, heap]] += [store [base, from], varPointsTo [from, heap], varPointsTo [base, baseH]],
    [varPointsTo [to, heap]] += [load [to, base], varPointsTo [base, baseH], locPointsTo [baseH, heap]],
    [reachable [callee], callGraph[c, callee]] += [vcall [base, c, caller], reachable [caller], varPointsTo[base, callee]],
    [reachable [callee], callGraph[c, callee]] += [call[callee, c, f], reachable [f]],
    [interProcAssign [to, from]] += [callGraph [c, f], formalArg[f, n, to], actualArg[c, n, from]],
    [interProcAssign [to, from]] += [callGraph [c, f], formalReturn[f, from], actualReturn [c, to]],
    [varPointsTo [to, heap]] += [interProcAssign [to, from], varPointsTo[from, heap]]
    ]
