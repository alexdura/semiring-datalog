module SaigaCFGLangSpec where

import Saiga
import CFGLang
import Test.Tasty
import Test.Tasty.HUnit
import AST
import Util

import Util (findNodeByPath, nodeId, parseAndNumber)

saigaCFGLangTests = testGroup "Saiga attributes for the CFG language" [
  testCase "Number nodes" $
    AST.numberNodes cfg1 @?= cfg1',

  testCase "Compute decl attributre" $
    let nid = 35 -- root node of the cfg
        expr = Node <.> FindDecl <?> [SVal "Y"]
    in nodeId (evalExpr1 cfgProgram cfg1' nid expr) @?= 19,

  testCase "Compute decl attributre" $
    let nid = 22 -- "Y"
        expr = Node <.> Decl <?> []
    in nodeId (evalExpr1 cfgProgram cfg1' nid expr) @?= 19,

  testCase "Compute X.nullable attribute - non circular" $
    let nid = 0 -- "NDecl X"
        expr = Node <.> Nullable <?> []
    in evalExpr1 cfgProgram cfg1' nid expr @?= DBool True,

  testCase "Compute Y.nullable attribute - non circular" $
    let nid = 11 -- "NDecl Y"
        expr = Node <.> Nullable <?> []
    in evalExpr1 cfgProgram cfg1' nid expr @?= DBool True,

  testCase "Compute Z.nullable attribute - circular" $
    let nid = 20 -- "NDecl Z"
        expr = Node <.> Nullable <?> []
    in evalExpr1 cfgProgram cfg1' nid expr @?= DBool False,

  testCase "Compute X.first attribute - non circular" $
    let nid = 0 -- "NDecl X"
        expr = Node <.> First <?> []
    in evalExpr1 cfgProgram cfg1' nid expr @?= (DList $ DString <$> ["a", "c"]),

  testCase "Compute Y.first attribute - non circular" $
    let nid = 11 -- "NDecl Y"
        expr = Node <.> First <?> []
    in evalExpr1 cfgProgram cfg1' nid expr @?= (DList [DString "c"]),

  testCase "Compute Z.first attribute - circular" $
    let nid = 20 -- "NDecl Z"
        expr = Node <.> First <?> []
    in evalExpr1 cfgProgram cfg1' nid expr @?= (DList $ DString <$> ["a", "c", "d"])

  ]


a = terminal "a"
c = terminal "c"
d = terminal "d"

cfg1 = cfg [
  --  X ::= Y
  --      | a
  --  Y ::= c
  --      | empty
  --  Z ::= XYZ
  --      | d
  rule (nDecl "X") (prods [symbols [nUse "Y"], symbols [a]]),
  rule (nDecl "Y") (prods [symbols [c], symbols []]),
  rule (nDecl "Z") (prods [symbols [nUse "X", nUse "Y", nUse "Z"], symbols [d]])
  ]


cfg1' =
  AST
    { kind = ("CFG", 35),
      token = "",
      children =
        [ AST
            { kind = ("Rule", 10),
              token = "",
              children =
                [ AST
                    { kind = ("NDecl", 0),
                      token = "X",
                      children =
                        []
                    },
                  AST
                    { kind = ("NonEmptyProdList", 9),
                      token = "",
                      children =
                        [ AST
                            { kind = ("NonEmptySymbolList", 3),
                              token = "",
                              children =
                                [ AST
                                    { kind = ("NUse", 1),
                                      token = "Y",
                                      children =
                                        []
                                    },
                                  AST
                                    { kind = ("EmptySymbolList", 2),
                                      token = "",
                                      children =
                                        []
                                    }
                                ]
                            },
                          AST
                            { kind = ("NonEmptyProdList", 8),
                              token = "",
                              children =
                                [ AST
                                    { kind = ("NonEmptySymbolList", 6),
                                      token = "",
                                      children =
                                        [ AST
                                            { kind = ("Terminal", 4),
                                              token = "a",
                                              children =
                                                []
                                            },
                                          AST
                                            { kind = ("EmptySymbolList", 5),
                                              token = "",
                                              children =
                                                []
                                            }
                                        ]
                                    },
                                  AST
                                    { kind = ("EmptyProdList", 7),
                                      token = "",
                                      children =
                                        []
                                    }
                                ]
                            }
                        ]
                    }
                ]
            },
          AST
            { kind = ("Rule", 19),
              token = "",
              children =
                [ AST
                    { kind = ("NDecl", 11),
                      token = "Y",
                      children =
                        []
                    },
                  AST
                    { kind = ("NonEmptyProdList", 18),
                      token = "",
                      children =
                        [ AST
                            { kind = ("NonEmptySymbolList", 14),
                              token = "",
                              children =
                                [ AST
                                    { kind = ("Terminal", 12),
                                      token = "c",
                                      children =
                                        []
                                    },
                                  AST
                                    { kind = ("EmptySymbolList", 13),
                                      token = "",
                                      children =
                                        []
                                    }
                                ]
                            },
                          AST
                            { kind = ("NonEmptyProdList", 17),
                              token = "",
                              children =
                                [ AST
                                    { kind = ("EmptySymbolList", 15),
                                      token = "",
                                      children =
                                        []
                                    },
                                  AST
                                    { kind = ("EmptyProdList", 16),
                                      token = "",
                                      children =
                                        []
                                    }
                                ]
                            }
                        ]
                    }
                ]
            },
          AST
            { kind = ("Rule", 34),
              token = "",
              children =
                [ AST
                    { kind = ("NDecl", 20),
                      token = "Z",
                      children =
                        []
                    },
                  AST
                    { kind = ("NonEmptyProdList", 33),
                      token = "",
                      children =
                        [ AST
                            { kind = ("NonEmptySymbolList", 27),
                              token = "",
                              children =
                                [ AST
                                    { kind = ("NUse", 21),
                                      token = "X",
                                      children =
                                        []
                                    },
                                  AST
                                    { kind = ("NonEmptySymbolList", 26),
                                      token = "",
                                      children =
                                        [ AST
                                            { kind = ("NUse", 22),
                                              token = "Y",
                                              children =
                                                []
                                            },
                                          AST
                                            { kind = ("NonEmptySymbolList", 25),
                                              token = "",
                                              children =
                                                [ AST
                                                    { kind = ("NUse", 23),
                                                      token = "Z",
                                                      children =
                                                        []
                                                    },
                                                  AST
                                                    { kind = ("EmptySymbolList", 24),
                                                      token = "",
                                                      children =
                                                        []
                                                    }
                                                ]
                                            }
                                        ]
                                    }
                                ]
                            },
                          AST
                            { kind = ("NonEmptyProdList", 32),
                              token = "",
                              children =
                                [ AST
                                    { kind = ("NonEmptySymbolList", 30),
                                      token = "",
                                      children =
                                        [ AST
                                            { kind = ("Terminal", 28),
                                              token = "d",
                                              children =
                                                []
                                            },
                                          AST
                                            { kind = ("EmptySymbolList", 29),
                                              token = "",
                                              children =
                                                []
                                            }
                                        ]
                                    },
                                  AST
                                    { kind = ("EmptyProdList", 31),
                                      token = "",
                                      children =
                                        []
                                    }
                                ]
                            }
                        ]
                    }
                ]
            }
        ]
    }
