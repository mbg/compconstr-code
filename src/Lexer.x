{{-# OPTIONS_GHC -fno-warn-tabs #-}

--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module Lexer (
    AlexState(..),
    AlexPosn(..),

    TokenP,

    alexMonadScan,
    runAlexFrom,

    runAlex,
    runLexer
) where

import Pretty
import Token
}

%wrapper "monad"

$ignore    = [\ \n\t\f\v\r]
$digit     = 0-9
$lower     = a-z
$upper     = A-Z
$idchar    = [$lower $upper $digit \'\_]
$op        = [\+\-\*\/]
@var       = $lower $idchar*
@ctr       = $upper $idchar*
@pint      = $digit+ "#"

stg :-
    $ignore+                    { skip                                      }
    "--" .*                     { skip                                      }

    "="                         { makeToken TEquals                         }
    ";"                         { makeToken TSemicolon                      }
    "->"                        { makeToken TArrow                          }
    "{"                         { makeToken TCurlyL                         }
    "}"                         { makeToken TCurlyR                         }
    ","                         { makeToken TComma                          }
    "|"                         { makeToken TBar                            }
    "("                         { makeToken TParL                           }
    ")"                         { makeToken TParR                           }

    "let"                       { makeToken TLet                            }
    "letrec"                    { makeToken TLetRec                         }
    "case"                      { makeToken TCase                           }
    "in"                        { makeToken TIn                             }
    "of"                        { makeToken TOf                             }
    "default"                   { makeToken TDefault                        }
    "type"                      { makeToken TType                           }

    "Int#"                      { makeToken TIntTy                          }

    "\u"                        { makeToken TUpdatable                      }
    "\n"                        { makeToken TNotUpdatable                   }

    @pint                       { makeTokenWith (TPrimInt . mkPrimInt)      }

    "+#"                        { makeToken (TPrimOp PrimAdd)               }
    "-#"                        { makeToken (TPrimOp PrimSub)               }
    "*#"                        { makeToken (TPrimOp PrimMul)               }
    "/#"                        { makeToken (TPrimOp PrimDiv)               }

    @var                        { makeTokenWith TVar                        }
    @ctr                        { makeTokenWith TCtr                        }
{

type TokenP = (Token, AlexPosn)

instance PP AlexPosn where
    pp (AlexPn a l c) = angulars $ int l <> colon <> int c

makeTokenWith :: (String -> Token) -> AlexAction TokenP
makeTokenWith t (p, _, _, xs) n = return $ (t (take n xs), p)

makeToken :: Token -> AlexAction TokenP
makeToken t = makeTokenWith (const t)

alexEOF :: Alex TokenP
alexEOF = do
    return (TEoF, AlexPn 0 0 0)

runLexer :: Alex [TokenP]
runLexer = do
    t <- alexMonadScan
    case fst t of
        TEoF -> return [t]
        _   -> do
            ts <- runLexer
            return (t:ts)

runAlexFrom :: Alex a -> AlexState -> Either String (AlexState, a)
runAlexFrom (Alex m) s = m s

}
