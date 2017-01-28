{
--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module Parser (programP, parseFile) where

--------------------------------------------------------------------------------

import Data.DList

import Token
import Lexer
import P
import AST
import Pretty (render, pp)

}

--------------------------------------------------------------------------------

%tokentype { TokenP }
%monad     { P } { >>= } { return }
%lexer     { lexer } { (TEoF, _) }
%error     { parseError }

--------------------------------------------------------------------------------

%name programP prog

--------------------------------------------------------------------------------

%token
    VAR                              { (TVar _, _)                          }

    '='                              { (TEquals, $$)                        }
    ';'                              { (TSemicolon, $$)                     }
    '->'                             { (TArrow, $$)                         }
    '{'                              { (TCurlyL, $$)                        }
    '}'                              { (TCurlyR, $$)                        }
    ','                              { (TComma, $$)                         }

    LET                              { (TLet, $$)                           }
    LETREC                           { (TLetRec, $$)                        }
    CASE                             { (TCase, $$)                          }
    IN                               { (TIn, $$)                            }
    OF                               { (TOf, $$)                            }
    DEFAULT                          { (TDefault, $$)                       }
    U                                { (TUpdatable, $$)                     }
    N                                { (TNotUpdatable, $$)                  }

    INT                              { (TPrimInt _, _)                      }
    OP                               { (TPrimOp _, _)                       }

%%

--------------------------------------------------------------------------------

prog :: { Prog }
prog : binds                         { MkProg $1                            }

--------------------------------------------------------------------------------

binds :: { [Bind] }
binds : bindL                        { toList $1                            }

bindL :: { DList Bind }
bindL : bind                         { singleton $1                         }
      | bindL bind                   { snoc $1 $2                           }

bind :: { Bind }
bind : VAR '=' lf ';'                {% mkBind $1 $3                        }

--------------------------------------------------------------------------------

lf :: { LambdaForm }
lf : vars uf vars '->' expr          {% mkLambdaForm $1 $2 $3 $5            }

uf :: { UpdateFlag }
uf : U                               { U                                    }
   | N                               { N                                    }

--------------------------------------------------------------------------------

expr :: { Expr }
expr : LET binds IN expr             {% mkLetE $1 $2 $4                     }
     | LETREC binds IN expr          {% mkLetRecE $1 $2 $4                  }
     | CASE expr OF alts             {% mkCaseE $1 $2 $4                    }
     | VAR atoms                     {% mkAppE $1 $2                        }
     | OP atoms                      {% mkOpE $1 $2                         }
     | INT                           {% mkLitE $1                           }

--------------------------------------------------------------------------------

alts :: { Alts }
alts : default                       { AlgAlts [] $1                        }
     | aalts default                 { AlgAlts $1 $2                        }
     | palts default                 { PrimAlts $1 $2                       }

aalts :: { [AlgAlt] }
aalt : aaltL                         { toList $1                            }

aaltL :: { DList AlgAlt }
aaltL : aalt ';'                     { singleton $1                         }
      | aaltL aalt ';'               { snoc $1 $2                           }

aalt :: { AlgAlt }
aalt :                               { undefined                            }

palts :: { [PrimAlt] }
palts : paltL                        { toList $1                            }

paltL :: { DList PrimAlt }
paltL : palt ';'                     { singleton $1                         }
      | paltL palt ';'               { snoc $1 $2                           }

palt :: { PrimAlt }
palt : INT '->' expr                 {% mkPrimAlt $1 $3                     }

default :: { DefaultAlt }
default : VAR '->' expr              {% mkDefaultVar $1 $3                  }
        | DEFAULT '->' expr          {% mkDefault $1 $3                     }

--------------------------------------------------------------------------------

vars :: { [Var] }
vars : '{'      '}'                  { []                                   }
     | '{' varL '}'                  { toList $2                            }

varL :: { DList Var }
varL : VAR                           { singleton (fromTVar $1)              }
     | varL ',' VAR                  { snoc $1 (fromTVar $3)                }

--------------------------------------------------------------------------------

atoms :: { [Atom] }
atoms : '{' '}'                      { [] }
      | '{' atomL '}'                { toList $2                            }

atomL :: { DList Atom }
atomL : atom                         { singleton $1                         }
      | atomL ',' atom               { snoc $1 $3                           }

atom :: { Atom }
atom : VAR                           {% mkVar $1                            }
     | INT                           {% mkInt $1                            }

--------------------------------------------------------------------------------

{

fromTVar :: TokenP -> String
fromTVar (TVar var, _) = var

-- | `lexer f' invokes the lexer with continuation `f'
lexer :: (TokenP -> P a) -> P a
lexer f = MkP $ \s -> case runAlexFrom alexMonadScan s of
    (Left err)     -> Left err
    (Right (s',t)) -> let (MkP m) = f t in m s'

}
