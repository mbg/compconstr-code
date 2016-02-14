{
--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module Parser (programP, parseFile) where

--------------------------------------------------------------------------------

import Data.DList

import Var
import Token
import Lexer
import P
import AST
import Types
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
    CTR                              { (TCtr _, _)                          }

    '='                              { (TEquals, $$)                        }
    ';'                              { (TSemicolon, $$)                     }
    '->'                             { (TArrow, $$)                         }
    '{'                              { (TCurlyL, $$)                        }
    '}'                              { (TCurlyR, $$)                        }
    ','                              { (TComma, $$)                         }
    '|'                              { (TBar, $$)                           }
    '('                              { (TParL, $$)                          }
    ')'                              { (TParR, $$)                          }

    LET                              { (TLet, $$)                           }
    LETREC                           { (TLetRec, $$)                        }
    CASE                             { (TCase, $$)                          }
    IN                               { (TIn, $$)                            }
    OF                               { (TOf, $$)                            }
    DEFAULT                          { (TDefault, $$)                       }
    TYPE                             { (TType, $$)                          }

    TYINT                            { (TIntTy, $$)                         }

    U                                { (TUpdatable, $$)                     }
    N                                { (TNotUpdatable, $$)                  }

    INT                              { (TPrimInt _, _)                      }
    OP                               { (TPrimOp _, _)                       }

%%

--------------------------------------------------------------------------------

prog :: { Prog }
prog : tyBinds binds                 { MkProg $1 $2                         }

--------------------------------------------------------------------------------

tyBinds :: { [TyBind] }
tyBinds : tyBindL                    { toList $1                            }

tyBindL :: { DList TyBind }
tyBindL :                            { empty                                }
        | tyBindL tyBind             { snoc $1 $2                           }

tyBind :: { TyBind }
tyBind : TYPE CTR vars '=' dCtrs ';' {% mkTyBind $1 $2 $3 $5                }

dCtrs :: { [AlgCtr] }
dCtrs : dCtrL                        { toList $1                            }

dCtrL :: { DList AlgCtr }
dCtrL : dCtr                         { singleton $1                         }
      | dCtrL '|' dCtr               { snoc $1 $3                           }

dCtr :: { AlgCtr }
dCtr : CTR types                     {% mkAlgCtr $1 $2                      }

types :: { [Type] }
types :                              { []                                   }
      | typeL                        { toList $1                            }

typeL :: { DList Type }
typeL : type1                        { singleton $1                         }
      | typeL type1                  { snoc $1 $2                           }

type :: { Type }
type : type0 '->' type               { ArrTy $1 $3                          }
     | type0                         { $1                                   }

type0 :: { Type }
type0 : type0 type1                  { AppTy $1 $2                          }
      | type1                        { $1                                   }

type1 :: { Type }
type1 : TYINT                        { PrimIntTy                            }
      | CTR                          { AlgTy (fromTCtr $1)                  }
      | VAR                          { VarTy (Var (fromTVar $1) ())         }
      | '(' type ')'                 { $2                                   }

--------------------------------------------------------------------------------

binds :: { [Bind] }
binds : bindL                        { toList $1                            }

bindL :: { DList Bind }
bindL : bind                         { singleton $1                         }
      | bindL bind                   { snoc $1 $2                           }

bind :: { Bind }
bind : var '=' lf ';'                {% mkBind $1 $3                        }

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
     | var atoms                     {% mkAppE $1 $2                        }
     | CTR atoms                     {% mkCtrE $1 $2                        }
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
aalt : CTR vars '->' expr            {% mkAlgAlt $1 $2 $4                   }

palts :: { [PrimAlt] }
palts : paltL                        { toList $1                            }

paltL :: { DList PrimAlt }
paltL : palt ';'                     { singleton $1                         }
      | paltL palt ';'               { snoc $1 $2                           }

palt :: { PrimAlt }
palt : INT '->' expr                 {% mkPrimAlt $1 $3                     }

default :: { DefaultAlt }
default : var '->' expr              {% mkDefaultVar $1 $3                  }
        | DEFAULT '->' expr          {% mkDefault $1 $3                     }

--------------------------------------------------------------------------------

vars :: { [Var] }
vars : '{'      '}'                  { []                                   }
     | '{' varL '}'                  { toList $2                            }

varL :: { DList Var }
varL : var                           { singleton $1                         }
     | varL ',' var                  { snoc $1 $3                           }

var :: { Var }
var : VAR                            {% mkVar $1                            }

--------------------------------------------------------------------------------

atoms :: { [Atom] }
atoms : '{' '}'                      { [] }
      | '{' atomL '}'                { toList $2                            }

atomL :: { DList Atom }
atomL : atom                         { singleton $1                         }
      | atomL ',' atom               { snoc $1 $3                           }

atom :: { Atom }
atom : var                           {% mkVarAtom $1                        }
     | INT                           {% mkInt $1                            }

--------------------------------------------------------------------------------

{

fromTVar :: TokenP -> String
fromTVar (TVar var, _) = var

fromTCtr :: TokenP -> String
fromTCtr (TCtr ctr, _) = ctr

-- | `lexer f' invokes the lexer with continuation `f'
lexer :: (TokenP -> P a) -> P a
lexer f = MkP $ \s -> case runAlexFrom alexMonadScan s of
    (Left err)     -> error err
    (Right (s',t)) -> let (MkP m) = f t in m s'

}
