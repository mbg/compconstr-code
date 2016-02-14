--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module Token (
    module Posn,
    module Prim,

    --TokenP(..),
    Token(..)
) where

--------------------------------------------------------------------------------

import Posn
import Pretty
import Prim

--------------------------------------------------------------------------------

-- | Enumerates token types.
data Token = TVar String -- ^ Variables
           | TCtr String -- ^ Constructors

           | TEquals
           | TSemicolon
           | TArrow
           | TCurlyL
           | TCurlyR
           | TComma
           | TBar
           | TParL
           | TParR

           | TLet
           | TLetRec
           | TCase
           | TIn
           | TOf
           | TDefault
           | TUpdatable
           | TNotUpdatable
           | TType

           | TIntTy

           | TPrimInt PrimInt
           | TPrimOp PrimOp

           | TEoF

           deriving (Eq, Show)

--------------------------------------------------------------------------------

instance PP Token where
    pp (TVar xs)     = text "variable" <+> text xs
    pp (TCtr xs)     = text "constructor" <+> text xs

    pp TEquals       = char '='
    pp TSemicolon    = char ';'
    pp TArrow        = text "->"
    pp TCurlyL       = char '{'
    pp TCurlyR       = char '}'
    pp TComma        = char ','
    pp TBar          = char '|'
    pp TParL         = char '('
    pp TParR         = char ')'

    pp TLet          = text "let"
    pp TLetRec       = text "letrec"
    pp TCase         = text "case"
    pp TIn           = text "in"
    pp TOf           = text "of"
    pp TDefault      = text "default"
    pp TType         = text "type"

    pp TIntTy        = text "Int#"

    pp TUpdatable    = text "\\u"
    pp TNotUpdatable = text "\\n"

    pp (TPrimInt n)  = pp n
    pp (TPrimOp op)  = pp op

    pp TEoF          = text "end of file"
