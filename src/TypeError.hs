--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module TypeError where

--------------------------------------------------------------------------------

import Posn
import Var
import Types
import Pretty

--------------------------------------------------------------------------------

data TyError
    = MkTyError Posn TyErrorMsg

data TyErrorMsg
    = NotInScopeErr String
    | OpTypeErr
    | DuplicateCtrErr String
    | UnifyErr Type Type
    | OccursErr (AVar ()) Type

--------------------------------------------------------------------------------

instance PP TyError where
    pp (MkTyError p err) = pp p <> pp err

instance PP TyErrorMsg where
    pp (NotInScopeErr x)   = text "Not in scope: " <+> var x
    pp OpTypeErr =
        text "A built-in operator is applied to a non-primitive type."
    pp (DuplicateCtrErr x)
        = text "The constructor"
      <+> var x
      <+> text "is defined more than once"
    pp (UnifyErr t0 t1)
        = hang (text "Cannot unify types:") 4 (pp t0)
       $$ hang (text "and") 4 (pp t1)
    pp (OccursErr v t)
        = text "Occurs check failed. The variable"
      <+> var (varName v)
      <+> text "occurs in:"
       $$ nest 4 (pp t)

--------------------------------------------------------------------------------
