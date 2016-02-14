--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module FreeVars where

--------------------------------------------------------------------------------

import qualified Data.Map as M
import qualified Data.Set as S

import Var
import AST
import Types

--------------------------------------------------------------------------------

-- | Sets of global variables.
type Globals = S.Set String

--------------------------------------------------------------------------------

singleVar :: AVar a -> M.Map String a
singleVar (Var n ann) = M.singleton n ann

fromVar :: AVar a -> (String, a)
fromVar (Var n ann) = (n, ann)

-- | A special version of the function to calculate the free variables of
--   an STG binding, which *includes* globals. This is used to short
--   global bindings by their dependencies, for type inference purposes only.
class FreeVars f where
    freeVars :: Globals -> f a -> M.Map String a

instance FreeVars ABind where
    freeVars gs (MkBind _ lf ann) = freeVars gs lf

instance FreeVars ALambdaForm where
    freeVars gs (MkLambdaForm _ _ vs e) =
        M.difference (freeVars gs e) (M.fromList $ map fromVar vs)

instance FreeVars AExpr where
    freeVars gs (LetE bs e _) =
        -- compute the union of the free variables in the bindings and those
        -- that are free in the expression, minus those that are bound by `bs'
        zs `M.union` (freeVars gs e `M.difference` vs)
        where
            -- the list of names bound by `bs'
            vs = M.fromList $ map (fromVar . bindName) bs

            -- the set of all free variables in the bindings
            zs = M.unions $ map (freeVars gs) bs
    freeVars gs (LetRecE bs e _) =
        -- compute the union of the free variables in the bindings and those
        -- that are free in the expression, minus those that are bound by `bs'
        (zs `M.union` freeVars gs e) `M.difference` vs
        where
            -- the list of names bound by `bs'
            vs = M.fromList $ map (fromVar . bindName) bs

            -- the set of all free variables in the bindings
            zs = M.unions $ map (freeVars gs) bs
    freeVars gs (CaseE e alts _) =
        freeVars gs e `M.union` freeVars gs alts
    freeVars gs (AppE f as _)
        | varName f `S.member` gs = M.unions (map (freeVars gs) as)
        | otherwise               = singleVar f `M.union` M.unions (map (freeVars gs) as)
    freeVars gs (CtrE c as _) =
        M.unions $ map (freeVars gs) as
    freeVars gs (OpE op as _) =
        M.unions $ map (freeVars gs) as
    freeVars gs (LitE _ _) = M.empty

instance FreeVars AAlts where
    freeVars gs (AlgAlts alts def) =
        M.unions (map (freeVars gs) alts) `M.union` freeVars gs def
    freeVars gs (PrimAlts alts def) =
        M.unions (map (freeVars gs) alts) `M.union` freeVars gs def

instance FreeVars AAlgAlt where
    freeVars gs (AAlt c vs e _) =
        freeVars gs e `M.difference` M.fromList (map fromVar vs)

instance FreeVars APrimAlt where
    freeVars gs (PAlt k e _) = freeVars gs e

instance FreeVars ADefaultAlt where
    freeVars gs (Default e _)      = freeVars gs e
    freeVars gs (DefaultVar v e _) = M.delete (varName v) (freeVars gs e)

instance FreeVars AAtom where
    freeVars gs (VarAtom v _) | S.notMember (varName v) gs = singleVar v
                              | otherwise                  = M.empty
    freeVars gs (LitAtom _ _)                              = M.empty


--------------------------------------------------------------------------------
