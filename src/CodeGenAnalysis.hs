--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module CodeGenAnalysis where

--------------------------------------------------------------------------------

import AST
import Types

--------------------------------------------------------------------------------

closureSize :: ALambdaForm a -> Int
closureSize (MkLambdaForm fvs _ _ _) = 1 + length fvs

--------------------------------------------------------------------------------

class TypeInfo t where
    -- | `isPrimitive t' determines whether `t' is a primitive type.
    isPrimitive   :: t -> Bool
    -- | `algebraicType t' returns the name of the algebraic type represented
    --   by `t', if any.
    algebraicType :: t -> Maybe String

instance TypeInfo Type where
    isPrimitive PrimIntTy = True
    isPrimitive _         = False

    algebraicType (AlgTy n)   = Just n
    algebraicType (AppTy f _) = algebraicType f
    algebraicType _           = Nothing

instance TypeInfo PolyType where
    isPrimitive (MonoTy mt) = isPrimitive mt
    isPrimitive _           = False

    algebraicType (MonoTy mt)    = algebraicType mt
    algebraicType (QuantTy _ pt) = algebraicType pt

--------------------------------------------------------------------------------
