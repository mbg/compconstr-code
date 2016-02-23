--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module CodeGenAnalysis where

--------------------------------------------------------------------------------

import AST
import Types

--------------------------------------------------------------------------------

-- | `closureSize lf` calculates the size of a closure in words. (At least 2.)
closureSize :: ALambdaForm a -> Int
closureSize (MkLambdaForm fvs _ _ _) = max 2 (1 + length fvs)

--------------------------------------------------------------------------------

class DoesHeapAlloc a where
    heapCost :: a -> Int

instance DoesHeapAlloc (ABind a) where
    -- this should calculate the amount of memory needed to store a closure
    -- for this binding / lambda-form on the heap
    heapCost (MkBind v lf t) = 0

instance DoesHeapAlloc (AExpr a) where
    heapCost (LetE bs e a)    = 0
    heapCost (LetRecE bs e a) = 0
    heapCost (CaseE e as a)   = 0
    heapCost (CtrE c as a)    = 0
    heapCost _                = 0

instance DoesHeapAlloc (AAlts a) where
    heapCost (PrimAlts [] d) = 0
    heapCost (PrimAlts as d) = 0
    heapCost (AlgAlts [] d)  = 0
    heapCost (AlgAlts as d)  = 0

instance DoesHeapAlloc (ADefaultAlt a) where
    heapCost (Default e t)      = 0
    heapCost (DefaultVar v e t) = 0

instance DoesHeapAlloc (APrimAlt a) where
    heapCost (PAlt k e t) = 0

instance DoesHeapAlloc (AAlgAlt a) where
    heapCost (AAlt c as e t) = 0

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
