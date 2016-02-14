{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module Prim where

--------------------------------------------------------------------------------

import Pretty

--------------------------------------------------------------------------------

-- | A type representing primitive integers.
newtype PrimInt = MkPrimInt Int
                  deriving (Eq, Ord, Show, Num, Integral, Enum, Real)

-- | Enumerates primitive (built-in) operators.
data PrimOp = PrimAdd   -- ^ The addition operator.
            | PrimSub   -- ^ The subtraction operator.
            | PrimMul   -- ^ The multiplication operator.
            | PrimDiv   -- ^ The division operator.
            deriving (Eq, Show)

mkPrimInt :: String -> PrimInt
mkPrimInt = MkPrimInt . read . init

--------------------------------------------------------------------------------

instance PP PrimInt where
    pp (MkPrimInt n) = int n <> char '#'

instance PP PrimOp where
    pp PrimAdd = text "+#"
    pp PrimSub = text "-#"
    pp PrimMul = text "*#"
    pp PrimDiv = text "/#"
