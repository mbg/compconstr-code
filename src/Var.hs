--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module Var where

--------------------------------------------------------------------------------

import Posn
import Pretty

--------------------------------------------------------------------------------

-- | Annotated variables.
data AVar a = Var {
    varName :: String,
    varAnn  :: a
} deriving (Eq, Functor)

-- | Variables with position annotations, for compatability.
type Var = AVar Posn

rmPosn :: Var -> AVar ()
rmPosn (Var n _) = Var n ()

--------------------------------------------------------------------------------

instance PP a => PP (AVar a) where
    pp (Var n ann) = pp ann <> text n

-- | `ppVars vs' pretty-prints a set of variables `vs'.
ppVars :: PP a => [AVar a] -> Doc
ppVars = braces . hcat . punctuate comma . map pp

--------------------------------------------------------------------------------
