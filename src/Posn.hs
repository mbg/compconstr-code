--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module Posn where

--------------------------------------------------------------------------------

import Pretty

--------------------------------------------------------------------------------

-- | Positions.
data Posn
 = EoFPosn
   -- | No position.
 | NoPosn
   -- | A position within a file.
 | FilePosn {
    -- posnFileName :: FilePath,   -- ^ The name of the file.
    posnLine     :: Int,        -- ^ The line number.
    posnColumn   :: Int         -- ^ The column number.
 }

--------------------------------------------------------------------------------

instance PP Posn where
    pp EoFPosn        = empty
    pp NoPosn         = angulars $ text "No Position"
    pp (FilePosn l c) = angulars $
        int l <> colon <> int c
