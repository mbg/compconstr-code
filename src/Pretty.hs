--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module Pretty (
    module Text.PrettyPrint,

    PP(..),
    angulars,
    arrow,
    var
) where

--------------------------------------------------------------------------------

import Text.PrettyPrint

--------------------------------------------------------------------------------

class PP a where
    pp :: a -> Doc

instance PP () where
    pp () = empty

instance PP a => PP [a] where
    pp = brackets . sep . punctuate comma . map pp

instance (PP a, PP b) => PP (a,b) where
    pp (x,y) = pp x <> pp y

--------------------------------------------------------------------------------

-- | Wrap document in <...>
angulars :: Doc -> Doc
angulars doc = char '<' <> doc <> char '>'

arrow :: Doc
arrow = text "->"

var :: String -> Doc
var x = char '`' <> text x <> char '\''
