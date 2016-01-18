--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module AST where

--------------------------------------------------------------------------------

import Posn
import Pretty
import Prim

--------------------------------------------------------------------------------

-- | The type of variables.
type Var = String

-- | The type of constructors.
type Ctr = String

-- | Programs.
newtype Prog = MkProg { progBinds :: [Bind] }

-- | Bindings.
data Bind = MkBind {
    bindName :: Var,
    bindLF   :: LambdaForm,
    bindPosn :: Posn
}

-- | Lambda forms.
data LambdaForm = MkLambdaForm {
    lfFreeVars :: [Var],
    lfFlag     :: UpdateFlag,
    lfVars     :: [Var],
    lfExpr     :: Expr
}

data UpdateFlag = U | N

data Expr
    = LetE {
        letBinds :: [Bind],
        letExpr  :: Expr,
        letPosn  :: Posn
    }
    | LetRecE {
        letRecBinds :: [Bind],
        letRecExpr  :: Expr,
        letRecPosn  :: Posn
    }
    | CaseE {
        caseExpr :: Expr,
        caseAlts :: Alts,
        casePosn :: Posn
    }
    | AppE {
        appFun   :: Var,
        appAtoms :: [Atom],
        appPosn  :: Posn
    }
    | OpE {
        opType  :: PrimOp,
        opAtoms :: [Atom],
        opPosn  :: Posn
    }
    | LitE {
        litVal  :: PrimInt,
        litPosn :: Posn
    }

data Alts
    = AlgAlts [AlgAlt] DefaultAlt
    | PrimAlts [PrimAlt] DefaultAlt

data AlgAlt = AAlt {
    aaltCtr  :: Ctr,
    aaltVars :: [Var],
    aaltExor :: Expr,
    aaltPosn :: Posn
}

data PrimAlt = PAlt {
    paltVal  :: PrimInt,
    paltExpr :: Expr,
    paltPosn :: Posn
}

data DefaultAlt
    = DefaultVar {
        defaultVar  :: Var,
        defaultExpr :: Expr,
        defaultPosn :: Posn
    }
    | Default {
        defaultExpr :: Expr,
        defaultPosn :: Posn
    }

-- | Atoms.
data Atom
    = VarAtom {
        atomVar  :: Var,
        atomPosn :: Posn
    }
    | LitAtom {
        atomVal  :: PrimInt,
        atomPosn :: Posn
    }

--------------------------------------------------------------------------------

-- | `ppBinds bs' pretty-prints the bindings in `bs'.
ppBinds :: [Bind] -> Doc
ppBinds = vcat . punctuate semi . map pp

-- | `ppVars vs' pretty-prints a set of variables `vs'.
ppVars :: [Var] -> Doc
ppVars = braces . hcat . punctuate comma . map text

ppAtoms :: [Atom] -> Doc
ppAtoms = braces . hcat . punctuate comma . map pp

instance PP Prog where
    pp (MkProg bs) = vcat (map pp bs)

instance PP Bind where
    pp (MkBind v lf pos) = pp pos <> text v <+> equals <+> pp lf

instance PP LambdaForm where
    pp (MkLambdaForm fvs uf vs expr) =
        ppVars fvs <+> pp uf <+> ppVars vs <+> arrow <+> pp expr

instance PP UpdateFlag where
    pp U = text "\\u"
    pp N = text "\\n"

instance PP Expr where
    pp (LetE bs expr pos) =
        pp pos <> text "let" $$ ppBinds bs <+> text "in" <+> pp expr
    pp (LetRecE bs expr pos) =
        pp pos <> text "letrec" $$ ppBinds bs <+> text "in" <+> pp expr
    pp (CaseE expr alts pos) =
        pp pos <> text "case" <+> pp expr <+> text "of" $$ pp alts
    pp (AppE var args pos) =
        pp pos <> text var <+> ppAtoms args
    pp (OpE op args pos) =
        pp pos <> pp op <+> ppAtoms args
    pp (LitE val pos) =
        pp pos <> pp val

instance PP Alts where
    pp (AlgAlts aalts def)  = vcat (map pp aalts) $$ pp def
    pp (PrimAlts palts def) = vcat (map pp palts) $$ pp def

instance PP AlgAlt where
    pp (AAlt ctr vs expr pos) =
        pp pos <> text ctr <+> hsep (map text vs) <+> arrow <+> pp expr <> semi

instance PP PrimAlt where
    pp (PAlt lit expr pos) = pp pos <> pp lit <+> arrow <+> pp expr <> semi

instance PP DefaultAlt where
    pp (DefaultVar var expr pos) =
        pp pos <> text var <+> arrow <+> pp expr
    pp (Default expr pos)        =
        pp pos <> text "default" <+> arrow <+> pp expr

instance PP Atom where
    pp (VarAtom var pos) = pp pos <> text var
    pp (LitAtom lit pos) = pp pos <> pp lit
