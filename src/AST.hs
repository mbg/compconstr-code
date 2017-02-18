--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module AST where

--------------------------------------------------------------------------------

import Posn
import Var
import Pretty
import Prim
import Types

--------------------------------------------------------------------------------

-- | The type of constructors.
type Ctr = String

-- | Programs with position annotations, for backwards compatbility.
type Prog = AST Posn

-- | Programs.
data AST a = MkProg {
    progTyBinds :: [TyBind],
    progBinds   :: [ABind a]
}

-- | Type decls.
data TyBind = MkTyBind {
    tyBindName   :: Ctr,
    tyBindParams :: [Var],
    tyBindRHS    :: [AlgCtr],
    tyBindAnn    :: Posn
}

-- | Data constructors.
data AlgCtr = MkAlgCtr {
    algCtrName   :: Ctr,
    algCtrParams :: [Type],
    algCtrAnn    :: Posn
}

-- | Bindings with position annotations, for backwards compatbility.
type Bind = ABind Posn

-- | Bindings.
data ABind a = MkBind {
    bindName :: AVar a,
    bindLF   :: ALambdaForm a,
    bindAnn  :: a
} deriving Functor

-- | Lambda forms with position annotations, for backwards compatbility.
type LambdaForm = ALambdaForm Posn

-- | Lambda forms.
data ALambdaForm a = MkLambdaForm {
    lfFreeVars :: [AVar a],
    lfFlag     :: UpdateFlag,
    lfVars     :: [AVar a],
    lfExpr     :: AExpr a
} deriving Functor

-- | Update flags.
data UpdateFlag = U | N

-- | Expressions with position annotations, for backwards compatbility.
type Expr = AExpr Posn

-- | Expressions.
data AExpr a
    = LetE {
        letBinds :: [ABind a],
        letExpr  :: AExpr a,
        exprAnn  :: a
    }
    | LetRecE {
        letRecBinds :: [ABind a],
        letRecExpr  :: AExpr a,
        exprAnn  :: a
    }
    | CaseE {
        caseExpr :: AExpr a,
        caseAlts :: AAlts a,
        exprAnn  :: a
    }
    | AppE {
        appFun   :: AVar a,
        appAtoms :: [AAtom a],
        exprAnn  :: a
    }
    | CtrE {
        ctrName  :: Ctr,
        ctrAtoms :: [AAtom a],
        exprAnn  :: a
    }
    | OpE {
        opType  :: PrimOp,
        opAtoms :: [AAtom a],
        exprAnn  :: a
    }
    | LitE {
        litVal  :: PrimInt,
        exprAnn :: a
    } deriving Functor

-- | Case alternatives for backwards compatbility.
type Alts = AAlts Posn

-- | Case alternatives.
data AAlts a
    = AlgAlts [AAlgAlt a] (ADefaultAlt a)
    | PrimAlts [APrimAlt a] (ADefaultAlt a)
    deriving Functor

-- | Algebraic alternatives for backwards compatbility.
type AlgAlt = AAlgAlt Posn

-- | Algebraic alternatives.
data AAlgAlt a = AAlt {
    aaltCtr  :: Ctr,
    aaltVars :: [AVar a],
    aaltExpr :: AExpr a,
    aaltAnn  :: a
} deriving Functor

-- | Primitive alternatives for backwards compatbility.
type PrimAlt = APrimAlt Posn

-- | Primitive alternatives.
data APrimAlt a = PAlt {
    paltVal  :: PrimInt,
    paltExpr :: AExpr a,
    paltAnn  :: a
} deriving Functor

-- | Default alternatives for backwards compatbility.
type DefaultAlt = ADefaultAlt Posn

-- | Default alternatives.
data ADefaultAlt a
    = DefaultVar {
        defaultVar  :: AVar a,
        defaultExpr :: AExpr a,
        defaultAnn  :: a
    }
    | Default {
        defaultExpr :: AExpr a,
        defaultAnn  :: a
    } deriving Functor

-- | Atoms for backwards compatbility.
type Atom = AAtom Posn

-- | Atoms.
data AAtom a
    = VarAtom {
        atomVar :: AVar a,
        atomAnn :: a
    }
    | LitAtom {
        atomVal :: PrimInt,
        atomAnn :: a
    } deriving Functor

--------------------------------------------------------------------------------

-- | `ppBinds bs' pretty-prints the bindings in `bs'.
ppBinds :: PP a => [ABind a] -> Doc
ppBinds = vcat . punctuate semi . map pp

ppAtoms :: PP a => [AAtom a] -> Doc
ppAtoms = braces . hcat . punctuate comma . map pp

instance PP a => PP (AST a) where
    pp (MkProg ts bs) = vcat (map pp ts) $$ vcat (map pp bs)

instance PP TyBind where
    pp (MkTyBind ctr ps rhs ann) =
        pp ann <>
        text "type" <+>
        text ctr <+>
        ppVars ps <+>
        equals <+>
        sep (punctuate (char '|') (map pp rhs))

instance PP AlgCtr where
    pp (MkAlgCtr ctr ps ann) =
        pp ann <>
        text ctr <+>
        hsep (map pp ps)

instance PP a => PP (ABind a) where
    pp (MkBind v lf pos) = pp pos <> pp v <+> equals <+> pp lf

instance PP a => PP (ALambdaForm a) where
    pp (MkLambdaForm fvs uf vs expr) =
        ppVars fvs <+> pp uf <+> ppVars vs <+> arrow <+> pp expr

instance PP UpdateFlag where
    pp U = text "\\u"
    pp N = text "\\n"

instance PP a => PP (AExpr a) where
    pp (LetE bs expr pos) =
        pp pos <> text "let" $$ ppBinds bs <+> text "in" <+> pp expr
    pp (LetRecE bs expr pos) =
        pp pos <> text "letrec" $$ ppBinds bs <+> text "in" <+> pp expr
    pp (CaseE expr alts pos) =
        pp pos <> text "case" <+> pp expr <+> text "of" $$ pp alts
    pp (AppE var args pos) =
        pp pos <> pp var <+> ppAtoms args
    pp (CtrE ctr args pos) =
        pp pos <> text ctr <+> ppAtoms args
    pp (OpE op args pos) =
        pp pos <> pp op <+> ppAtoms args
    pp (LitE val pos) =
        pp pos <> pp val

instance PP a => PP (AAlts a) where
    pp (AlgAlts aalts def)  = vcat (map pp aalts) $$ pp def
    pp (PrimAlts palts def) = vcat (map pp palts) $$ pp def

instance PP a => PP (AAlgAlt a) where
    pp (AAlt ctr vs expr pos) =
        pp pos <> text ctr <+> hsep (map pp vs) <+> arrow <+> pp expr <> semi

instance PP a => PP (APrimAlt a) where
    pp (PAlt lit expr pos) = pp pos <> pp lit <+> arrow <+> pp expr <> semi

instance PP a => PP (ADefaultAlt a) where
    pp (DefaultVar var expr pos) =
        pp pos <> pp var <+> arrow <+> pp expr
    pp (Default expr pos)        =
        pp pos <> text "default" <+> arrow <+> pp expr

instance PP a => PP (AAtom a) where
    pp (VarAtom var pos) = pp pos <> pp var
    pp (LitAtom lit pos) = pp pos <> pp lit
