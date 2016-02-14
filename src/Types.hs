--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module Types where

--------------------------------------------------------------------------------

import qualified Data.Map as M
import qualified Data.Set as S

import Pretty
import Var

--------------------------------------------------------------------------------

-- | Kinds.
data Kind
    = StarK
    | ArrK Kind Kind

--------------------------------------------------------------------------------

-- | Types.
data Type
    = PrimIntTy
    | AlgTy String
    | VarTy (AVar ())
    | ArrTy Type Type
    | AppTy Type Type

instance Eq Type where
    PrimIntTy == PrimIntTy     = True
    (AlgTy c) == (AlgTy k)     = c == k
    (VarTy x) == (VarTy y)     = varName x == varName y
    (ArrTy f x) == (ArrTy g y) = f == g && x == y
    (AppTy f x) == (AppTy g y) = f == g && x == y
    _           == _           = False

infixr 9 `arr`
arr :: Type -> Type -> Type
arr = ArrTy

-- | Polymorphic types.
data PolyType
    = QuantTy String PolyType
    | MonoTy { unPoly :: Type }

--------------------------------------------------------------------------------

class HasTypeVars a where
    tyVars :: a -> S.Set String

instance HasTypeVars Type where
    tyVars (VarTy v)   = S.singleton (varName v)
    tyVars (ArrTy f a) = tyVars f `S.union` tyVars a
    tyVars (AppTy f a) = tyVars f `S.union` tyVars a
    tyVars _           = S.empty

instance HasTypeVars PolyType where
    tyVars (MonoTy ty)    = tyVars ty
    tyVars (QuantTy v pt) = S.delete v (tyVars pt)

--------------------------------------------------------------------------------

class CanSubst a where
    subst :: a -> (Type -> Type) -> a

instance CanSubst Type where
    subst (AlgTy c)   s = AlgTy c
    subst (VarTy v)   s = s (VarTy v)
    subst (ArrTy f a) s = ArrTy (subst f s) (subst a s)
    subst (AppTy f a) s = AppTy (subst f s) (subst a s)
    subst t           s = t

instance CanSubst PolyType where
    subst (MonoTy t)    s = MonoTy $ subst t s
    subst (QuantTy v t) s = QuantTy v $ subst t s

type Theta = M.Map String Type

apply :: Theta -> Type -> Type
apply s (VarTy v) = case M.lookup (varName v) s of
    Nothing  -> VarTy v
    (Just t) -> apply s t
apply s (ArrTy f a) = ArrTy (apply s f) (apply s a)
apply s (AppTy f a) = AppTy (apply s f) (apply s a)
apply s t = t

applyP :: Theta -> PolyType -> PolyType
applyP s (MonoTy mt)   = MonoTy $ apply s mt
applyP s (QuantTy v t) = QuantTy v $ applyP (M.delete v s) t

(<@>) :: Theta -> Theta -> Theta
s1 <@> s2 = M.map (apply s1) s2 `M.union` s1

--------------------------------------------------------------------------------

instance PP Type where
    pp PrimIntTy   = text "Int#"
    pp (AlgTy c)   = text c
    pp (VarTy v)   = pp v
    pp (ArrTy f a) = parens $ pp f <+> text " -> " <> pp a
    pp (AppTy f a) = parens $ pp f <+> pp a

instance Show Type where
    show = render . pp

instance PP PolyType where
    pp (MonoTy mt) = pp mt
    pp (QuantTy v pt) = text "forall" <+> text v <> char '.' <> pp pt

instance Show PolyType where
    show = render . pp

--------------------------------------------------------------------------------
