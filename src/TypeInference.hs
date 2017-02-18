--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module TypeInference where

--------------------------------------------------------------------------------

import Control.Applicative
import Control.Monad

import qualified Data.DList as DL
import qualified Data.Graph as G
import qualified Data.Map as M
import qualified Data.Set as S

import Pretty

import Posn
import Var
import AST
import Types
import FreeVars
import TypeError

--------------------------------------------------------------------------------

-- | Bindings in the typing context.
data CtxBinding
    = VarBinding { ctxType :: PolyType }
    | CtrBinding { ctxType :: PolyType }

-- | Type environments.
type Context = M.Map String CtxBinding

ppContext :: Context -> Doc
ppContext = text . show . M.toList . M.map ctxType

-- | The state of the type inference algorithm.
data TyInfSt = TyInfSt {
    tyInfStCounter :: Int,
    tyInfStLogger  :: DL.DList Doc
}

defaultState :: TyInfSt
defaultState = TyInfSt 0 DL.empty

-- | Type inference monad.
newtype HM a = HM {
    runHM :: Context -> TyInfSt -> Either TyError (a, TyInfSt)
}

instance Functor HM where
    f `fmap` (HM m) = HM $ \ctx s -> do
        (r,s') <- m ctx s
        return (f r, s')

instance Applicative HM where
    pure x = HM $ \_ s -> return (x, s)

    (HM m) <*> (HM n) = HM $ \ctx s -> do
        (f, s')  <- m ctx s
        (x, s'') <- n ctx s'
        return (f x, s'')

instance Monad HM where
    return = pure

    (HM m) >>= f = HM $ \ctx s -> do
        (r, s') <- m ctx s
        let (HM m') = f r in m' ctx s'

--------------------------------------------------------------------------------

-- | `initialContext ctx ts' adds the types of all constructors defined by
--   the type bindings in `ts' to a context, which is initially `ctx'
initialContext :: Context -> [TyBind] -> Either TyError Context
initialContext ctx [] = return ctx
initialContext ctx (MkTyBind ty ps rhs pos : bs) =
    do
        ctx' <- addCtrs ctx rhs
        initialContext ctx' bs
    where
        -- the return type of all constructors for this type
        rt = foldl AppTy (AlgTy ty) [VarTy (Var p ()) | (Var p _) <- ps]

        -- a computation which adds all constructors for this type to
        -- the context, or returns an error if there is a duplicate constructor
        addCtrs :: Context -> [AlgCtr] -> Either TyError Context
        addCtrs ctx [] = return ctx
        addCtrs ctx (MkAlgCtr c ts p : cs)
            | M.member c ctx = Left $ MkTyError p (DuplicateCtrErr c)
            | otherwise      = addCtrs (M.insert c t ctx) cs
                where
                    -- the type of this constructor
                    t = CtrBinding $ gen' M.empty $ foldr arr rt ts

-- |
bindingGroups :: [ABind a ] -> [[ABind a]]
bindingGroups =
    map G.flattenSCC .
    G.stronglyConnComp .
    map (\b -> (b, varName $ bindName b, M.keys $ freeVars S.empty b))

--------------------------------------------------------------------------------

mapI :: (a -> HM (Theta, b)) -> [a] -> HM (Theta, [b])
mapI f []     = return (M.empty, [])
mapI f (x:xs) = do
    (s,r)   <- f x
    (s',rs) <- withContext (applyCtx s) (mapI f xs)
    return (s' <@> s, r:rs)

-- | `typeError posn err' raises a type error `err' at position `posn'.
typeError :: Posn -> TyErrorMsg -> HM a
typeError p err = HM $ \_ _ -> Left $ MkTyError p err

-- | `lookupType x' looks up the type for a variable `x'.
lookupType :: String -> HM (Maybe CtxBinding)
lookupType x = HM $ \ctx s -> return (M.lookup x ctx, s)

-- | `diagnostic d' adds `d' to the log.
diagnostic :: Doc -> HM ()
diagnostic doc = HM $ \ctx s@(TyInfSt { tyInfStLogger = l }) ->
    return ((), s { tyInfStLogger = DL.snoc l doc })

context :: HM Context
context = HM $ curry return

withContext :: (Context -> Context) -> HM a -> HM a
withContext f (HM m) = HM $ \ctx s -> m (f ctx) s

applyCtx :: Theta -> Context -> Context
applyCtx s = M.map $ \b -> case b of
    VarBinding pt -> VarBinding (subst pt (apply s))
    CtrBinding pt -> CtrBinding pt

fresh :: HM Type
fresh = HM $ \ctx s@(TyInfSt { tyInfStCounter = c }) ->
    return (VarTy $ Var ('$' : show c) (), s { tyInfStCounter = c + 1 })

gen :: Type -> HM PolyType
gen t = HM $ \ctx s -> return (gen' ctx t, s)

gen' :: Context -> Type -> PolyType
gen' ctx t = S.foldr QuantTy (MonoTy t) vs
    where
        cs = S.unions $ map (tyVars . ctxType) $ M.elems ctx
        vs = tyVars t `S.difference` cs

-- | `inst pt' instantiates the universally-quantified type variables in `pt'
--   with fresh type variables and returns the resulting monomorphic type.
inst :: PolyType -> HM Type
inst (MonoTy t)     = return t
inst (QuantTy v pt) = do
    i <- fresh

    -- a substitution function (note that the traversal applies s to
    -- leaf nodes first, so we don't need to worry about `ts')
    let
        s (VarTy x) | x == Var v () = i
        s mt                        = mt

    -- continue with the type in which v has been substituted
    inst (subst pt s)

bind :: Posn -> AVar () -> Type -> HM Theta
bind p tv t
    | t == VarTy tv                  = return M.empty
    | varName tv `S.member` tyVars t = typeError p (OccursErr tv t)
    | otherwise                      = do
        diagnostic $
            text "I am binding a type to a type variable:" $$
            nest 4 (pp tv <+> text ":=" <+> pp t)

        return $ M.singleton (varName tv) t

-- | `unify pos t u' attempts to unify two types `t' and `u'.
unify :: Posn -> Type -> Type -> HM Theta
unify p t0 t1 =
    do
        diagnostic $ text "[Unify] Unifying:" $$ nest 4 (pp t0 $$ pp t1)
        go t0 t1
    where
        go PrimIntTy PrimIntTy = return M.empty
        go (AlgTy x) (AlgTy y)
            | x == y = return M.empty
        go (VarTy x) t = bind p x t
        go t (VarTy x) = bind p x t
        go (ArrTy f x) (ArrTy g y) = do
            s0 <- go f g
            s1 <- go (apply s0 x) (apply s0 y)
            return (s1 <@> s0)
        go (AppTy f x) (AppTy g y) = do
            s0 <- go f g
            s1 <- go (apply s0 x) (apply s0 y)
            return (s1 <@> s0)
        go t0 t1 = typeError p (UnifyErr t0 t1)

--------------------------------------------------------------------------------

type TyProg = AST PolyType

-- | `inferTypes p' infers the types of `p'.
inferTypes :: Prog -> Either TyError (TyProg, [Doc])
inferTypes (MkProg ts bs) =
    do
        ctx         <- initialContext M.empty ts
        ((_,rs),st) <- runHM (inferBindingGroups gs) ctx defaultState
        return (MkProg ts rs, DL.toList $ tyInfStLogger st)
    where
        gs = bindingGroups bs

-- |
inferBindingGroups :: [[Bind]] -> HM (Theta,[ABind PolyType])
inferBindingGroups [] = return (M.empty,[])
inferBindingGroups (bg:bgs) = do
    diagnostic $
        text "I am inferring the types of a binding group:" $$ nest 4 (pp bg)

    -- generate fresh type variable identifiers for all bindings in the
    -- current binding group
    vs <- mapM (const fresh) bg

    -- generate a type environment in which each of the current bindings
    -- is given one of the fresh type variable identifiers as its type
    let
        pt v = VarBinding $ MonoTy v
        ctx  = M.fromList [(varName $ bindName n, pt v) | (n,v) <- zip bg vs]

    -- report changes to the context
    diagnostic $
        text "[BindingGroup] Extending the context with:" $$
        nest 4 (ppContext ctx)

    -- infer the types of the bindings in this group
    (s,bg') <- withContext (M.union ctx) $ mapI inferBindingTy bg

    diagnostic $
        text "I have inferred the types of a binding group:" $$ nest 4 (pp bg')

    -- now that we have the types of the bindings in the current group,
    -- we want to recursively infer those of the following binding groups;
    -- for which we need an environment containing this binding group's
    -- types
    pts <- mapM (gen . unPoly . bindAnn) bg'

    let
        ptb  = zip bg' pts
        qbg  = [MkBind n lf pt | (MkBind n lf _, pt) <- ptb]
        ctx' = M.fromList $
            zip (map (varName . bindName) bg') (map VarBinding pts)

    -- recursively infer the types of bindings in binding groups
    (s',bgs') <- withContext (M.union ctx') (inferBindingGroups bgs)

    -- return the combined bindings
    return (s' <@> s, qbg ++ bgs')

{-inferRecBindingTy :: (Bind, Type) -> HM (Theta, ABind Type)
inferRecBindingTy (MkBind (Var v _) lf p, bt) = do
    (s1, lf', t) <- inferLambdaTy lf

    s2 <- withContext (applyCtx s1) $ unify p (apply s1 bt) (apply s1 t)

    let
        s3 = s2 <@> s1

    return (s3, MkBind (Var v t) (fmap (apply s3) lf') t)-}

inferBindingTy :: Bind -> HM (Theta, ABind PolyType)
inferBindingTy (MkBind (Var v _) lf p) = do
    (s1, lf', t) <- inferLambdaTy lf
    return (s1, MkBind (Var v t) (fmap (applyP s1) lf') t)

-- | `inferLambdaTy lf' infers the type of a lambda form `lf'
inferLambdaTy :: LambdaForm -> HM (Theta, ALambdaForm PolyType, PolyType)
inferLambdaTy (MkLambdaForm fvs uf vs e) = do
    -- generate fresh type variables for all parameters of this function
    ts <- mapM (\_ -> MonoTy <$> fresh) vs

    -- create a typing environment with typings for the parameters (mapped to
    -- the fresh type vars)
    let
        ns  = map varName vs
        ctx = M.fromList (zip ns (map VarBinding ts))

    -- report changes to the context
    diagnostic $
        text "[Lambda] Extending the context with:" $$
        nest 4 (ppContext ctx)

    -- infer the type of the expression in the new context
    (s, e') <- withContext (M.union ctx) (inferExprTy e)

    -- construct the type of this lambda abstraction by adding arrows
    -- between all the parameter types and the return type
    let
        rt = MonoTy $ foldr (arr . apply s . unPoly) (unPoly $ exprAnn e') ts

    -- report the inferred type
    diagnostic $
        text "[Lambda] I have inferred the type of a lambda form as:" $$
        nest 4 (pp rt)

    mbs <- mapM (lookupType . varName) fvs

    -- annotate the parameters with their respective types
    let
        vs'  = [Var v (applyP s t) | (Var v _, t) <- zip vs ts]
        fvs' = [Var v t | (Var v _, Just (VarBinding t)) <- zip fvs mbs]

    -- return the typed lambda form
    return (s, MkLambdaForm fvs' uf vs' e', rt)

-- | `inferExprTy e' infers the type of an expression `e'
inferExprTy :: Expr -> HM (Theta, AExpr PolyType)
inferExprTy (LetE bs e p) = do
    -- report what we are doing
    diagnostic $
        text "[LetE] I am inferring the types of the following bindings:" $$
        nest 4 (pp bs)

    -- infer the types of the bindings ...
    (s, bs') <- mapI inferBindingTy bs

    pts <- withContext (applyCtx s) $ mapM (gen . unPoly . bindAnn) bs'

    let
        ts  = zip (map (varName . bindName) bs') (map VarBinding pts)
        pbs = [MkBind n lf pt | (MkBind n lf _, pt) <- zip bs' pts]
        ctx = M.union (M.fromList ts)


    c <- context
    diagnostic $
        text "I have inferred the types of bindings in a let expression:" $$
        nest 4 (text $ show [(n,ctxType t) | (n,t) <- ts ]) $$
        text "in context" $$
        nest 4 (text $ show [(n,ctxType t) | (n,t) <- M.toList c ])

    -- infer the type of the expression
    (s', e') <- withContext (applyCtx s . ctx) (inferExprTy e)

    let
        s'' = s' <@> s
        rt  = applyP s'' $ exprAnn e'

    diagnostic $
        text "I have inferred the type of a let expression as:" $$
        nest 4 (pp rt)


    return (s'', LetE pbs e' rt)
inferExprTy (LetRecE bs e p) = do
    -- report what we are doing
    diagnostic $
        text "I am inferring the types of bindings in a letrec expression:" $$
        nest 4 (pp bs)

    -- infer the types of the bindings as a single binding group
    (s,pbs) <- inferBindingGroups [bs]

    let
        ts  = zip (map (varName . bindName) pbs) (map (VarBinding . bindAnn) pbs)
        ctx = M.union (M.fromList ts)

    -- infer the type of the expression
    (s', e') <- withContext (applyCtx s . ctx) (inferExprTy e)

    let
        s'' = s' <@> s
        rt  = applyP s'' $ exprAnn e'

    diagnostic $
        text "I have inferred the type of a letrec expression as:" $$
        nest 4 (pp rt)

    return (s'', LetRecE pbs e' rt)
inferExprTy (CaseE e alts p) = do
    -- infer the type of the expression that is examined by this
    -- case expression
    (s0,e') <- inferExprTy e

    -- report our findings
    diagnostic $
        text "[CaseE] I have inferred the type of the expression as:" $$
        nest 4 (pp e <+> text "::" <+> pp (applyP s0 $ exprAnn e'))

    -- infer the types of the alternatives
    (s1,alts',rt) <- withContext (applyCtx s0) $
        inferAlts p (unPoly $ exprAnn e') alts

    -- combine the substitutions
    let
        s2 = s1 <@> s0

    diagnostic $
        text "I have inferred the type of a case expression as:" $$
        nest 4 (pp $ applyP s2 rt)

    return (s2, CaseE e' alts' rt)
inferExprTy (AppE (Var f _) as p) = do
    -- try to look up the type of the variable
    r <- lookupType f

    case r of
        Just (VarBinding pt) -> do
            vt <- inst pt

            diagnostic $
                text "[AppE] I found a variable that I was looking for:" $$
                nest 4 (text f <+> text "::" <+> pp pt) $$
                text "and instantiated it as:" $$
                nest 4 (text f <+> text "::" <+> pp vt)

            -- infer the types of the arguments
            as' <- mapM inferAtomTy as

            rt <- fresh

            let
                -- calculate the expected type of the constructor
                et = foldr arr rt $ map (unPoly . atomAnn) as'

            -- unify the actual and expected types
            s <- unify p et vt

            -- TODO: check that the function is fully applied

            -- return the typed constructor
            return (s, AppE (Var f (MonoTy vt)) as' (applyP s (MonoTy rt)))
        _                      -> typeError p (NotInScopeErr f)
inferExprTy (CtrE c as p) = do
    -- try to look up the type of the constructor
    r  <- lookupType c

    -- check if the constructor is in scope
    case r of
        (Just (CtrBinding pt)) -> do
            ct <- inst pt

            diagnostic $
                text "[CtrE] I found a constructor that I was looking for:" $$
                nest 4 (text c <+> text "::" <+> pp pt) $$
                text "and instantiated it as:" $$
                nest 4 (text c <+> text "::" <+> pp ct)

            -- infer the types of the arguments
            as' <- mapM inferAtomTy as

            rt <- fresh

            let
                -- calculate the expected type of the constructor
                et = foldr arr rt $ map (unPoly . atomAnn) as'

            -- unify the actual and expected types
            s <- unify p ct et

            -- TODO: check that the constructor is fully applied

            -- return the typed constructor
            diagnostic $
                text "[CtrE] Result:" $$
                nest 4 (text c <+> text "::" <+> pp (apply s rt))

            return (s, CtrE c as' (applyP s (MonoTy rt)))

        _                      -> typeError p (NotInScopeErr c)
inferExprTy (OpE o as p) =
    do
        v <- fresh
        as' <- mapM inferAtomTy as

        let
            at = foldr (arr . unPoly . atomAnn) v as'

        s <- unify p t at

        return (s, OpE o as' (MonoTy $ apply s v))
    where
        t = PrimIntTy `arr` PrimIntTy `arr` PrimIntTy
inferExprTy (LitE l _)  = return (M.empty, LitE l $ MonoTy PrimIntTy)

inferAlts :: Posn -> Type -> Alts -> HM (Theta, AAlts PolyType, PolyType)
inferAlts p et (AlgAlts alts def) = do
    rt <- fresh

    (s, alts', def') <- inferAlgAlts et rt def alts

    return (s, AlgAlts alts' def', MonoTy $ apply s rt)
inferAlts p et (PrimAlts alts def) = do
    rt <- fresh

    (s, alts') <- mapI (inferPrimAlt et rt) alts
    (s', def') <- withContext (applyCtx s) $
        inferDefaultAlt (apply s et) (apply s rt) def

    let
        s'' = s' <@> s

    return (s'', PrimAlts alts' def', MonoTy $ apply s'' rt)

inferAlgAlts ::
    Type ->
    Type ->
    DefaultAlt ->
    [AlgAlt] ->
    HM (Theta, [AAlgAlt PolyType], ADefaultAlt PolyType)
inferAlgAlts et rt def []     = do
    (s, def') <- inferDefaultAlt et rt def
    return (s, [], def')
inferAlgAlts et rt def (x:xs) = do
    (s,r)        <- inferAlgAlt et rt x
    (s',rs,def') <- withContext (applyCtx s) $
        inferAlgAlts (apply s et) (apply s rt) def xs

    return (s' <@> s, r:rs, def')

inferAlgAlt :: Type -> Type -> AlgAlt -> HM (Theta, AAlgAlt PolyType)
inferAlgAlt et rt (AAlt c vs e p) = do
    r <- lookupType c

    diagnostic $
        text "[AAlt] Entered:" $$
        nest 4 (text c $$ text "LHS:" <+> pp et $$ text "RHS:" <+> pp rt)

    case r of
        Just (CtrBinding pt) -> do
            -- instantiate the type of the constructor with fresh type vars
            ct <- inst pt

            -- report the findings
            diagnostic $
                text "[AAlt] I found a constructor that I was looking for:" $$
                nest 4 (text c <+> text "::" <+> pp pt) $$
                text "and instantiated it as:" $$
                nest 4 (text c <+> text "::" <+> pp ct)

            -- create new type variables for the pattern variables
            ts <- mapM (const fresh) vs

            -- create a new type variable for the whole pattern
            dt <- fresh

            let
                -- the expected type of the constructor
                bt = foldr arr dt ts

            unless (null vs) $ diagnostic $
                text "[AAlt] My pattern variables are:" $$
                nest 4 (text $ show $ zip (map varName vs) ts)

            -- unify the fresh type of the constructor (ct) with the expected
            -- type (bt)
            s <- unify p ct bt

            let
                -- a function to update the context with bindings
                -- for the pattern variables
                ts' = map (VarBinding . MonoTy . apply s) ts

                bs  = M.fromList (zip (map varName vs) ts')
                ctx = M.union bs

            -- report changes to the context
            unless (null vs) $ diagnostic $
                text "[AAlt] Extending the context with:" $$
                nest 4 (ppContext bs)

            -- infer the type of the expression with the pattern variables
            -- in scope
            (s',e') <- withContext (applyCtx s . ctx) (inferExprTy e)

            let
                s1 = s' <@> s

            -- unify things
            diagnostic $
                text "[AAlt] Unifying the type of the case expression with the type of the pattern:" $$
                nest 4 (pp (apply s1 et) $$ pp (apply s1 dt))

            s2 <- withContext (applyCtx s1) $ unify p (apply s1 et) (apply s1 dt)

            let
                s2' = s2 <@> s1

            s3 <- withContext (applyCtx s2') $ unify p (apply s2' rt) (apply s2' $ unPoly $ exprAnn e')

            let
                rs = s3 <@> s2 <@> s' <@> s
                at = apply rs et `arr` apply rs rt
                vs' = [Var n (MonoTy $ apply rs t) | (Var n _, t) <- zip vs ts]

            diagnostic $ text (show $ M.toList rs)

            diagnostic $ text "[AAlt] Case alternative" <> parens (text c) <> colon $$
                nest 4 (pp at)

            return (rs, AAlt c vs' e' (MonoTy at))
        _                    -> typeError p (NotInScopeErr c)

inferPrimAlt :: Type -> Type -> PrimAlt -> HM (Theta, APrimAlt PolyType)
inferPrimAlt et rt (PAlt k e p) = do
    -- infer the type of the expression
    (s, e') <- inferExprTy e

    s2 <- unify p et PrimIntTy
    s3 <- unify p rt (unPoly $ exprAnn e')

    let
        rs = s3 <@> s2 <@> s

    return (rs, PAlt k e' (MonoTy $ PrimIntTy `arr` unPoly (exprAnn e')))

-- | `inferDefaultAlt et rt d' infers the type of a default case alternative,
--   `d', where `et' is the type of the LHS and `rt' the type of the RHS.
inferDefaultAlt ::
    Type ->
    Type ->
    DefaultAlt ->
    HM (Theta, ADefaultAlt PolyType)
inferDefaultAlt et rt (Default e p) = do
    diagnostic $ text "[Default] Entered:" $$ nest 4 (pp et $$ pp rt)

    -- create a fresh type variable for the LHS
    t <- fresh

    -- infer the type of the expression
    (s0, e') <- inferExprTy e

    -- unify the return type of the case expression with the type of the
    -- expression in this case alternative
    s1 <- unify p et t
    s2 <- unify p rt (unPoly $ exprAnn e')

    let
        rs = s2 <@> s1 <@> s0
        at = MonoTy $ apply rs $ et `arr` rt

    diagnostic $ text "[Default] Default alternative:" $$
        nest 4 (pp at)

    return (rs, Default e' at)
inferDefaultAlt et rt (DefaultVar (Var v _) e p) = do
    diagnostic $ text "[DefaultVar] Entered:" $$
        nest 4 (text v $$ pp et $$ pp rt)

    -- generate a fresh type variable for the variable that is bound
    -- by this case alternative
    t <- fresh

    -- construct a function to update the context
    let
        ctx = M.insert v (VarBinding $ MonoTy t)

    -- infer the type of the expression
    (s, e') <- withContext ctx (inferExprTy e)

    s2 <- unify p et t
    s3 <- unify p rt (unPoly $ exprAnn e')

    -- construct the final substitution and type
    let
        rs = s3 <@> s2 <@> s
        at = MonoTy $ apply rs $ et `arr` rt

    diagnostic $
        text "[DefaultVar] Default alternative" <> parens (text v) <> colon $$
        nest 4 (pp at)

    -- return the substitution and the updated DefaultVar node
    return (rs, DefaultVar (Var v (MonoTy $ apply rs t)) e' at)

-- | `inferAtomTy a' infers the type of an atom `a'.
inferAtomTy :: Atom -> HM (AAtom PolyType)
inferAtomTy (LitAtom l _) = return (LitAtom l (MonoTy PrimIntTy))
inferAtomTy (VarAtom (Var v _) p) = do
    -- try to look up the variable in the type environment
    r <- lookupType v

    case r of
        -- if the variable is found in the context, then instantiate then
        -- polymorphic type and return it
        (Just (VarBinding t)) -> do
            mt <- MonoTy <$> inst t

            diagnostic $
                text "[VarAtom] Variable found:" $$
                nest 4 (text v <+> text "::" <+> pp mt)

            return (VarAtom (Var v mt) mt)
        -- if it is not found, then we raise an error
        _                     -> typeError p (NotInScopeErr v)

--------------------------------------------------------------------------------
