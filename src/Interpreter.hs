--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module Interpreter where

--------------------------------------------------------------------------------

import Control.Monad

import qualified Data.Map as M

import Posn
import Var
import Pretty
import Prim
import AST

--------------------------------------------------------------------------------

-- | The type of memory addresses.
newtype Addr = Addr Int deriving (Eq, Ord)

-- | The global environment maps the names of global bindings to memory
--   addresses.
type GlEnv = M.Map String Addr

-- | STG values.
data Value
    = AddrV Addr
    | IntV PrimInt

-- | Local environments map local binding names to values.
type LocEnv = M.Map String Value

-- | STG closures.
data Closure
    = Closure LambdaForm [Value]

-- | The heap contains closures, identified by memory addresses.
type Heap = M.Map Addr Closure

-- |
data Code
      -- | Evaluate an expression in an environment and apply its value to the
      --   arguments on the argument stack.
    = Eval Expr LocEnv
      -- | Apply the closure at the specified address to the arguments on the
      --   argument stack.
    | Enter Addr
      -- | Return a constructor applied to some values to the continuation on
      --   the return stack.
    | ReturnCon String [Value]
      -- | Return a primitive integer to the continuation on the return stack.
    | ReturnInt PrimInt

-- | The types of the three STG stacks.
type ArgStack = [Value]
type RetStack = [(Alts, LocEnv)]
type UpdStack = [(ArgStack, RetStack, Addr)]

-- | Configurations of the STG machine.
type Config = (Code, ArgStack, RetStack, UpdStack, Heap, GlEnv)

--------------------------------------------------------------------------------

-- | `val p sig a' returns the value of the atom `a'. If it is a literal,
--   then its value is returned. If it is variable, then it is looked up
--   in the local environment `p' and then the global environment `sig'.
val :: LocEnv -> GlEnv -> Atom -> Maybe Value
val p sig (LitAtom k _) = return $ IntV k
val p sig (VarAtom v _) = case M.lookup (varName v) p of
    (Just r) -> return r
    Nothing  -> case M.lookup (varName v) sig of
        (Just r) -> return $ AddrV r
        Nothing  -> mzero

--------------------------------------------------------------------------------

-- | `initialHeap n bs' constructs the initial heap from a list of bindings
--   `bs', starting at address `n'.
initialHeap :: Int -> [Bind] -> Heap
initialHeap n []                   = M.empty
initialHeap n (MkBind g lf _ : bs) =
    M.insert (Addr n) (Closure lf []) (initialHeap (n+1) bs)

-- | `initialEnv n bs' constructs the initial global environment from a list
--   of bindings `bs', starting at address `n'.
initialEnv :: Int -> [Bind] -> GlEnv
initialEnv n [] = M.empty
initialEnv n (MkBind g lf _ : bs) =
    M.insert (varName g) (Addr n) (initialEnv (n+1) bs)

-- | `initialState p main' constructs the initial state of the STG machine for
--   a program `p' where `main' is the name of the entry point.
initialState :: Prog -> String -> Config
initialState (MkProg _ bs) main = (Eval expr M.empty, [], [], [], h, env)
    where
        expr = (AppE (Var main NoPosn) [] NoPosn)
        h    = initialHeap 0 bs
        env  = initialEnv 0 bs

--------------------------------------------------------------------------------

-- | `allocAddr p n bs' extends the local environment `p' with address
--   mappings for the bindings in `bs', starting with address `n'.
allocAddr :: LocEnv -> Int -> [Bind] -> LocEnv
allocAddr p _ []                  = p
allocAddr p n (MkBind x _ _ : bs) =
    allocAddr (M.insert (varName x) (AddrV $ Addr n) p) (n+1) bs

allocClosures' :: LocEnv -> Heap -> Int -> [Bind] -> Maybe Heap
allocClosures' p_rhs h _ [] = return h
allocClosures' p_rhs h n (MkBind _ lf@(MkLambdaForm fvs uf vs e) _ : bs) = do
    vs' <- mapM (flip M.lookup p_rhs . varName) vs
    allocClosures' p_rhs (M.insert (Addr n) (Closure lf vs') h) (n+1) bs

-- | `allocClosures p h bs' adds closures for all bindings in `bs' to the
--   heap `h' where address mappings are added to `p'.
allocClosures :: LocEnv -> Heap -> [Bind] -> Maybe (LocEnv, Heap)
allocClosures p h bs = do
    let
        n  = M.size h
        p' = allocAddr p n bs
    h' <- allocClosures' p h n bs
    return (p', h')

allocClosuresRec :: LocEnv -> Heap -> [Bind] -> Maybe (LocEnv, Heap)
allocClosuresRec p h bs = do
    let
        n  = M.size h
        p' = allocAddr p n bs
    h' <- allocClosures' p' h n bs
    return (p', h')

--------------------------------------------------------------------------------

patternMatchCtr :: String -> [AlgAlt] -> Maybe AlgAlt
patternMatchCtr c [] = Nothing
patternMatchCtr c (alt@(AAlt ctr _ _ _) : alts)
    | c == ctr  = Just alt
    | otherwise = patternMatchCtr c alts

patternMatchPrim :: PrimInt -> [PrimAlt] -> Maybe PrimAlt
patternMatchPrim k [] = Nothing
patternMatchPrim k (alt@(PAlt v _ _) : alts)
    | k == v    = Just alt
    | otherwise = patternMatchPrim k alts

--------------------------------------------------------------------------------

-- | `step c` performs a single transition from a configuration `c`
step :: Config -> Maybe Config
step (Eval (AppE f xs pos) p, as, rs, us, h, env) = do
    -- get the value associated with `f'
    v <- val p env (VarAtom f pos)

    case v of
        -- Rule 1 (Application)
        AddrV a -> do
            -- get the values associated with the arguments `xs'
            as' <- mapM (val p env) xs
            -- enter the closure at `a'
            return (Enter a, as' ++ as, rs, us, h, env)
        -- Rule 10 (Variable bound to integer)
        IntV k -> do
            return (ReturnInt k, as, rs, us, h, env)
-- Rules 2 and 15 (Enter Non-Updatable/Updatable Closure)
step (Enter a, as, rs, us, h, env) = do
    -- find the closure at address `a' on the heap
    (Closure (MkLambdaForm vs u xs e) ws_f) <- M.lookup a h

    -- determine whether the closure is updatable or not
    case u of
        N -> do
            -- ensure that there are at least as many items on the argument stack
            -- as are expected by the closure
            -- Rule 2 (Enter Non-Updatable Closure)
            if length as >= length xs then do
                let
                    -- remove the items from the argument stack
                    as'  = drop (length xs) as

                    -- take the items from the argument stack
                    ws_a = take (length xs) as

                    -- create a local environment for the closure
                    p    = M.fromList $
                        zip (map varName vs) ws_f ++ zip (map varName xs) ws_a

                -- evaluate the body `e' in a local environment `p'
                return (Eval e p, as', rs, us, h, env)
            -- Rule 17 (NotEnoughArguments Update)
            else do
                -- the return stack should be empty
                guard (null rs)

                case us of
                    []                        -> mzero
                    ((as_u, rs_u, a_u) : us') -> do
                        let
                            xs1 = take (length as) xs
                            xs2 = drop (length as) xs
                            lf  = MkLambdaForm (vs ++ xs1) N xs2 e
                            h_u = M.insert a_u (Closure lf (ws_f ++ as)) h

                        return (Enter a, as ++ as_u, rs_u, us, h_u, env)
        U -> do
            let
                p = M.fromList $ zip (map varName vs) ws_f

            -- evaluate the body `e' in a local environment `p'
            return (Eval e p, [], [], (as,rs,a) : us, h, env)
-- Rule 3 (Let(Rec) expressions)
step (Eval (LetE bs e _) p, as, rs, us, h, env) = do
    (p',h') <- allocClosures p h bs
    return (Eval e p', as, rs, us, h', env)
step (Eval (LetRecE bs e _) p, as, rs, us, h, env) = do
    (p',h') <- allocClosuresRec p h bs
    return (Eval e p', as, rs, us, h', env)
-- Rule 4 (Case expressions)
step (Eval (CaseE e alts _) p, as, rs, us, h, env) = do
    return (Eval e p, as, (alts,p) : rs, us, h, env)
-- Rule 5 (Constructors)
step (Eval (CtrE c xs _) p, as, rs, us, h, env) = do
    args <- mapM (val p env) xs
    return (ReturnCon c args, as, rs, us, h, env)
-- Rule 6 (ReturnCon Case Match)
step (ReturnCon c ws, as, (AlgAlts cs d, p) : rs, us, h, env) = do
    case patternMatchCtr c cs of
        -- Rule 6 (ReturnCon Case Match)
        (Just (AAlt _ vs e _)) -> do
            let
                p' = p `M.union` M.fromList (zip (map varName vs) ws)
            return (Eval e p', as, rs, us, h, env)
        Nothing                -> case d of
            -- Rule 7 (ReturnCon Case Default)
            (Default e _)      ->
                return (Eval e p, as, rs, us, h, env)
            -- Rule 8 (ReturnCon Case DefaultVar)
            (DefaultVar v e _) -> do
                let
                    n  = M.size h
                    p' = M.insert (varName v) (AddrV $ Addr n) p
                    vs = [Var ('v' : show i) NoPosn | i <- [1..length ws]]
                    zs = map (flip VarAtom NoPosn) vs
                    lf = MkLambdaForm vs N [] (CtrE c zs NoPosn)
                    h' = M.insert (Addr n) (Closure lf ws) h
                return (Eval e p', as, rs, us, h', env)
-- Rule 9 (Literals)
step (Eval (LitE k _) p, as, rs, us, h, env) = do
    return (ReturnInt k, as, rs, us, h, env)
-- Rules 11,12,13 (ReturnInt)
step (ReturnInt k, as, (PrimAlts cs d, p) : rs, us, h, env) = do
    case patternMatchPrim k cs of
        -- Rule 11 (ReturnInt Case Match)
        (Just (PAlt _ e _)) -> do
            return (Eval e p, as, rs, us, h, env)
        Nothing -> case d of
            -- Rule 12 (ReturnInt Case DefaultVar)
            (DefaultVar v e _) -> do
                let
                    p' = M.insert (varName v) (IntV k) p
                return (Eval e p', as, rs, us, h, env)
            -- Rule 13 (ReturnInt Case Default)
            (Default e _)      -> do
                return (Eval e p, as, rs, us, h, env)
-- Rule 14 (Built-in operations)
step (Eval (OpE op [x1, x2] _) p, as, rs, us, h, env) = do
    i1 <- val p M.empty x1
    i2 <- val p M.empty x2
    r  <- case (i1,i2) of
        (IntV x, IntV y) -> case op of
            PrimAdd -> return (x+y)
            PrimSub -> return (x-y)
            PrimMul -> return (x*y)
            PrimDiv -> return (x `div` y)
        _ -> mzero
    return (ReturnInt r, as, rs, us, h, env)
-- Rule 16 (EmptyReturnStack Update)
step (ReturnCon c ws, [], [], (as, rs, a) : us, h, env) = do
    let
        vs = [Var ('v' : show i) NoPosn | i <- [1..length ws]]
        zs = map (flip VarAtom NoPosn) vs
        lf = MkLambdaForm vs N [] (CtrE c zs NoPosn)
        h' = M.insert a (Closure lf ws) h
    return (ReturnCon c ws, as, rs, us, h', env)
-- Otherwise, we are stuck:
step _ = Nothing

--------------------------------------------------------------------------------

instance PP Addr where
    pp (Addr a) = text "Addr" <+> int a

instance PP Value where
    pp (AddrV a) = pp a
    pp (IntV k)  = pp k

instance PP Closure where
    pp (Closure lf vs) = parens (pp lf) <+> pp vs

instance PP Code where
    pp (Eval expr env)  = text "Eval" <+> parens (pp expr) <+> ppEnv text env
    pp (Enter a)        = text "Enter" <+> pp a
    pp (ReturnCon c xs) = text "ReturnCon" <+> text c <+> pp xs
    pp (ReturnInt k)    = text "ReturnInt" <+> pp k

ppEnv :: PP b => (a -> Doc) -> M.Map a b -> Doc
ppEnv ppK env = braces $ vcat $ punctuate comma $
    map (\(k,v) -> ppK k <+> text "->" <+> pp v) $ M.toList env

ppRS :: RetStack -> Doc
ppRS = brackets . sep . map (\(alts, p) ->
    parens $ pp alts <> comma <> ppEnv text p)

ppUS :: UpdStack -> Doc
ppUS = brackets . sep . map (\(as, rs, a) ->
    parens $ pp as <> comma <> ppRS rs <> comma <> pp a)

ppConfig :: Config -> Doc
ppConfig (c, as, rs, us, h, env) = parens $ sep $ punctuate comma
    [pp c, pp as, ppRS rs, ppUS us, ppEnv pp h, ppEnv text env]
