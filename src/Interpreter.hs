--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module Interpreter where

--------------------------------------------------------------------------------

import Control.Monad

import qualified Data.Map as M

import Posn
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
      -- | An address value (a pointer).
    = AddrV Addr
      -- | A primitive value (a non-pointer).
    | IntV PrimInt

-- | Local environments map local binding names to values.
type LocEnv = M.Map String Value

-- | STG closures consist of code (a lambda form) and free variables.
data Closure
    = Closure LambdaForm [Value]

-- | The heap contains closures, identified by memory addresses.
type Heap = M.Map Addr Closure

-- | Different types of instructions to evaluate.
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

-- | The types of the three STG stacks:
--   * The argument stack contains values.
--   * The return stack contains continuations.
--   * The update stack contains update frames, cosisting of a saved argument
--     stack, return stack, and the address of the closure that is to be
--     updated.
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
val p sig (VarAtom v _) = case M.lookup v p of
    (Just r) -> return r
    Nothing  -> case M.lookup v sig of
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
    M.insert g (Addr n) (initialEnv (n+1) bs)

-- | `initialState p main' constructs the initial state of the STG machine for
--   a program `p' where `main' is the name of the entry point.
initialState :: Prog -> String -> Config
initialState (MkProg bs) main = (Eval expr M.empty, [], [], [], h, env)
    where
        expr = (AppE main [] NoPosn)
        h    = initialHeap 0 bs
        env  = initialEnv 0 bs

--------------------------------------------------------------------------------

-- | `allocAddr p n bs' extends the local environment `p' with address
--   mappings for the bindings in `bs', starting with address `n'.
allocAddr :: LocEnv -> Int -> [Bind] -> LocEnv
allocAddr p _ []                  = p
allocAddr p n (MkBind x _ _ : bs) =
    allocAddr (M.insert x (AddrV $ Addr n) p) (n+1) bs

-- | `allocClosures p h n bs' extends the heap `h' with closures for the
--   bindings in `bs', starting with address `n'.
allocClosures' :: LocEnv -> Heap -> Int -> [Bind] -> Maybe Heap
allocClosures' p_rhs h _ [] = return h
allocClosures' p_rhs h n (MkBind _ lf@(MkLambdaForm fvs uf vs e) _ : bs) = do
    vs' <- mapM (flip M.lookup p_rhs) vs
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

-- | `allocClosuresRec p h bs' adds closures for the recursive bindings in
--   `bs' to a heap `h' and adds address mappings to the local environment `p'.
allocClosuresRec :: LocEnv -> Heap -> [Bind] -> Maybe (LocEnv, Heap)
allocClosuresRec p h bs = do
    let
        n  = M.size h
        p' = allocAddr p n bs
    h' <- allocClosures' p' h n bs
    return (p', h')

--------------------------------------------------------------------------------

-- | `patternMatchCtr c cs' tries to find a case alternative in `cs' which
--   matches the constructor `c'.
patternMatchCtr :: String -> [AlgAlt] -> Maybe AlgAlt
patternMatchCtr c [] = Nothing
patternMatchCtr c (alt@(AAlt ctr _ _ _) : alts)
    | c == ctr  = Just alt
    | otherwise = patternMatchCtr c alts

-- | `patternMatchPrim k cs' tries to find a case alternative in `cs' which
--   matches the prim. integer `k'.
patternMatchPrim :: PrimInt -> [PrimAlt] -> Maybe PrimAlt
patternMatchPrim k [] = Nothing
patternMatchPrim k (alt@(PAlt v _ _) : alts)
    | k == v    = Just alt
    | otherwise = patternMatchPrim k alts

--------------------------------------------------------------------------------

-- | `step c` performs a single transition from a configuration `c`
step :: Config -> Maybe Config
-- Rules 1 and 11 (Application and variable bound to a prim. integer)
step (Eval (AppE f xs pos) p, as, rs, us, h, env) = do
    -- get the value associated with `f'
    v <- val p env (VarAtom f pos)

    -- figure out whether the value `v' is an address or a prim. integer
    -- to determine which rule to apply
    case v of
        -- Rule 1 (Application)
        AddrV a -> undefined
        -- Rule 11 (Variable bound to integer)
        IntV k  -> undefined
-- Rules 2 and 16 (Enter Non-Updatable/Updatable Closure)
step (Enter a, as, rs, us, h, env) = do
    -- find the closure at address `a' on the heap
    (Closure (MkLambdaForm vs u xs e) ws_f) <- M.lookup a h

    -- determine whether the closure is updatable or not
    case u of
        N ->
            -- ensure that there are at least as many items on the argument
            -- stack as are expected by the closure
            -- Rule 2 (Enter Non-Updatable Closure)
            if length as >= length xs then do
                undefined
            -- Rule 18 (NotEnoughArguments Update)
            else do
                -- the return stack should be empty
                guard (null rs)

                -- try to obtain an update frame from the update stack
                case us of
                    []                        -> Nothing
                    ((as_u, rs_u, a_u) : us') -> undefined
        -- Rule 16 (Enter Updatable Closure)
        U -> undefined
-- Rule 3 (Let expressions)
step (Eval (LetE bs e _) p, as, rs, us, h, env) = do
    undefined
-- Rule 4 (LetRec expressions)
step (Eval (LetRecE bs e _) p, as, rs, us, h, env) = do
    undefined
-- Rule 5 (Case expressions)
step (Eval (CaseE e alts _) p, as, rs, us, h, env) =
    undefined
-- Rule 6 (Constructors)
-- NOTE: `CtrE' may be called something else for you, depending on what
--       name you gave it in the previous exercise
--step (Eval (CtrE c xs _) p, as, rs, us, h, env) = do
--    undefined
-- Rule 7 (ReturnCon Case Match)
step (ReturnCon c ws, as, (AlgAlts cs d, p) : rs, us, h, env) =
    -- pattern-match on `c' using `cs' to determine which rule needs
    -- to be applied
    case patternMatchCtr c cs of
        -- Rule 7 (ReturnCon Case Match)
        (Just (AAlt _ vs e _)) -> do
            let
                -- construct a new local environment in which the pattern
                -- variables `vs' are bound to the arguments of the
                -- constructor `ws`.
                p' = undefined
            return (Eval e p', as, rs, us, h, env)
        Nothing                -> case d of
            -- Rule 8 (ReturnCon Case Default)
            (Default e _)      -> undefined
            -- Rule 9 (ReturnCon Case DefaultVar)
            (DefaultVar v e _) -> do
                let
                    -- get the current size of the heap (we can use this
                    -- to calculate the next available memory address, since
                    -- we never remove anything from the heap)
                    n  = M.size h
                    -- construct a new local environment in which the
                    -- variable `v' maps to some memory address
                    p' = undefined
                    -- a list of distinct variables for every argument of
                    -- the constructor
                    vs = ['v' : show i | i <- [0..length ws]]
                    -- a lambda form for the new closure
                    lf = undefined
                    -- an updated heap with the new closure added to it
                    h' = undefined
                return (Eval e p', as, rs, us, h', env)
-- Rule 10 (Literals)
step (Eval (LitE k _) p, as, rs, us, h, env) =
    undefined
-- Rules 12,13,14 (ReturnInt)
step (ReturnInt k, as, (PrimAlts cs d, p) : rs, us, h, env) =
    -- pattern-match on `k' using `cs' to determine which rule needs
    -- to be applied
    undefined
-- Rule 15 (Built-in operations)
step (Eval (OpE op [x1, x2] _) p, as, rs, us, h, env) = do
    -- look up the values of `x1' and `x2'
    i1 <- val p M.empty x1
    i2 <- val p M.empty x2

    -- check that both values are primitive integer values before
    -- calculating the result of the built-in operation and binding it to `r'
    r  <- case (i1,i2) of
        (IntV x, IntV y) -> undefined
        _                -> Nothing
    return (ReturnInt r, as, rs, us, h, env)
-- Rule 17 (Update triggered by an empty return stack)
step (ReturnCon c ws, [], [], (as, rs, a) : us, h, env) = do
    let
        -- a list of distinct variables for every value in `ws'
        vs = ['v' : show i | i <- [0..length ws]]
        -- construct the lambda form for the new closure
        lf = undefined
        -- construct an updated heap
        h' = undefined
    -- transition to the new configuration
    return (ReturnCon c ws, as, rs, us, h', env)
-- If no pattern matches, we are stuck:
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
