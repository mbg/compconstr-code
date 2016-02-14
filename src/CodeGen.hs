--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module CodeGen where

--------------------------------------------------------------------------------

import Control.Monad.State

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

import System.IO

import Prim
import Var
import AST
import FreeVars
import Types
import AbstractC
import Pretty (render, pp)
import CodeGenAnalysis

--------------------------------------------------------------------------------

-- | `compile fp ast' compiles the program represented by `ast' to a file
--   whose path is given by `fp'.
compile :: FilePath -> AST PolyType -> IO ()
compile fp prog = withFile fp WriteMode $ \h ->
    hPutStrLn h (render $ pp $ runCodeGen prog)

-- | `runCodeGen ast' compiles the program represented by `ast' in an initial
--   compilation context and returns the resulting, abstract C program.
runCodeGen :: AST PolyType -> AbstractC
runCodeGen prog = execState (compProg prog) initial

--------------------------------------------------------------------------------

-- | `compProg ast' generates code for all the top-level bindings in `ast'.
compProg :: AST PolyType -> CodeGen ()
compProg (MkProg ts bs) = do
    buildCtrIndex ts
    let
        gs = map (varName . bindName) bs
    modify $ \s -> s { cGlobals = S.fromList gs }
    mapM_ compBinding bs

-- | `compBinding b' generates code for a binding `b'.
compBinding :: ABind PolyType -> CodeGen ()
compBinding (MkBind (Var name t) lf _) = do
    entry <- withNewNamedFunction (name ++ "_entry") (standardEntry lf)
    tbl   <- infoTbl name entry
    staticClosure name tbl (symbolsForFreeVars (map varName $ lfFreeVars lf))
    return ()

--------------------------------------------------------------------------------

-- | `compEnter sym' generates code to enter the closure pointed at by `sym'.
compEnter :: Symbol a -> CodeGenFn ()
compEnter sym = do
    -- set node register
    writeRegister NodeR sym

    -- jump
    enter (RegisterSym NodeR)

--------------------------------------------------------------------------------

-- | `loadArgs (0,0) vs' pops arguments off the two stacks and binds them to
--   local variables. The number of arguments obtained from each stack is
--   returned.
loadArgs ::
    (Int,Int)       ->
    [AVar PolyType] ->
    CodeGenFn ([Symbol Local], Int, Int)
loadArgs (p,v) []             = return ([], p,v)
loadArgs (p,v) (Var n t : vs)
    | isPrimitive t = do
        s          <- loadLocalFromStack ValStk v n t
        (ss,p',v') <- loadArgs (p,v+1) vs
        return (s:ss,p',v')
    | otherwise     = do
        s          <- loadLocalFromStack PtrStk p n t
        (ss,p',v') <- loadArgs (p+1,v) vs
        return (s:ss,p',v')

-- | `standardEntry lf' generates the standard entry code for `lf'
standardEntry :: ALambdaForm PolyType -> CodeGenFn ()
standardEntry (MkLambdaForm fvs uf vs e) = do
    -- register free variables in the current scope
    registerFreeVars $ map varName fvs

    -- generate the argument satisfaction check if there are arguments

    -- stack overflow check
    stackOverflowCheck

    -- heap overflow check

    -- if updatable, replace info pointer with black hole

    -- if updatable, construct update frame

    -- for simplicity, we pop all arguments off the stack here
    -- a more efficient implementation might leave them on the stack
    -- if possible
    -- YOUR CODE HERE

    -- compile the expression
    compExpr e

--------------------------------------------------------------------------------

-- | `storeVarOnHeap i v' stores a variable that is in scope somewhere on the
--   STG heap at offset -`i'.
storeVarOnHeap :: Int -> AVar a -> CodeGenFn ()
storeVarOnHeap i (Var x _) = withVar x $ \sym -> writeHeap i sym

-- | `storeVarsOnHeap i vs' stores variables on the heap, starting at offset
--    -`i', increasing with each variable.
storeVarsOnHeap :: Int -> [AVar a] -> CodeGenFn ()
storeVarsOnHeap i [] = return ()
storeVarsOnHeap i (v:vs) = do
    storeVarOnHeap i v
    storeVarsOnHeap (i-1) vs

-- | `storeAtomsOnHeap i as' stores the atoms in `as' on the heap, starting
--   at offset -`i', increasing with each atom.
storeAtomsOnHeap :: Int -> [AAtom PolyType] -> CodeGenFn ()
storeAtomsOnHeap i [] = return ()
storeAtomsOnHeap i (LitAtom k _ : as) = do
    writeHeap i (PrimSym k)
    storeAtomsOnHeap (i-1) as
storeAtomsOnHeap i (VarAtom x _ : as) = do
    storeVarOnHeap i x
    storeAtomsOnHeap (i-1) as

-- | `loadHeapArgs i vs' loads arguments from the heap and stores them in
--   local variables.
loadHeapArgs :: Int -> [AVar PolyType] -> CodeGenFn [Symbol Local]
loadHeapArgs i [] = return []
loadHeapArgs i (Var x t : vs) = do
    l  <- loadLocalFromHeap i x t
    ls <- loadHeapArgs (i+1) vs
    return (l:ls)

--------------------------------------------------------------------------------

type LocalEnv = M.Map String PolyType

-- | `saveEnvironment env' stores all of the variables in `env' on the
--   respective stacks. The number of items stored on each stack is returned.
--   The locations of the variables on the stacks will be tracked.
saveEnvironment :: LocalEnv -> CodeGenFn (Int,Int)
saveEnvironment fvs = go (0,0) vs
    where
        vs = M.toList fvs

        go (v,p) [] = return (v,p)
        go (v,p) ((n,t):as)
            | isPrimitive t = do
                withVar n $ \sym -> writeStack ValStk v sym
                trackStack ValStk n
                go (v+1,p) as
            | otherwise = do
                withVar n $ \sym -> writeStack PtrStk p sym
                trackStack PtrStk n
                go (v,p+1) as

restoreEnvironment :: LocalEnv -> CodeGenFn (Int,Int)
restoreEnvironment fvs =
    do
        (v,p) <- go (0,0) vs
        adjustStack ValStk (negate v)
        adjustStack PtrStk (negate p)

        return (v,p)
    where
        vs = reverse $ M.toList fvs

        go (v,p) [] = return (v,p)
        go (v,p) ((n,t):as)
            | isPrimitive t = do
                withVar n $ \sym -> loadLocalFromSymbol sym n t
                go (v+1,p) as
            | otherwise = do
                withVar n $ \sym -> loadLocalFromSymbol sym n t
                go (v,p+1) as

--------------------------------------------------------------------------------

-- | `allocClosures bs' allocates dynamic closures on the STG heap for all
--   bindings in `bs'
allocClosures :: [ABind PolyType] -> CodeGenFn ()
allocClosures [] = return ()
allocClosures (MkBind (Var n _) lf t : bs) = do
    -- generate the standard entry code for the closure
    entry <- lift $ lift $
        withNewFunction (n ++ "_entry_") (standardEntry lf)

    -- generate an info table on the C heap
    tbl <- lift $ lift $ infoTbl n entry

    -- calculate the size of the closure for this binding and allocate memory
    -- on the STG heap and refer to it as `n'
    let
        s = closureSize lf

    -- YOUR CODE HERE

    -- continue with the other bindings
    allocClosures bs

pushArgs :: (Int, Int) -> [AAtom PolyType] -> CodeGenFn (Int, Int)
pushArgs (v,p) []                 = return (v,p)
pushArgs (v,p) (LitAtom k _ : as) = do
    writeStack ValStk v (PrimSym k)
    pushArgs (v+1,p) as
pushArgs (v,p) (VarAtom (Var x _) _ : as) = do
    s <- withVar x $ \sym -> case sym of
        (LocalSym n t) | isPrimitive t -> do
            writeStack ValStk v sym
            return ValStk
        _              -> do
            writeStack PtrStk p sym
            return PtrStk

    -- push the remaining arguments onto the stack
    case s of
        ValStk -> pushArgs (v+1,p) as
        PtrStk -> pushArgs (v,p+1) as

-- | `compPrimDefault d' generates code for the default alternative of
--   a case expression on a value of a primitive type
compPrimDefault :: LocalEnv -> ADefaultAlt PolyType -> CodeGenFn ()
compPrimDefault env (Default e t) = restoreEnvironment env >> compExpr e
compPrimDefault env (DefaultVar v e t) = do
    restoreEnvironment env

    -- the value of v will be in Ret
    loadLocalFromRegister RetR (varName v) (varAnn v)

    -- compile the expression
    compExpr e

-- | `compPrimAlt alt' generates code for a primitive case alternative `alt'.
compPrimAlt :: LocalEnv -> APrimAlt PolyType -> CodeGenFn (PrimInt, Symbol Function)
compPrimAlt env (PAlt k e t) = do
    f <- withNewFunctionInScope "ret_prim_case_" (restoreEnvironment env >> compExpr e)
    return (k,f)

-- | `compReturn alts' generates a continuation for a case expression which
--   examines a primitive value with the alternatives in `as'.
compReturn :: LocalEnv -> AAlts PolyType -> CodeGenFn (Symbol Function)
compReturn env (AlgAlts [] d) = withNewFunctionInScope "ret_prim" $ do
    d <- withNewFunctionInScope "ret_prim_default" (compPrimDefault env d)

    cases [] d
compReturn env (PrimAlts as d) = withNewFunctionInScope "ret_prim" $ do
    d <- withNewFunctionInScope "ret_prim_default" (compPrimDefault env d)
    fs <- mapM (compPrimAlt env) as

    cases fs d

-- | `compAlgDefault d' generates code for the default alternative of
--   a case expression on a value of an algebraic type.
compAlgDefault :: LocalEnv -> ADefaultAlt PolyType -> CodeGenFn ()
compAlgDefault env (Default e t) = do
    restoreEnvironment env

    -- NOTE: there will be constructor arguments on the heap,
    --       but we don't know how much to remove (at this time)
    compExpr e
compAlgDefault env (DefaultVar v e t) = do
    restoreEnvironment env

    -- NOTE: implementing this is slightly tricky, so we don't (at this time)
    --       we would have to generate code for constructors of all arities
    --       and corresponding info tables so that they can return the
    --       arguments when we attempt to evaluate `v' somewhere

    -- compile the expression
    compExpr e

-- | `compAlgAltCont alt' generates code for an algebraic alternative.
compAlgAltCont :: LocalEnv -> AAlgAlt PolyType -> CodeGenFn ()
compAlgAltCont env (AAlt c vs e t) = do
    restoreEnvironment env

    -- pattern variables will be on the heap
    -- YOUR CODE HERE

    -- generate the code for the expression
    compExpr e

-- | `compAlgAlt alt' generates code for an algebraic alternative and
--   identifies the index of the constructor in the corresponding type.
compAlgAlt :: LocalEnv -> AAlgAlt PolyType -> CodeGenFn (Int, Symbol Function)
compAlgAlt env alt@(AAlt c vs e t) = do
    r <- lift $ lift $ lookupCtrIndex c

    case r of
        Nothing -> fail "Internal error: constructor not found"
        Just i  -> do
            f <- withNewFunctionInScope ("ret_" ++ c) (compAlgAltCont env alt)
            return (i,f)

-- | `compReturnVec t as' generates a return vector for a case expression
--   which examines a value of type `t' with the alternatives in `as'
compReturnVec :: LocalEnv -> String -> AAlts PolyType -> CodeGenFn (Symbol RetVec)
compReturnVec env pt (PrimAlts _ _) =
    -- this should not get past the type-checker
    fail "Internal error: primitive alts for algebraic type"
compReturnVec env pt (AlgAlts as d) = do
    r <- lift $ lift $ lookupCtrCount pt

    case r of
        Nothing -> fail "Internal error: can't find constructor count"
        Just c  -> do
            -- compile the default alternative
            dc <- withNewFunctionInScope "default" (compAlgDefault env d)

            cs <- mapM (compAlgAlt env) as

            -- initialise a return vector where every continuation is the
            -- default alternative
            let
                dv  = M.fromList [(i,dc) | i <- [0..c-1]]
                vec = M.fromList cs `M.union` dv

            returnVector (M.elems vec)

-- | `compBuiltIn op a b' generates code for a built-in operator, applied to
--   two arguments, `a' and `b'.
compBuiltIn :: PrimOp -> AAtom PolyType -> AAtom PolyType -> CodeGenFn ()
compBuiltIn op a b = withAtom a $ \x -> withAtom b $ \y -> builtin op x y
    where
        withAtom ::
            AAtom PolyType ->
            (forall a.Symbol a -> CodeGenFn ()) ->
            CodeGenFn ()
        withAtom (LitAtom v _) f = f (PrimSym v)
        withAtom (VarAtom v _) f = withVar (varName v) f

-- | `compExpr e' generates code for an expression `e'.
compExpr :: AExpr PolyType -> CodeGenFn ()
compExpr (LetE bs e _) = do
    -- allocate closures on the heap
    allocClosures bs

    -- compile code for e
    compExpr e
compExpr (LetRecE bs e _) = do
    -- allocate closures on the heap
    allocClosures bs

    -- compile code for e
    compExpr e
compExpr (CaseE e alts _) = do
    -- save volatile variables used by alts on the stacks and push
    -- return address on the ptr stack
    gl <- lift $ lift $ gets cGlobals

    let
        savedEnv = freeVars gl alts

    (v,p) <- saveEnvironment savedEnv
    adjustStack ValStk v
    adjustStack PtrStk p

    -- generate a continuation/vector of continuations depending on the type
    -- of the expression that is examined by this case expression
    if isPrimitive (exprAnn e) then do
        r <- compReturn savedEnv alts

        writeStack ValStk 0 r
        adjustStack ValStk 1
    else case algebraicType (exprAnn e) of
        Nothing -> fail "Internal error: case analysis on function"
        Just tn -> do
            -- generate return vector and continuations
            rv <- compReturnVec savedEnv tn alts

            -- push return vector
            writeStack ValStk 0 rv
            adjustStack ValStk 1

    -- followed by the code for e
    compExpr e
compExpr (AppE f as _) = do
    -- push arguments onto the appropriate stacks and adjust the stack
    -- pointers accordingly
    undefined

    -- enter the closure pointed to by f
    withVar (varName f) $ \sym -> compEnter sym
compExpr (CtrE c as _) = do
    -- obtain the return vector from the value stack
    undefined

    -- allocate a closure for the arguments
    unless (null as) $ do
        -- allocate enough memory for all of the constructor's arguments and
        -- a pointer to the constructor's info table
        -- NOTE: the info table bit is not implemented, since it is slightly
        --       tricky -- see the note in the definition of `compAlgDefault'
        undefined

        -- set the node register to the right location
        withVar "_c" $ \sym -> writeRegister NodeR sym

    -- look up the index of the constructor and jump to the
    -- corresponding entry in the return vector
    r <- lift $ lift $ lookupCtrIndex c

    case r of
        Nothing  -> error "Internal error: constructor not found"
        (Just i) -> jump (IndexSym (RegisterSym RetVecR) i)
compExpr (OpE op [x,y] _) = do
    -- pop the continuation off the pointer stack

    -- compile the operator application

    -- jump to the continuation
    undefined
compExpr (LitE v _) = do
    -- pop the continuation off the pointer stack
    k <- loadLocalFromStack ValStk 0 "_k" (MonoTy $ AlgTy "_Cont")
    adjustStack ValStk (-1)

    -- set the return register value
    -- returnVal v

    -- jump to the continuation
    jump k

--------------------------------------------------------------------------------
