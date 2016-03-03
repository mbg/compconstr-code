--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module AbstractC where

--------------------------------------------------------------------------------

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer (WriterT, runWriterT, tell)

import qualified Data.Set as S
import qualified Data.Map as M

import Pretty
import Prim
import Var
import AST
import Types

import Debug.Trace (trace)

--------------------------------------------------------------------------------

forwardPtrTbl :: Symbol InfoTbl
forwardPtrTbl = InfoTblSym "_forwarding_ptr"

--------------------------------------------------------------------------------

-- | Enumerates stacks.
data Stack
    = ValStk
    | PtrStk

pushOffsetForStack :: Stack -> Int -> Int
pushOffsetForStack ValStk i = negate i
pushOffsetForStack PtrStk i = i

-- |
popOffsetForStack :: Stack -> Int -> Int
popOffsetForStack ValStk i = i + 1
popOffsetForStack PtrStk i = negate $ i + 1

-- | Enumerates registers.
data Register
    = NodeR
    | RetR
    | RetVecR
    | HeapPtrR
    | PtrStkR
    | ValStkR

-- | Types of symbols.
data SymType
    = Closure
    | InfoTbl
    | RetVec
    | Function
    | Local
    | Register
    | Indexed
    | Prim

-- | Symbols.
data Symbol (ty :: SymType) where
    ClosureSym  :: String -> Symbol Closure
    InfoTblSym  :: String -> Symbol InfoTbl
    RetVecSym   :: Int -> Symbol RetVec
    FunctionSym :: String -> Symbol Function
    LocalSym    :: String -> PolyType -> Symbol Local
    RegisterSym :: Register -> Symbol 'Register
    IndexSym    :: Symbol a -> Int -> PolyType -> Symbol Indexed
    PrimSym     :: PrimInt -> Symbol Prim

funSymName :: Symbol Function -> String
funSymName (FunctionSym n) = n

-- | An "Abstract C" source file
data AbstractC = AbstractC {
    cGlobals  :: S.Set String,
    cTypes    :: M.Map String Int,
    cCtrs     :: M.Map String Int,
    cIncludes :: S.Set String,
    cClosures :: M.Map String [String],
    cInfoTbls :: M.Map String [String],
    cRetVecs  :: M.Map Int [Symbol Function],
    cFuns     :: M.Map String [Stmt],
    cCounter  :: Int
}

-- | A statement in a function.
data Stmt
    = JumpStmt String
    | EnterStmt String
    | WriteRegisterStmt Register String
    | WriteRegisterIStmt Register Int String
    | WriteStackStmt Stack Int String
    | LoadRegisterStmt Register Stack Int
    | LoadLocalStmt Stack Int String
    | LocalFromRegisterStmt Register Int String
    | LocalFromSymbol String String
    -- | Adjust a stack.
    | AdjustStmt Stack Int
    | WriteHpStmt Int String
    | LoadLocalHpStmt Int String
    | AdjustHpStmt Int
    | StackOverflowCheckStmt
    | HeapOverflowCheckStmt Int
    | ReturnValStmt Int
    | PrimOpStmt PrimOp String String
    | CaseStmt [(PrimInt,String)] String
    | CallEvacStmt (Symbol Local) Int
    | CallScavStmt (Symbol Local) Int
    | ReturnStmt String
    | TraceStmt String

--------------------------------------------------------------------------------

initial :: AbstractC
initial = AbstractC {
    cGlobals = S.empty,
    cTypes    = M.empty,
    cCtrs     = M.empty,
    cIncludes = S.singleton "rts.h",
    cClosures = M.empty,
    cInfoTbls = M.empty,
    cRetVecs  = M.empty,
    cFuns     = M.empty,
    cCounter  = 0
}

--------------------------------------------------------------------------------

type CodeGen = State AbstractC

buildCtrIndex :: [TyBind] -> CodeGen ()
buildCtrIndex []                       = return ()
buildCtrIndex (MkTyBind t _ cs _ : bs) = do
    let
        addCtrs :: Int -> [AlgCtr] -> CodeGen ()
        addCtrs _ []                    = return ()
        addCtrs i (MkAlgCtr c _ _ : ds) = do
            modify $ \s -> s { cCtrs = M.insert c i (cCtrs s) }
            addCtrs (i+1) ds
    modify $ \s -> s { cTypes = M.insert t (length cs) (cTypes s) }
    addCtrs 0 cs
    buildCtrIndex bs

lookupCtrCount :: Ctr -> CodeGen (Maybe Int)
lookupCtrCount n = do
    ts <- gets cTypes
    return $ M.lookup n ts

lookupCtrIndex :: Ctr -> CodeGen (Maybe Int)
lookupCtrIndex n = do
    cs <- gets cCtrs
    return $ M.lookup n cs

fresh :: CodeGen Int
fresh = do
    n <- gets cCounter
    modify $ \s -> s { cCounter = n+1 }
    return n

closureForGlobal :: String -> Symbol Closure
closureForGlobal = ClosureSym

symbolsForFreeVars :: [String] -> [Symbol Closure]
symbolsForFreeVars = map ClosureSym

--------------------------------------------------------------------------------

staticClosure :: String -> Symbol InfoTbl -> [Symbol Closure] -> CodeGen (Symbol Closure)
staticClosure n (InfoTblSym tbl) fvs = do
    let
        ip = tbl ++ "_info"
        vp = [n ++ "_closure" | ClosureSym n <- fvs]
    modify $ \s -> s { cClosures = M.insert n (ip:vp) (cClosures s) }
    return (ClosureSym n)

infoTbl :: String -> [Symbol Function] -> CodeGen (Symbol InfoTbl)
infoTbl n fs = do
    modify $ \s -> s { cInfoTbls = M.insert n (map funSymName fs) (cInfoTbls s) }
    return (InfoTblSym n)

returnVector :: [Symbol Function] -> CodeGenFn (Symbol RetVec)
returnVector fs = lift $ lift $ do
    i <- fresh
    modify $ \s -> s { cRetVecs = M.insert i fs (cRetVecs s) }
    return (RetVecSym i)

withNewFunction :: String -> CodeGenFn () -> CodeGen (Symbol Function)
withNewFunction tpl k = do
    n <- fresh
    withNewNamedFunction (tpl ++ show n) k

withNewNamedFunction :: String -> CodeGenFn () -> CodeGen (Symbol Function)
withNewNamedFunction n k = do
    (_,stmts) <- evalStateT (runWriterT $ debug n >> k) initialFn
    modify $ \s -> s { cFuns = M.insert n stmts (cFuns s) }
    return $ FunctionSym n

--------------------------------------------------------------------------------

-- | Computations which generate code.
type CodeGenFn = WriterT [Stmt] (StateT FnState CodeGen)

data FnState = MkFnSt {
    fnFree            :: M.Map String (Int, PolyType),
    fnLocals          :: M.Map String PolyType,
    fnHeapTracked     :: M.Map String (Int, PolyType),
    fnPtrStackTracked :: M.Map String (Int, PolyType),
    fnValStackTracked :: M.Map String (Int, PolyType)
} deriving Show

initialFn :: FnState
initialFn = MkFnSt M.empty M.empty M.empty M.empty M.empty

localInfoTable :: String -> Symbol Function -> CodeGenFn (Symbol InfoTbl)
localInfoTable tpl (FunctionSym f) = lift $ lift $ do
    i <- fresh
    let
        n = tpl ++ show i
    modify $ \s -> s { cInfoTbls = M.insert n [f] (cInfoTbls s) }
    return (InfoTblSym n)

withNewLocalFunction ::
    String ->
    PolyType ->
    String ->
    CodeGenFn () ->
    CodeGenFn (Symbol Function)
withNewLocalFunction sym pt tpl k = do
    i <- lift $ lift fresh
    let
        n = tpl ++ show i
    st <- get
    (_,stmts) <- lift $ lift $ evalStateT (runWriterT $
        debug n >> loadLocalFromSymbol (RegisterSym NodeR) sym pt >>= const k) initialFn
    lift $ lift $ modify $ \s -> s { cFuns = M.insert n stmts (cFuns s) }
    return $ FunctionSym n

withNewFunctionInScope :: String -> CodeGenFn () -> CodeGenFn (Symbol Function)
withNewFunctionInScope tpl k = do
    i <- lift $ lift fresh
    let
        n = tpl ++ show i
    st <- get
    (_, stmts) <- lift $ lift $ evalStateT (runWriterT $ debug n >> k) st { fnLocals = M.empty, fnHeapTracked = M.empty }
    lift $ lift $ modify $ \s -> s { cFuns = M.insert n stmts (cFuns s) }
    return $ FunctionSym n

registerFreeVars :: [AVar PolyType] -> CodeGenFn ()
registerFreeVars fvs = modify $ \s -> s { fnFree = vs }
    where
        vs = M.fromList $ zip (map varName fvs) (zip [1..] (map varAnn fvs))

trackHeap :: String -> PolyType -> CodeGenFn ()
trackHeap n pt = modify $
    \s -> s { fnHeapTracked = M.insert n (0,pt) (fnHeapTracked s) }

trackStack :: Stack -> Int -> String -> PolyType -> CodeGenFn ()
trackStack PtrStk i n pt = modify $
    \s -> s { fnPtrStackTracked = M.insert n (i,pt) (fnPtrStackTracked s) }
trackStack ValStk i n pt = modify $
    \s -> s { fnValStackTracked = M.insert n (i,pt) (fnValStackTracked s) }

withVar :: String -> (forall a.Symbol a -> CodeGenFn b) -> CodeGenFn b
withVar n f = do
    st <- get

    -- 1. is it a local?
    case M.lookup n (fnLocals st) of
        Just t -> f $ LocalSym n t
        Nothing ->
            -- 2. is it a free variable?
            case M.lookup n (fnFree st) of
                Just (i,pt) -> f $ IndexSym (RegisterSym NodeR) i pt
                Nothing -> case M.lookup n (fnHeapTracked st) of
                    Just (i,pt) -> f $ IndexSym (RegisterSym HeapPtrR) i pt
                    Nothing -> case M.lookup n (fnPtrStackTracked st) of
                        Just (i,pt)  -> f $ IndexSym (RegisterSym PtrStkR) i pt
                        Nothing -> case M.lookup n (fnValStackTracked st) of
                            Just (i,pt)  -> f $ IndexSym (RegisterSym ValStkR) (negate i) pt
                            Nothing -> do
                                gs <- lift $ lift $ gets cGlobals

                                -- 3. is it a global?
                                if S.member n gs
                                then f $ closureForGlobal n
                                else fail $ "Internal error: symbol not found " ++ n

stackOverflowCheck :: CodeGenFn ()
stackOverflowCheck = tell [StackOverflowCheckStmt]

heapOverflowCheck :: Int -> CodeGenFn ()
heapOverflowCheck n = tell [HeapOverflowCheckStmt n]

writeRegister :: Register -> Symbol a -> CodeGenFn ()
writeRegister r sym = tell [WriteRegisterStmt r (render $ pp sym)]

writeRegisterIx :: Register -> Int -> Symbol a -> CodeGenFn ()
writeRegisterIx r ix sym = tell [WriteRegisterIStmt r ix (render $ pp sym)]

writeStack :: Stack -> Int -> Symbol a -> CodeGenFn ()
writeStack s i sym =
    tell [WriteStackStmt s (pushOffsetForStack s i) (render $ pp sym)]

loadLocalFromStack :: Stack -> Int -> String -> PolyType -> CodeGenFn (Symbol Local)
loadLocalFromStack s i n t = do
    tell [LoadLocalStmt s (popOffsetForStack s i) n]
    modify $ \s -> s { fnLocals = M.insert n t (fnLocals s) }
    return (LocalSym n t)

loadLocalFromRegister :: Register -> String -> PolyType -> CodeGenFn (Symbol Local)
loadLocalFromRegister r n t = do
    tell [LocalFromRegisterStmt r 0 n]
    modify $ \s -> s { fnLocals = M.insert n t (fnLocals s) }
    return (LocalSym n t)

loadLocalFromSymbol :: Symbol a -> String -> PolyType -> CodeGenFn (Symbol Local)
loadLocalFromSymbol sym n t = do
    tell [LocalFromSymbol (render $ pp sym) n]
    modify $ \s -> s { fnLocals = M.insert n t (fnLocals s) }
    return (LocalSym n t)

loadRegisterFromStack :: Register -> Stack -> Int -> CodeGenFn ()
loadRegisterFromStack r s i =
    tell [LoadRegisterStmt r s (popOffsetForStack s i)]

adjustStack :: Stack -> Int -> CodeGenFn ()
adjustStack s n = do
    tell [AdjustStmt s n]
    case s of
        PtrStk -> modify $ \s -> s { fnPtrStackTracked = M.map (\(i,pt) -> (i-n,pt)) (fnPtrStackTracked s) }
        ValStk -> modify $ \s -> s { fnValStackTracked = M.map (\(i,pt) -> (i-n,pt)) (fnValStackTracked s) }

writeHeap :: Int -> Symbol a -> CodeGenFn ()
writeHeap i sym = tell [WriteHpStmt i (render $ pp sym)]

loadLocalFromHeap :: Int -> String -> PolyType -> CodeGenFn (Symbol Local)
loadLocalFromHeap i n t = do
    tell [LoadLocalHpStmt i n]
    modify $ \s -> s { fnLocals = M.insert n t (fnLocals s) }
    return (LocalSym n t)

allocMemory :: String -> Int -> PolyType -> CodeGenFn ()
allocMemory v n pt = do
    trackHeap v pt
    modify $ \s -> s { fnHeapTracked = M.map (\(i,pt) -> (i-n,pt)) (fnHeapTracked s) }
    tell [AdjustHpStmt n]

-- | `deallocateMemory n' deallocates `n' words on the heap.
deallocateMemory :: Int -> CodeGenFn ()
deallocateMemory n = do
    modify $ \s -> s { fnHeapTracked = M.map (\(i,pt) -> (n+i,pt)) (fnHeapTracked s) }
    tell [AdjustHpStmt (negate n)]

jump :: Symbol a -> CodeGenFn ()
jump s = tell [JumpStmt $ render $ pp s]

enter :: Symbol a -> CodeGenFn ()
enter s = tell [EnterStmt $ render $ pp s]

returnVal :: PrimInt -> CodeGenFn ()
returnVal (MkPrimInt k) = tell [ReturnValStmt k]

builtin :: PrimOp -> Symbol a -> Symbol b -> CodeGenFn ()
builtin op x y = tell [PrimOpStmt op (render $ pp x) (render $ pp y)]

cases :: [(PrimInt, Symbol Function)] -> Symbol Function -> CodeGenFn ()
cases cs d = tell [CaseStmt [(i,render $ pp f) | (i,f) <- cs] (render $ pp d)]

debug :: String -> CodeGenFn ()
debug msg = tell [TraceStmt msg]

callEvac :: Symbol Local -> Int -> CodeGenFn ()
callEvac sym i = tell [CallEvacStmt sym i]

callScav :: Symbol Local -> Int ->  CodeGenFn ()
callScav sym i = tell [CallScavStmt sym i]

returnSymbol :: Symbol a -> CodeGenFn ()
returnSymbol sym = tell [ReturnStmt (render $ pp sym)]

--------------------------------------------------------------------------------

instance PP Stack where
    pp PtrStk = text "SpPtr"
    pp ValStk = text "SpVal"

instance PP Register where
    pp NodeR = text "Node"
    pp RetR = text "Ret"
    pp RetVecR = text "RetVec"
    pp HeapPtrR = text "Hp"
    pp PtrStkR = text "SpPtr"
    pp ValStkR = text "SpVal"

instance PP AbstractC where
    pp file = vcat (map ppInclude (S.toList $ cIncludes file))
           $$ text "// prototypes"
           $$ vcat (map ppClosureProto (M.toList $ cClosures file))
           $$ vcat (map ppInfoTblProto (M.toList $ cInfoTbls file))
           $$ vcat (map ppFunProto (M.toList $ cFuns file))
           $$ vcat (map ppRetVecProto (M.toList $ cRetVecs file))
           $$ text "// static closures and info tables"
           $$ vcat (map ppClosureDec (M.toList $ cClosures file))
           $$ vcat (map ppInfoTblDec (M.toList $ cInfoTbls file))
           $$ vcat (map ppRetVec (M.toList $ cRetVecs file))
           $$ text "// code"
           $$ vcat (map ppFunDec (M.toList $ cFuns file))

ppInclude :: String -> Doc
ppInclude fn =
    text "#include" <+> doubleQuotes (text fn)

ppClosureProto :: (String, [String]) -> Doc
ppClosureProto (n, ps) =
    text "StgWord" <+> text n <> text "_closure" <>
    brackets (int $ length ps) <> semi

ppInfoTblProto :: (String, [String]) -> Doc
ppInfoTblProto (n, ps) =
    text "StgWord" <+> text n <> text "_info" <>
    brackets (int $ length ps) <> semi

ppRetVecProto :: (Int, [Symbol Function]) -> Doc
ppRetVecProto (i, fs) =
    text "StgWord ret_vec_" <> int i <> brackets (int $ length fs) <> semi

ppClosureDec (n, ps) =
    text "StgWord" <+> text n <> text "_closure[]" <+> equals <+>
    braces (hcat $ punctuate comma $ map text ps) <> semi

ppInfoTblDec (n, ps) =
    text "StgWord" <+> text n <> text "_info[]" <+> equals <+>
    braces (hcat $ punctuate comma $ map text ps) <> semi

ppRetVec :: (Int, [Symbol Function]) -> Doc
ppRetVec (i, fs) =
    text "StgWord ret_vec_" <> int i <> brackets empty <+> equals <+>
    braces (hcat $ punctuate comma $ map pp fs) <> semi

ppFunProto (n, stmts) =
    text "CodeLabel" <+> text n <> parens empty <> semi

ppFunDec :: (String, [Stmt]) -> Doc
ppFunDec (n, stmts) =
    text "CodeLabel" <+> text n <> parens empty <+> lbrace $+$
    nest 4 (vcat $ map pp stmts) $+$ rbrace

instance PP (Symbol a) where
    pp (ClosureSym n) = char '&' <> text n <> text "_closure"
    pp (InfoTblSym n) = text n <> text "_info"
    pp (RetVecSym i) = text "ret_vec_" <> int i
    pp (FunctionSym n) = text n
    pp (LocalSym n t) = text n <> text "/* :: " <> pp t <> text " */"
    pp (RegisterSym r) = pp r
    pp (IndexSym (RegisterSym HeapPtrR) i pt) = char '&' <> text "Hp" <> brackets (int i)
    pp (IndexSym s i pt) = pp s <> brackets (int i)
    pp (PrimSym (MkPrimInt v)) = int v

instance PP Stmt where
    pp (JumpStmt n) = text "JUMP" <> parens (text n) <> semi
    pp (EnterStmt n) = text "ENTER" <> parens (text n) <> semi
    pp StackOverflowCheckStmt = text "if (SpPtr > SpVal) { stack_overflow(); }"
    pp (HeapOverflowCheckStmt n)
        | n /= 0    = text "if (Hp +" <+> int n <> text " > HLimit) { run_gc(); }"
        | otherwise = empty
    pp (WriteRegisterStmt r v) = pp r <+> equals <+> text v <> semi
    pp (WriteRegisterIStmt r ix v) =
        pp r <> brackets (int ix) <+> equals <+> text v <> semi
    pp (WriteStackStmt s i v) =
        pp s <> brackets (int i) <+> equals <+> text v <> semi
    pp (LoadRegisterStmt r s i) =
        pp r <+> equals <+> pp s <> brackets (int i) <> semi
    pp (LocalFromSymbol sym n) =
        text "StgWord" <+> text n <+> equals <+>
        text sym <> semi
    pp (LoadLocalStmt s i n) =
        text "StgWord" <+> text n <+> equals <+>
        pp s <> brackets (int i) <> semi
    pp (LocalFromRegisterStmt NodeR _ n) =
        text "StgWord*" <+> text n <+> equals <+> pp NodeR <> semi
    pp (LocalFromRegisterStmt r _ n) =
        text "StgWord" <+> text n <+> equals <+>
        pp r <> semi
    pp (AdjustStmt ValStk i)
        | i > 0     = text "SpVal -=" <+> int i <> semi
        | i < 0     = text "SpVal +=" <+> int (abs i) <> semi
        | otherwise = empty
    pp (AdjustStmt PtrStk i)
        | i > 0     = text "SpPtr +=" <+> int i <> semi
        | i < 0     = text "SpPtr -=" <+> int (abs i) <> semi
        | otherwise = empty
    pp (WriteHpStmt i s) =
        text "Hp" <> brackets (int $ negate i) <+> equals <+> text s <> semi
    pp (LoadLocalHpStmt i v) =
        text "StgWord" <+> text v <+> equals <+>
        text "Hp" <> brackets (int i) <> semi
    pp (AdjustHpStmt i)
        | i > 0     = text "Hp +=" <+> int i <> semi
        | i < 0     = text "Hp -=" <+> int (abs i) <> semi
        | otherwise = empty
    pp (ReturnValStmt k) = text "Ret" <+> equals <+> int k <> semi
    pp (PrimOpStmt op x y) =
        text "Ret" <+> equals <+>
        text "(int)" <> text x <+> ppPrimOp op <+>
        text "(int)" <> text y <> semi
    pp (CaseStmt cs d) =
        text "switch" <> parens (text "(int)Ret") <> lbrace $+$
        nest 4 (vcat (map ppSwCase cs) $$ ppSwDefault) $+$ rbrace
        where
            ppSwCase (MkPrimInt k,f) = text "case" <+> int k <> colon <> nest 4 (pp (JumpStmt f) $$ text "break" <> semi)
            ppSwDefault = text "default:" <> nest 4 ((pp $ JumpStmt d) $$ text "break" <> semi)
    pp (CallEvacStmt sym i) =
        text "Node" <+> equals <+> parens (text "StgWord*") <> pp (IndexSym sym i undefined) <> semi $$
        pp (IndexSym sym i undefined) <+> equals <+>
        parens (parens (text "CodeLabel") <>
            text "Node" <> brackets (int 0) <> brackets (int 1)) <>
        parens empty <> semi
    pp (CallScavStmt sym i) =
        text "Node" <+> equals <+> parens (text "StgWord*") <> pp (IndexSym sym i undefined) <> semi $$
        parens (parens (text "CodeLabel") <>
            text "Node" <> brackets (int 0) <> brackets (int 2)) <>
        parens empty <> semi
    pp (ReturnStmt sym) = text "return" <+> text sym <> semi
    pp (TraceStmt msg) = text "printf" <> parens (doubleQuotes $ text msg <> text "\\n") <> semi

ppPrimOp :: PrimOp -> Doc
ppPrimOp PrimAdd = char '+'
ppPrimOp PrimSub = char '-'
ppPrimOp PrimMul = char '*'
ppPrimOp PrimDiv = char '/'

--------------------------------------------------------------------------------
