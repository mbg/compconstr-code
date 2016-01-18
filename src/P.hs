--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module P where

--------------------------------------------------------------------------------

import Pretty
import Token
import Lexer
import AST

--------------------------------------------------------------------------------

newtype P a = MkP { runParser :: AlexState -> Either String a }

--------------------------------------------------------------------------------

instance Functor P where
    fmap f (MkP m) = MkP $ fmap f . m

instance Applicative P where
    pure x = MkP (\s -> Right x)

    (MkP f) <*> (MkP x) = MkP $ \s -> case f s of
        Left err -> Left err
        Right f' -> case x s of
            Left err -> Left err
            Right x' -> Right (f' x')

instance Monad P where
    (MkP m) >>= f = MkP $ \s -> case m s of
        Left err -> Left err
        Right r  -> let (MkP m') = f r in m' s

--------------------------------------------------------------------------------

mkParseState :: String -> AlexState
mkParseState xs = AlexState {
    alex_pos = AlexPn 0 1 1,
    alex_inp = xs,
    alex_chr = '\n',
    alex_bytes = [],
    alex_scd = 0
}

parseFile :: FilePath -> P a -> IO (Either String a)
parseFile fp p = do
    xs <- readFile fp
    return $ runParser p (mkParseState xs)

-- | `parseError tkn' raises a parser error as a result of encountering `tkn'.
parseError :: TokenP -> P a
parseError (tkn, pos) = MkP $ \s -> Left $ render $
    pp pos <+> text "Parse error: unexpected" <+> pp tkn

--------------------------------------------------------------------------------

toPosn :: AlexPosn -> Posn
toPosn (AlexPn a l c) = FilePosn l c

mkBind :: TokenP -> LambdaForm -> P Bind
mkBind (TVar var, pos) lf = return $ MkBind var lf (toPosn pos)

mkLambdaForm :: [Var] -> UpdateFlag -> [Var] -> Expr -> P LambdaForm
mkLambdaForm fvs uf vs expr = return $ MkLambdaForm fvs uf vs expr

mkLetE :: AlexPosn -> [Bind] -> Expr -> P Expr
mkLetE pos bs expr = return $ LetE bs expr (toPosn pos)

mkLetRecE :: AlexPosn -> [Bind] -> Expr -> P Expr
mkLetRecE pos bs expr = return $ LetRecE bs expr (toPosn pos)

mkCaseE :: AlexPosn -> Expr -> Alts -> P Expr
mkCaseE pos expr alts = return $ CaseE expr alts (toPosn pos)

mkAppE :: TokenP -> [Atom] -> P Expr
mkAppE (TVar var, pos) as = return $ AppE var as (toPosn pos)

mkCtrE :: TokenP -> [Atom] -> P Expr
mkCtrE = undefined

mkOpE :: TokenP -> [Atom] -> P Expr
mkOpE (TPrimOp op, pos) as = return $ OpE op as (toPosn pos)

mkLitE :: TokenP -> P Expr
mkLitE (TPrimInt val, pos) = return $ LitE val (toPosn pos)

mkAlgAlt :: TokenP -> [Var] -> Expr -> P AlgAlt
mkAlgAlt = undefined

mkPrimAlt :: TokenP -> Expr -> P PrimAlt
mkPrimAlt (TPrimInt val, pos) expr = return $ PAlt val expr (toPosn pos)

mkDefaultVar :: TokenP -> Expr -> P DefaultAlt
mkDefaultVar (TVar var, pos) expr = return $ DefaultVar var expr (toPosn pos)

mkDefault :: AlexPosn -> Expr -> P DefaultAlt
mkDefault pos expr = return $ Default expr (toPosn pos)

mkVar :: TokenP -> P Atom
mkVar (TVar var, pos) = return $ VarAtom var (toPosn pos)

mkInt :: TokenP -> P Atom
mkInt (TPrimInt val, pos) = return $ LitAtom val (toPosn pos)
