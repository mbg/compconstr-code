--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module Main (main, mapMain) where

--------------------------------------------------------------------------------

import Control.Monad

import System.Environment (withArgs)
import System.Exit

import Pretty
import Lexer
import Parser
import CmdArgs

--------------------------------------------------------------------------------

-- | The main entry point for the compiler.
main :: IO ()
main = do
    -- parse command-line arguments
    args <- parseCmdArgs

    -- complain if there are no inputs
    when (null $ argsInputs args) $
        die "No inputs!"

    -- process inputs
    forM_ (argsInputs args) $ \input -> do
        putStrLn $ render $ text "Parsing" <+> text input <> text "..."

        -- read and parse the source file
        r <- parseFile input programP

        case r of
            Left err  -> putStrLn err
            Right ast -> putStrLn $ render $ pp ast

    -- do nothing
    return ()

mapMain :: IO ()
mapMain = withArgs ["tests/map.stg"] main
