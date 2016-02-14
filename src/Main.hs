--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module Main (main, mapMain) where

--------------------------------------------------------------------------------

import Control.Monad

import Data.Char (toLower)

import System.Environment (withArgs)
import System.Exit
import System.FilePath
import System.IO

import Pretty
import Lexer
import Parser
import CmdArgs
import Interpreter
import TypeInference
import CodeGen

--------------------------------------------------------------------------------

-- | `confirm k' prompts the user to confirm whether an action `k' should be
--   executed.
confirm :: IO () -> IO ()
confirm k = do
    putStr "Continue? [y/n] "

    -- get the user input; getLine instead of getChar because getChar is
    -- buggy on Windows
    r <- getLine

    case map toLower r of
        "y" -> k
        _   -> return ()

-- | `steps cfg' pretty-prints the configuration `cfg' and attempts to
--   transition to a new configuration; if successful, it will call itself
--   recursively with the new configuration. If unsuccessful, it will
--   terminate.
steps :: Config -> IO ()
steps cfg = do
    -- print the current configuration
    putStrLn $ render $ ppConfig cfg

    -- try to transition to the next
    case step cfg of
        Nothing     -> putStrLn "Can't reduce further."
        (Just cfg') -> confirm (steps cfg')

-- | The main entry point for the compiler.
main :: IO ()
main = do
    -- disable buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering

    -- parse command-line arguments
    args <- parseCmdArgs

    -- complain if there are no inputs
    when (null $ argsInputs args) $ do
        putStrLn "No inputs!"
        exitFailure

    -- process inputs
    forM_ (argsInputs args) $ \input -> do
        putStrLn $ render $ text "Parsing" <+> text input <> text "..."

        -- read and parse the source file
        r <- parseFile input programP

        case r of
            Left err  -> putStrLn err
            Right ast -> do
                -- render the AST
                when (argsVerbose args) $ do
                    putStrLn $ render $ pp ast
                    putStrLn ""

                -- interpret the program, if asked for
                when (argsInterpret args) $ do
                    putStrLn $ render $
                        text "Evaluating" <+> text input <+> text "..."

                    steps $ initialState ast (argsEntry args)

                putStrLn $ render $
                    text "Inferring types of" <+> text input <> text "..."

                -- infer the types
                case inferTypes ast of
                    Left err   -> putStrLn $ render $ pp err
                    Right (tast, l) -> do
                        -- print diagnostic messages as well as the typed
                        -- abstract syntax tree, if asked for
                        when (argsDebug args) $ do
                            putStrLn $ render $ vcat l
                            putStrLn ""
                        when (argsVerbose args) $ do
                            putStrLn $ render $ pp tast
                            putStrLn ""

                        -- compile the program to C
                        compile (input -<.> ".c") tast

    -- do nothing
    return ()

mapMain :: IO ()
mapMain = withArgs ["tests/Map.stg"] main
