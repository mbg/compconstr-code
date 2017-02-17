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

import CmdArgs
import Driver

--------------------------------------------------------------------------------

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
    forM_ (argsInputs args) (driver args)

    -- do nothing
    return ()

mapMain :: IO ()
mapMain = withArgs ["tests/Map.stg"] main
