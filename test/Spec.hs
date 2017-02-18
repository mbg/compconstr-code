--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import GHC.IO.Handle

import Control.Exception

import System.IO
import qualified System.IO.Strict as SIO
import System.Directory
import System.FilePath
import System.Process

import Test.Hspec

import CmdArgs
import Pretty
import Parser
import Interpreter
import Driver (driver)

--------------------------------------------------------------------------------

files :: [FilePath]
files =
    [ "tests/Map.stg"
    {-, "tests/codegen/example1/Prim.stg"
    , "tests/codegen/example2/BuiltIn.stg"
    , "tests/codegen/example3/TruthTest.stg"
    , "tests/codegen/example4/AlgToPrim.stg"
    , "tests/codegen/example5/Boxed.stg"
    , "tests/codegen/example6/LetRec.stg" -}
    ]

capture :: IO () -> IO String
capture m = do
    tmpd         <- getTemporaryDirectory
    (tmpf, tmph) <- openTempFile tmpd "stdout"
    stdout'      <- hDuplicate stdout
    hDuplicateTo tmph stdout
    hClose tmph
    m `catch` (\e -> print (e :: SomeException))
    hDuplicateTo stdout' stdout
    str <- SIO.readFile tmpf
    removeFile tmpf
    return str

--------------------------------------------------------------------------------

driverCfg :: FilePath -> CmdArgs
driverCfg input = MkCmdArgs {
    argsInputs    = [input],
    argsEntry     = "main",
    argsVerbose   = False,
    argsInterpret = True,
    argsDebug     = False,
    argsQuiet     = True
}

testDriver :: FilePath -> IO ()
testDriver fp = do
    -- run the STG compiler, this will compile the .stg file to a .c file
    driver (driverCfg fp) fp

    -- run GCC to compile the C code
    let
        cfile = fp -<.> "c"
        rtspath = "/cbits"
        rtssrc  = rtspath </> "rts.c"
        outfile = fp -<.> "out"

    r <- createProcess (proc "gcc" [cfile, rtssrc, "-I", rtspath, "-o", outfile])

    -- run the resulting executable
    r' <- createProcess (proc outfile [])

    return ()

runTest :: FilePath -> Expectation
runTest fp = let fp' = replaceExtension fp "stdout" in do
    result <- SIO.readFile fp'
    capture (testDriver fp) `shouldReturn` result

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ mapM_ (\fp -> it fp $ runTest fp) files
