--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module ParserSpec where

--------------------------------------------------------------------------------

import GHC.IO.Handle

import System.IO
import qualified System.IO.Strict as SIO
import System.Directory
import System.FilePath

import Test.Hspec

import Pretty
import Parser

--------------------------------------------------------------------------------

files :: [FilePath]
files = ["tests/Map.stg"]

capture :: IO () -> IO String
capture m = do
    tmpd         <- getTemporaryDirectory
    (tmpf, tmph) <- openTempFile tmpd "stdout"
    stdout'      <- hDuplicate stdout
    hDuplicateTo tmph stdout
    hClose tmph
    m
    hDuplicateTo stdout' stdout
    str <- SIO.readFile tmpf
    removeFile tmpf
    return str

--------------------------------------------------------------------------------

driver :: FilePath -> IO ()
driver input = do
    putStrLn $ render $ text "Parsing" <+> text input <> text "..."

    -- read and parse the source file
    r <- parseFile input programP

    case r of
        Left err  -> putStrLn err
        Right ast -> putStrLn $ render $ pp ast


parseTest :: FilePath -> Expectation
parseTest fp = let fp' = replaceExtension fp "stdout" in do
    result <- SIO.readFile fp'
    capture (driver fp) `shouldReturn` result

--------------------------------------------------------------------------------

spec :: Spec
spec = mapM_ (\fp -> it fp $ parseTest fp) files
