--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module ParserSpec where

--------------------------------------------------------------------------------

import GHC.IO.Handle

import Control.Exception

import System.IO
import qualified System.IO.Strict as SIO
import System.Directory
import System.FilePath

import Test.Hspec

import Pretty
import Parser
import Interpreter

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
    m `catch` (\e -> print (e :: SomeException))
    hDuplicateTo stdout' stdout
    str <- SIO.readFile tmpf
    removeFile tmpf
    return str

--------------------------------------------------------------------------------

steps :: Config -> IO ()
steps cfg = do
    -- print the current configuration
    putStrLn $ render $ ppConfig cfg

    -- try to transition to the next
    case step cfg of
        Nothing     -> putStrLn "Can't reduce further."
        (Just cfg') -> steps cfg'

driver :: FilePath -> IO ()
driver input = do
    putStrLn $ render $ text "Parsing" <+> text input <> text "..."

    -- read and parse the source file
    r <- parseFile input programP

    case r of
        Left err  -> putStrLn err
        Right ast -> do
            -- render the AST
            putStrLn $ render $ pp ast
            putStrLn ""

            -- reduce the entry point
            putStrLn $ render $
                text "Evaluating" <+> text input <+> text "..."

            steps $ initialState ast "main"


parseTest :: FilePath -> Expectation
parseTest fp = let fp' = replaceExtension fp "stdout" in do
    result <- SIO.readFile fp'
    capture (driver fp) `shouldReturn` result

--------------------------------------------------------------------------------

spec :: Spec
spec = mapM_ (\fp -> it fp $ parseTest fp) files
