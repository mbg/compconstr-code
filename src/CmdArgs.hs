--------------------------------------------------------------------------------
-- Compiler for the STG Language                                              --
-- By Michael B. Gale (michael.gale@cl.cam.ac.uk)                             --
--------------------------------------------------------------------------------

module CmdArgs (
    CmdArgs(..),

    parseCmdArgs
) where

--------------------------------------------------------------------------------

import Options.Applicative

--------------------------------------------------------------------------------

data CmdArgs = MkCmdArgs {
    argsInputs    :: [FilePath],
    argsEntry     :: String,
    argsVerbose   :: Bool,
    argsInterpret :: Bool,
    argsDebug     :: Bool
}

cmdArgsP :: Parser CmdArgs
cmdArgsP = MkCmdArgs
    <$> many (argument str (metavar "FILES..."))
    <*> strOption (long "entry"      <>
                   short 'e'         <>
                   metavar "BINDING" <>
                   value "main"      <>
                   help "Name of the program's entry point")
    <*> switch (long "verbose" <>
                short 'v'      <>
                help "Display more information during compilation")
    <*> switch (long "interpret" <>
                help "Interprets the file")
    <*> switch (long "debug" <>
                help "Debug the compiler")

cmdArgsOpts :: ParserInfo CmdArgs
cmdArgsOpts = info (helper <*> cmdArgsP) fullDesc

-- | Parses command line arguments.
parseCmdArgs :: IO CmdArgs
parseCmdArgs = execParser cmdArgsOpts
