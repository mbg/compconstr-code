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
    argsInputs :: [FilePath]
}

cmdArgsP :: Parser CmdArgs
cmdArgsP = MkCmdArgs
    <$> many (argument str (metavar "FILES..."))

cmdArgsOpts :: ParserInfo CmdArgs
cmdArgsOpts = info (helper <*> cmdArgsP) fullDesc

-- | Parses command line arguments.
parseCmdArgs :: IO CmdArgs
parseCmdArgs = execParser cmdArgsOpts
