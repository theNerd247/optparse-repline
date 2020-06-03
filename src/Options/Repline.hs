{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Options.Repline where

import Options.Applicative
import Options.Applicative.Types
import Control.Arrow
import Options.Applicative.Common (mapParser)
import qualified System.Console.Repline as REPL

type CmdName = String
type Options a = [(String, Args -> a)]

-- | A natural transformation for converting optparse-applicative style parsers
-- into the format required by repline.
toRepline :: ParserInfo a -> [(String, Args -> a)]
toRepline p = mkToplevelCmdParser p <$> (collectTopLevelCmdNames p)

collectTopLevelCmdNames :: ParserInfo a -> [CmdName]
collectTopLevelCmdNames = undefined

mkToplevelCmdParser :: ParserInfo a -> CmdName -> (CmdName, Args -> a)
mkToplevelCmdParser pInfo cmdName = 
  ( cmdName
  , runParser pInfo . prependCmdName cmdName
  )

runParser :: ParserInfo a -> Args -> a
runParser = undefined

prependCmdName :: String -> Args -> Args
prependCmdName cmdName = ([cmdName] <>)

appendHelpFlag :: Args -> Args
appendHelpFlag = (<> ["--help"])

-- 
-- (String, ....) --> for toplevel commands only, composable only between
-- commands / command groups
-- Args -> a  --> for everything (defines how to parse commands),
-- composable
--
--
-- Problem 1:
--
-- Repline expects argument parsers without toplevel command names
-- but Optparse Applicative expects arguments WITH toplevel command names
--
-- Solution 1:
--
--   prependCmdName :: CmdName -> Args -> Args
--   prependCmdName cmdName = (<> [cmdName])
--
-- Problem 2:
--
--   parseAll :: ParserInfo a -> (Args -> a)
--
-- User creates a SINGLE (ParserInfo a). If we were to use only runParserPure
-- we would get only 1 (parseAll :: ParserInfo a -> Args -> a) as apposed
-- to a [Args -> a] which is what we need to reconstruct the interface that
-- repline expects.
--
-- Solution 2:
--
-- Theorem 2:
-- parseAll can be used to parse ALL commands. A parser, by definition, is a
-- mapping between a Args to a final result (modulo error handling). This
-- means by select some Args I also select some parser to run and give a 
-- final result.  If I modify the incoming arguments than I am also modifying
-- which parser is selected and run. There is a 1-1 correspondence between the
-- arguments being parsed and the parser which gets executed.
--
-- If Theorem 2 is true then I can construct the interface repline expects by
-- modifying the incoming arguments by using prependCmdName from Solution 1:
--
--    mkToplevelCmdParser :: ParserInfo a -> CmdName -> (CmdName, Args -> a)
--    mkToplevelCmdParser pInfo cmdName = (cmdName, parseAll pInfo . prependCmdName $ cmdName)
--
-- The only stpe after this is to collect all toplevel commands from the parser
-- and construct the repline parser:
--
--    toRepline :: ParserInfo a -> [(String, Args -> a)]
--    toRepline = uncurry ($) . (fmap . mkToplevelCmdParser) &&& collectTopLevelCmdNames 
--
-- Problem 3:
-- Generating toplevel help command
--
-- Using Theorem 2:
-- We can easily select the top help-parser by passing the toplevel parser a "--help" flag.
-- We can also select subcommand help by NOT appending the name of the toplevel help command
-- and instead append the "-h" flag to the given arguments. For example:
--
--   :help     |--> ["--help"]
--   :help foo |--> ["foo", "--help"]
--
--   mkHelpArgs :: (Args -> a) -> Args -> a
--   mkHelpArgs = (. (<> ["-h"]))
--
--   topLevelHelp :: CmdName -> ParserInfo a -> (CmdName, Args -> a)
--   topLevelHelp cmdName pInfo = (cmdName, parseAll pInfo . mkHelpArgs)
--  
