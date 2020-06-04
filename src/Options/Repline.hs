{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module Options.Repline where

import Options.Applicative
import Options.Applicative.Types
import Data.List (sortOn)
import Options.Applicative.Common (mapParser)
import qualified System.Console.Repline as REPL

type CmdName = String
type Options a = [(String, Args -> a)]

data OptParser a = forall b. OptParser
  { parserPrefs        :: ParserPrefs
  , parserInfo         :: ParserInfo b
  , handleParserResult :: ParserResult b -> a
  }

toRepline :: OptParser a -> [(String, Args -> a)]
toRepline p@OptParser{..} = 
      sortOn fst
  $  ("help", mkHelpParser p)
  : (fmap (mkToplevelCmdParser p) . collectTopLevelCmdNames $ parserInfo)

collectTopLevelCmdNames :: ParserInfo a -> [CmdName]
collectTopLevelCmdNames = mconcat . mapParser (const $ getTopLevelCmdNames . optMain) . infoParser

getTopLevelCmdNames :: OptReader a -> [CmdName]
getTopLevelCmdNames (CmdReader _ cmds _) = cmds
getTopLevelCmdNames _                    = mempty

mkToplevelCmdParser :: OptParser a -> CmdName -> (CmdName, Args -> a)
mkToplevelCmdParser pInfo cmdName = 
  ( cmdName
  , runParser pInfo . prependCmdName cmdName 
  )

mkHelpParser :: OptParser a -> Args -> a
mkHelpParser pInfo = runParser pInfo . appendHelpFlag

runParser :: OptParser a -> Args -> a
runParser OptParser{..} = handleParserResult . execParserPure parserPrefs parserInfo 

commandNameFromArgs :: Args -> CmdName
commandNameFromArgs []     = ""
commandNameFromArgs (x:xs) = x

showFailure :: ParserFailure ParserHelp -> String
showFailure = fst . flip renderFailure "" 

prependCmdName :: CmdName -> Args -> Args
prependCmdName = (:)

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
-- Problem 4:
-- Consuming left over arguments might not be trivial. E.g:
--
--    :apropos foo bar blarg 
-- Would be parsed by optparse applicative where "foo bar blarg" are considered
-- command line arguments as apposed to a single string. Repline creates the
-- args as ["foo", "bar", "blarg"]. We need somehow to convert this into a single
-- string before passing it to the optparse-applicative parser.
--
-- Solution 4:
--
--   consumeLine :: Parser String
--   consumeLine = unwords <$> many strArg (Definitely won't work because of many but, it gets the point across)
--
-- This would consume the rest of the list of args and use `unwords` to
-- recreate a string that allows the user to treat the args as a string
--
--  
