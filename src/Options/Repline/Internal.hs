{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Options.Repline.Internal where

import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (sortOn)
import Options.Applicative
import Options.Applicative.Common (mapParser)
import Options.Applicative.Types (OptReader(..), optMain)
import qualified System.Console.Repline as REPL

type Args = String
type CmdName = String
type Options a = [(String, Args -> a)]

data OptParser a = forall b. OptParser
  { parserPrefs        :: ParserPrefs
  , parserInfo         :: ParserInfo b
  , handleParserResult :: ParserResult b -> a
  }

toRepline :: OptParser a -> [(String, Args -> a)]
toRepline p@OptParser{..} = sortOn fst $ (fmap (mkToplevelCmdParser p) . collectTopLevelCmdNames $ parserInfo)

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
runParser OptParser{..} =
  handleParserResult . execParserPure parserPrefs parserInfo . words

commandNameFromArgs :: Args -> CmdName
commandNameFromArgs = fromMaybe "" . listToMaybe . words

showFailure :: ParserFailure ParserHelp -> String
showFailure = fst . flip renderFailure "" 

prependCmdName :: CmdName -> Args -> Args
prependCmdName cmdName s = cmdName <> " " <> s

appendHelpFlag :: Args -> Args
appendHelpFlag = (<> " -h")
