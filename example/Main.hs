{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Monad.Trans (lift)
import qualified Options.Applicative as Options
import Options.Repline
import System.Console.Repline (CompleterStyle(..), ExitDecision(..), HaskelineT, ReplOpts(..), abort, evalReplOpts)

type ReplCmdM = HaskelineT IO

data ReplCmd
  = Echo String
  | Quit
  deriving (Show)

main = evalReplOpts ReplOpts{..}
  where
    banner = const (pure "> ")
    command = \input -> lift (putStrLn ("Got normal input: " <> input))
    options :: [(String, String -> ReplCmdM ())] =
      ("help", mkHelpParser optParser)
      : toRepline optParser
    prefix = Just ':'
    multilineCommand = Nothing
    tabComplete = File
    initialiser = lift $ putStrLn "Hello...."
    finaliser = pure Exit

optParser =
  OptParser
  { parserPrefs        = Options.prefs Options.showHelpOnError
  , parserInfo         = mainParser
  , handleParserResult = handleResult
  }

handleResult :: Options.ParserResult ReplCmd -> ReplCmdM ()
handleResult (Options.Success cmd) = handleReplCmd cmd
handleResult (Options.Failure x) = lift . putStrLn $ showFailure x

handleReplCmd :: ReplCmd -> ReplCmdM ()
handleReplCmd Quit     = abort
handleReplCmd (Echo s) = lift $ putStrLn s

mainParser :: Options.ParserInfo ReplCmd
mainParser = Options.info (Options.helper <*> parser) Options.fullDesc
  where
    parser = Options.hsubparser $ echoReplCmd <> quitReplCmd

echoReplCmd :: Options.Mod Options.CommandFields ReplCmd
echoReplCmd = Options.command "echo" $ Options.info parser Options.fullDesc
  where
    parser = Echo . unwords <$> some (Options.strArgument (Options.metavar "TEXT" <> Options.help "text to echo"))

quitReplCmd :: Options.Mod Options.CommandFields ReplCmd
quitReplCmd = Options.command "quit" $ Options.info (pure Quit) $ Options.fullDesc <> Options.header "Quit the REPL"
