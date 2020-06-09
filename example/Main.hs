module Main where

import Options.Repline
import Options.Applicative
import System.Console.Repline
import Control.Monad.Trans (lift)

type ReplCmdM = HaskelineT IO

data ReplCmd
  = Echo String
  | Quit
  deriving (Show)

main = evalRepl 
  (pure "> ") 
  (const $ pure ()) 
  options 
  (Just ':') 
  File 
  (lift $ putStrLn "Hello...")

options = ("help", mkHelpParser optParser) : toRepline optParser

optParser =
  OptParser 
  { parserPrefs        = prefs showHelpOnError
  , parserInfo         = mainParser
  , handleParserResult = handleResult
  }

handleResult :: ParserResult ReplCmd -> ReplCmdM ()
handleResult (Success cmd) = handleReplCmd cmd
handleResult (Failure x) = lift . putStrLn $ showFailure x

handleReplCmd :: ReplCmd -> ReplCmdM ()
handleReplCmd Quit     = abort
handleReplCmd (Echo s) = lift $ putStrLn s

mainParser :: ParserInfo ReplCmd
mainParser = info (helper <*> parser) fullDesc
  where
    parser = hsubparser $ echoReplCmd <> quitReplCmd

echoReplCmd :: Mod CommandFields ReplCmd
echoReplCmd = command "echo" $ info parser fullDesc
  where
    parser = Echo . unwords <$> some (strArgument (metavar "TEXT" <> help "text to echo"))

quitReplCmd :: Mod CommandFields ReplCmd
quitReplCmd = command "quit" $ info (pure Quit) $ fullDesc <> header "Quit the REPL"
