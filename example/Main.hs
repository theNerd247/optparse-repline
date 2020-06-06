module Main where

import Options.Repline
import Options.Applicative
import System.Console.Repline
import Control.Monad.Trans (lift)

type CmdM = HaskelineT IO

main = evalRepl (pure "> ") (const $ pure ()) options (Just ':') File (lift $ putStrLn "Hello...")

options = ("help", mkHelpParser optParser) : toRepline optParser

optParser =
  OptParser 
  { parserPrefs        = prefs $ showHelpOnError
  , parserInfo         = mainParser
  , handleParserResult = handleResult
  }

handleResult :: ParserResult (CmdM ()) -> CmdM ()
handleResult (Success m) = m
handleResult (Failure x) = lift . putStrLn $ showFailure x

mainParser :: ParserInfo (CmdM ())
mainParser = info 
  ( hsubparser $
       echoCmd
    <> quitCmd
  ) fullDesc

echoCmd :: Mod CommandFields (CmdM ())
echoCmd = command "echo" $ 
  info 
    ( (lift . putStrLn) <$> strArgument (metavar "TEXT" <> help "text to echo")
    ) 
    fullDesc

quitCmd :: Mod CommandFields (CmdM ())
quitCmd = command "quit" $ 
  info 
    ( pure abort
    ) 
  (fullDesc <> (header "Quit the REPL"))
