{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Options.Repline where

import Options.Applicative
import Options.Applicative.Types
import Control.Arrow ((>>>))
import Options.Applicative.Common (mapParser)
import qualified System.Console.Repline as REPL

type Options a = [(String, [String] -> a)]

-- | A natural transformation for converting optparse-applicative style parsers
-- into the format required by repline.
toRepline :: ParserInfo a -> Options a
toRepline pInfo@ParserInfo{..} = undefined -- TODO:  implement using mapParser

-- Parser a is the free applicative, alternative, and monad on the Option
-- functor
-- toRepline' :: Option a -> Options a
-- toRepline' = optMain >>> \case 
--   (CmdReader groupName cmds parseCmd) -> foldMap (toRepline . parseCmd) cmds
--   (ArgReader cReader) -> -- ~ ReaderT String (Except e) a  ≅ [String] -> a
--   _ -> mempty

optReaderToReplineOption :: OptReader a -> [String] -> a
optReaderToReplineOption = \case
  OptReader optNames creader mkError          -> 
  FlagReader optNames value                   -> 
  ArgReader cReader                           -> 
  CmdReader mGroupName cmds selectParserInfos -> 

-- (String, ....) --> for toplevel commands only, composable only between
-- commands / command groups
-- [String] -> a  --> for everything (defines how to parse commands),
-- composable
--
--
-- Problem 1:
-- Repline expects options of this type
-- options :: CmdName :*: Args -> a
--
-- Optparse Applicative creates this function
-- runParser :: [CmdName] <> Args -> a
--
-- Solution 1:
-- prependCmdName :: CmdName -> (Args -> a) -> Args -> a
-- prependCmdName cmdName = mappend $ pure cmdName
--
-- Problem 2:
--
--   parseAll :: ParserInfo a -> ([String] -> a)
--
-- User creates a SINGLE (ParserInfo a). If we were to use only runParserPure
-- we would get only 1 (parseAll :: ParserInfo a -> [String] -> a) as apposed
-- to a [[String] -> a] which is what we need to reconstruct the interface that
-- repline expects.
--
-- Solution 2:
--
-- Theorem 2:
-- parseAll can be used to parse ALL commands. A parser, by definition, is a
-- mapping between a [String] to a final result (modulo error handling). This
-- means by select some [String] I also select some parser to run and give a 
-- final result.  If I modify the incoming arguments than I am also modifying
-- which parser is selected and run. There is a 1-1 correspondence between the
-- arguments being parsed and the parser which gets executed.
--
-- If Theorem 2 is true then I can construct the interface repline expects by
-- modifying the incoming arguments by using prependCmdName from Solution 1:
--
--    mkToplevelCmdParser :: ParserInfo a -> CmdName -> (CmdName, Args -> a)
--    mkToplevelCmdParser pInfo cmdName = (cmdName, prependCmdName (parseAll pInfo) cmdName)
--
-- The only stpe after this is to collect all toplevel commands from the parser
-- and construct the repline parser:
--
--    toRepline :: ParserInfo a -> [(String, [String] -> a)]
--    toRepline = uncurry ($) . (fmap . mkToplevelCmdParser) &&& collectTopLevelCmdNames 
--  

-- | Constructs a help command that renders the help text 
-- for optparse applicative
mkHelpCommand :: ParserInfo a -> Options a
mkHelpCommand info = pure $ ("help", showHelp info)

showHelp :: ParserInfo a -> [String] -> a
showHelp = undefined

-- | Hask, (), (,)
--
-- i :: () -> F ()
-- m :: a x b -> F (a x b)
--
-- Fix f = f (Fix f)
--
-- Fix f ≅ FreeM f Void
--
-- FreeM f a
--  = Pure a
--  | Free (f (FreeF f a))
--
-- FreeA f a
--  = Pure a
--  | forall b. App (f (b -> a)) (FreeAp f a)
