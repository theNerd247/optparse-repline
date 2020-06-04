{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arbitrary.Repline where

import Options.Repline
import Test.QuickCheck
import Options.Applicative

-- The goal here is to generate random parsers for various
-- needs. Here is a sketch of some of the properties we'll
-- need to test:
--
--
-- Ensure proper cmd names extraction
-- 1. A parser with no toplevel subparser produces an empty list of toplevel commands
-- 2. A parser with N commands under a single subparser produces N toplevel commands
-- 3. A parser with M subparsers produces sum(Ni, 0, M) toplevel commands
--
-- Ensure calling a optparse-applicative parser is the same as appending command name
-- 1. Given command names NS and an Identity parser (idPN :: OptParser (Maybe CmdName))
-- then forall N in NS: runParser p [N] == Just N
-- 2. Given a command name N and idPN (previous item): runParser p [N, "--help"] == Nothing
-- assuming optparse-applicative defines the "--help" flags as identical to
-- failing a parser
--    

-- TODO: write instance
instance Arbitrary a => Arbitrary (OptParser a) where
  arbitrary = (\x -> OptParser
    { parserPrefs        = defaultPrefs
    , parserInfo         = 
    , handleParserResult = const x
    }) <$> arbitrary
