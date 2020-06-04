{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Arbitrary.Repline where

import Data.Char (isLetter)
import Data.Foldable (toList)
import Data.Monoid (Alt(..), Ap(..), First(..))
import Options.Applicative
import Options.Repline
import Test.QuickCheck

subParserTestParser :: [[CmdName]] -> ParserInfo CmdName
subParserTestParser = emptyParser . getAlt . foldMap (Alt . subparser . foldMap mkCommand)

testOptParser :: ParserInfo a ->  OptParser (Maybe a)
testOptParser p = OptParser
  { parserPrefs        = defaultPrefs
  , parserInfo         = p
  , handleParserResult = getParseResult
  }

emptyParser :: Parser a -> ParserInfo a
emptyParser = flip info mempty 

arbitrarySubParserCmdNames :: Gen [[CmdName]]
arbitrarySubParserCmdNames = resize 10 $ listOf arbitraryCmdNames

arbitraryCmdNames :: Gen [CmdName]
arbitraryCmdNames = resize 10 $ listOf arbitraryCmdName

arbitraryCmdName :: Gen CmdName
arbitraryCmdName = resize 7 $ listOf $ arbitrary `suchThat` isLetter

-- Creates a command parser that always succeeds and returns its name. This is
-- used to determine if the correct internal parser is called when commandline
-- args that corresponds to the created parser
mkCommand :: CmdName -> Mod CommandFields CmdName
mkCommand cmdName = command cmdName $ info (pure $ cmdName) mempty

withSelected :: (Foldable t) => t a -> Gen (t a, Maybe a)
withSelected t = ((t,)) <$> (maybeChooseFirst t)

maybeChooseFirst :: (Foldable t) => t a -> Gen (Maybe a)
maybeChooseFirst = fmap getFirst . getAp . foldMap (\x -> Ap $ First <$> elements [Nothing, Just x])
