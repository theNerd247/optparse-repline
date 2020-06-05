{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Arbitrary.Repline where

import Control.Arrow (right, (***))
import Control.Monad.Free 
import Data.Char (isLetter)
import Data.Foldable (toList)
import Data.Functor.Foldable
import Data.Monoid (Alt(..), Ap(..), First(..))
import Numeric.Natural
import Options.Applicative
import Options.Repline
import Test.QuickCheck
import qualified Control.Monad.Trans.Free as CMTF

type Cmd a = Mod CommandFields a

type PVal a = Either (Cmd a) (Parser a)

-- ParserTree a = Free [] (Cmd a) = Free [] (Cmd a)

instance (Arbitrary1 f) => Arbitrary1 (Free f) where
  liftArbitrary genA = frequency 
    [ (3, Pure <$> genA)
    , (1, Free <$> liftArbitrary (liftArbitrary genA))
    ]
  liftShrink shrinkA (Pure a)  = Pure <$> (shrinkA a)
  liftShrink shrinkA (Free fFree) = Free <$> liftShrink (liftShrink shrinkA) fFree

-- fromParserTree :: (Foldable f) => Free f (Cmd a) -> m (Parser a)
-- fromParserTree = cata randParserAlg

fromPVal :: PVal a -> Parser a
fromPVal = either subparser id

randParserAlg :: (Foldable f) => CMTF.FreeF f (Cmd a) (PVal a) -> (PVal a)
randParserAlg (CMTF.Pure cmd) = Left cmd
randParserAlg (CMTF.Free ps)  = Right . uncurry (<|>) . (subparser *** getAlt) . foldMap (collectMonoid . right Alt) $ ps

collectMonoid :: (Monoid a, Monoid b) => Either a b -> (a, b)
collectMonoid = either ((,mempty)) ((mempty,))

testOptParser :: ParserInfo a ->  OptParser (Maybe a)
testOptParser p = OptParser
  { parserPrefs        = defaultPrefs
  , parserInfo         = p
  , handleParserResult = getParseResult
  }

emptyParser :: Parser a -> ParserInfo a
emptyParser = flip info mempty 

arbitraryCmd :: Gen (Cmd CmdName)
arbitraryCmd = mkCommand <$> arbitraryCmdName

arbitraryCmdName :: Gen CmdName
arbitraryCmdName = resize 7 $ listOf $ arbitrary `suchThat` isLetter

-- Creates a command parser that always succeeds and returns its name. This is
-- used to determine if the correct internal parser is called when commandline
-- args that corresponds to the created parser
mkCommand :: CmdName -> Cmd CmdName
mkCommand cmdName = command cmdName $ info (pure $ cmdName) mempty

withSelected :: (Foldable t) => t a -> Gen (t a, Maybe a)
withSelected t = ((t,)) <$> (maybeChooseFirst t)

maybeChooseFirst :: (Foldable t) => t a -> Gen (Maybe a)
maybeChooseFirst = fmap getFirst . getAp . foldMap (\x -> Ap $ First <$> elements [Nothing, Just x])
