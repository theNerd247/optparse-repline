{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Arbitrary.Repline where

import Data.Coerce
import Control.Newtype
import Control.Arrow
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

type ParserTree = Free []

newtype CmdName' = CmdName' { unCmdName :: CmdName }
  deriving (Show)

instance Arbitrary CmdName' where
  arbitrary = coerce <$> arbitraryCmdName
  shrink    = coerce . (shrink @CmdName) . coerce

arbitraryCmdName :: Gen CmdName
arbitraryCmdName = resize 7 $ listOf $ arbitrary `suchThat` isLetter

instance (Arbitrary a, Arbitrary1 f) => Arbitrary (Free f a) where
  arbitrary = liftArbitrary arbitrary
  shrink    = liftShrink shrink

instance (Arbitrary1 f) => Arbitrary1 (Free f) where
  liftArbitrary genA = sized $ \case
    0 -> Pure <$> genA
    n -> fmap Free $ liftArbitrary $ choose (0, n-1) >>= flip resize (liftArbitrary genA)
  liftShrink shrinkA (Pure a)     = Pure <$> (shrinkA a)
  liftShrink shrinkA (Free fFree) = Free <$> liftShrink (liftShrink shrinkA) fFree

fromParserTree :: (Functor f, Foldable f) => Free f CmdName -> ParserInfo CmdName
fromParserTree = fromPTree . cata randParserAlg

fromPTree :: Either (Cmd a) (Parser a) -> ParserInfo a
fromPTree = emptyParser . either subparser id

fromParserTreeSelName :: (Functor f, Foldable f) => Free f CmdName -> (ParserInfo CmdName, Gen (Maybe CmdName))
fromParserTreeSelName = 
  (fromPTree *** id)
  . cata (cAlg randParserAlg maybeChooseFirst)

cAlg :: (Functor f) => (f a -> c) -> (f b -> d) -> f (a,b) -> (c,d)
cAlg algA algB = (algA . fmap fst) &&& (algB . fmap snd)

getCmdNames :: (Foldable f) => Free f CmdName -> [CmdName]
getCmdNames = toList

randParserAlg :: (Foldable f) => CMTF.FreeF f CmdName (Either (Cmd CmdName) (Parser CmdName)) -> Either (Cmd CmdName) (Parser CmdName)
randParserAlg (CMTF.Pure cmd) = Left $ mkCommand cmd
randParserAlg (CMTF.Free ps)  = 
    Right 
  . uncurry (<|>) 
  . (subparser *** getAlt) 
  . foldMap (eitherMonoid . right Alt) 
  $ ps

eitherMonoid :: (Monoid a, Monoid b) => Either a b -> (a, b)
eitherMonoid = ((,mempty)) ||| ((mempty,))

testOptParser :: ParserInfo a ->  OptParser (Maybe a)
testOptParser p = OptParser
  { parserPrefs        = defaultPrefs
  , parserInfo         = p
  , handleParserResult = getParseResult
  }

emptyParser :: Parser a -> ParserInfo a
emptyParser = flip info mempty 

-- Creates a command parser that always succeeds and returns its name. This is
-- used to determine if the correct internal parser is called when commandline
-- args that corresponds to the created parser
mkCommand :: CmdName -> Cmd CmdName
mkCommand cmdName = command cmdName $ info (pure $ cmdName) mempty

maybeChooseFirst :: (Foldable f) => CMTF.FreeF f a (Gen (Maybe a)) -> Gen (Maybe a)
maybeChooseFirst = foldAlgMap (Af . fmap First) (elements . (:mempty) . Just)

newtype Af f a = Af { unAf :: f a }
  deriving (Functor, Applicative)

instance (Applicative f, Monoid m) => Semigroup (Af f m) where
  x <> y = Af $ (<>) <$> (coerce x) <*> (coerce y)

instance (Applicative f, Monoid m) => Monoid (Af f m) where
  mempty = Af $ pure mempty

instance (Functor f, Newtype a b) => Newtype (Af f a) (f b) where
  pack   = Af . fmap pack
  unpack = fmap unpack . unAf

foldAlgMap :: (Newtype m b, Foldable f, Monoid m) => (b -> m) -> (a -> b) -> CMTF.FreeF f a b -> b
foldAlgMap _ f (CMTF.Pure x) = f x
foldAlgMap g _ (CMTF.Free x) = ala g foldMap x
