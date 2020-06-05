{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Arbitrary.Repline where

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
  (fromPTree *** getMaybeChooseFirst)
  . cata (cAlg randParserAlg maybeChooseFirst)

cAlg :: (Functor f) => (f a -> c) -> (f b -> d) -> f (a,b) -> (c,d)
cAlg algA algB = (algA . fmap fst) &&& (algB . fmap snd)

getCmdNames :: (Foldable f) => Free f CmdName -> [CmdName]
getCmdNames = toList

randParserAlg :: (x ~ Either (Cmd CmdName) (Parser CmdName), Foldable f) => CMTF.FreeF f CmdName x -> x
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

arbitraryCmdName :: Gen CmdName
arbitraryCmdName = resize 7 $ listOf $ arbitrary `suchThat` isLetter

-- Creates a command parser that always succeeds and returns its name. This is
-- used to determine if the correct internal parser is called when commandline
-- args that corresponds to the created parser
mkCommand :: CmdName -> Cmd CmdName
mkCommand cmdName = command cmdName $ info (pure $ cmdName) mempty

maybeChooseFirst :: (Foldable f, x ~ Ap Gen (First a)) => CMTF.FreeF f a x -> x
maybeChooseFirst (CMTF.Pure x) = Ap $ elements [First Nothing, First $ Just x]
maybeChooseFirst (CMTF.Free x) = foldMap id x

getMaybeChooseFirst :: Ap Gen (First a) -> Gen (Maybe a)
getMaybeChooseFirst = fmap getFirst . getAp
