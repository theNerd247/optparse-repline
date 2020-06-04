{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Arbitrary.Repline where

import Control.Arrow (right, (***))
import Control.Monad.Trans.Free
import Data.Char (isLetter)
import Data.Foldable (toList)
import Data.Functor.Foldable
import Data.Monoid (Alt(..), Ap(..), First(..))
import Numeric.Natural
import Options.Applicative
import Options.Repline
import Test.QuickCheck

-- Rose a = Leaf a | Branch [(Rose a)] 
-- RoseF e a = Leaf e | Branch [a]
--
-- RoseF a = Free []

type Cmd a = Mod CommandFields a

type PVal a = Either (Cmd a) (Parser a)


randParserAlg :: FreeF [] (Cmd a) (PVal a) -> (PVal a)
randParserAlg (Pure cmd) = Left cmd
randParserAlg (Free [])  = Right empty
randParserAlg (Free ps)  = Right . uncurry (<|>) . (subparser *** getAlt) . foldMap (collectMonoid . right Alt) $ ps

randParserCoAlg :: Natural -> Gen (FreeF [] (Cmd CmdName) Natural)
randParserCoAlg 0 = Pure <$> arbitraryCmd
randParserCoAlg n = fmap Free $ resize (fromIntegral n) $ listOf $ resize (fromIntegral (n-2)) arbitrarySizedNatural

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
