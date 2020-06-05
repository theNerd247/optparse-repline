{-# LANGUAGE TypeApplications #-}

module Main where

import Arbitrary.Repline
import Data.Functor.Compose (Compose(..))
import Data.List (sort)
import Options.Applicative
import Options.Repline
import Test.QuickCheck
import Test.Tasty
import Data.Coerce
import qualified Test.Tasty.QuickCheck as QC

main = defaultMain tests

tests :: TestTree
tests = testGroup "Properties" [qcProperties]

qcProperties :: TestTree
qcProperties = localOption (QC.QuickCheckMaxSize 10) $ testGroup "QuickCheck" 
  [ collectCmdNamesTest
  , runParserTest
  ]

collectCmdNamesTest :: TestTree
collectCmdNamesTest = QC.testProperty "Collected Command Names From Parser" collectCmdNamesProp

runParserTest :: TestTree
runParserTest = QC.testProperty "runParser gives back correct cmd" runParserProp

-- Tests that collecting all the toplevel command names works 
collectCmdNamesProp :: Property
collectCmdNamesProp = property $ \ptree' ->
    let 
        ptree :: ParserTree CmdName
        ptree = fmap (coerce @CmdName' @CmdName) ptree'

        collectedNames = sort $ collectTopLevelCmdNames $ fromParserTree ptree 
        cmdNames = sort $ getCmdNames ptree
    in 
      counterexample ("Collected names: " <> (show collectedNames))
    $ classify (cmdNames == []) ("cmdName == []")
    $ classify (length cmdNames > 2) ("cmdNames =" <> (show cmdNames))
    $ collectedNames == cmdNames

-- Tests that calling the toplevel parser with one of the
-- generated command names produces the same command name in the result. This ensures that
-- the optparse-applicative parser will work with the repline parser after we've appended
-- the command name to the list of arguments.
runParserProp  :: Property
runParserProp = property $ \ptree' ->
  let 
    ptree :: ParserTree CmdName
    ptree = fmap (coerce @CmdName' @CmdName) ptree'
    (parser, pickName) = fromParserTreeSelName ptree
  in forAll pickName $ \selectedCmdName ->
    let
      parsedCmd = selectedCmdName >>= runParser (testOptParser parser) . pure
    in 
      counterexample ("Parser returned: " <> (show parsedCmd))
    $ parsedCmd == selectedCmdName
