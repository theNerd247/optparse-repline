module Main where

import Arbitrary.Repline
import Data.Functor.Compose (Compose(..))
import Data.List (sort)
import Options.Applicative
import Options.Repline
import Test.QuickCheck
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

main = defaultMain tests

tests :: TestTree
tests = testGroup "Properties" [qcProperties]

qcProperties :: TestTree
qcProperties = testGroup "QuickCheck" 
  [ collectCmdNamesTest
  , runParserTest
  ]

collectCmdNamesTest :: TestTree
collectCmdNamesTest = QC.testProperty "Collected Command Names From Parser" collectCmdNamesProp

runParserTest :: TestTree
runParserTest = QC.testProperty "runParser gives back correct cmd" runParserProp

collectCmdNamesProp :: Property
collectCmdNamesProp = forAll arbitrarySubParserCmdNames $ \subParsersCmdNames ->
    let collectedNames = sort $ collectTopLevelCmdNames $ subParserTestParser subParsersCmdNames
        cmdNames = sort $ mconcat subParsersCmdNames
    in 
      counterexample ("Collected names: " <> (show collectedNames))
    $ collectedNames == cmdNames

-- Tests that calling the toplevel parser with one of the
-- generated command names produces the same command name in the result. This ensures that
-- the optparse-applicative parser will work with the repline parser after we've appended
-- the command name to the list of arguments.
runParserProp  :: Property
runParserProp = forAll (arbitrarySubParserCmdNames >>= withSelected . Compose) $ 
  \(subParsersCmdNames, selectedCmdName) ->
  let 
    parsedCmd = 
      selectedCmdName 
      >>= runParser 
        (testOptParser . subParserTestParser . getCompose $ subParsersCmdNames) 
        . pure
  in 
    counterexample ("Parser returned: " <> (show parsedCmd))
  $ parsedCmd == selectedCmdName
