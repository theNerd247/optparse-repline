module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Options.Repline


main = defaultMain tests

tests :: TestTree
tests = testGroup "Properties" [qcProperties]

qcProperties :: TestTree
qcProperties = testGroup "QuickCheck" 
  [ 
  ]
