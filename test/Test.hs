{-|
Module      : Tests
Description : Tests for the modules

A few tests using QuickCheck and Tasty to make sure everything works
the way it's supposed to.
-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Protocol
import Networking

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [quickCheckTests]

quickCheckTests = testGroup "QuickCheck Tests" [
      QC.testProperty "parseCheck" parseCheck
    , QC.testProperty "sendCheck" sendCheck
    , QC.testProperty "lengthCheck" lengthCheck
    ]
