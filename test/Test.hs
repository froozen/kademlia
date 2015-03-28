{-|
Module      : Tests
Description : Tests for the modules

A few tests using QuickCheck to make sure everything works
the way it's supposed to.
-}

module Test where

import Protocol
import Networking

import Test.QuickCheck as Q
import Distribution.TestSuite as TS

toTSResult :: Q.Result -> TS.Result
toTSResult Q.Success {} = TS.Pass
toTSResult Q.GaveUp {} = TS.Fail "Gave up"
toTSResult Q.Failure {reason=qReason} = TS.Fail qReason

runQuickCheck :: Q.Testable p => p -> IO TS.Progress
runQuickCheck prop = do
    qres <- Q.quickCheckWithResult Q.stdArgs prop
    return $ Finished . toTSResult $ qres

tests :: IO [Test]
tests = return [ Test $ TestInstance (runQuickCheck parseCheck)
                                      "parseCheck" [] [] undefined
               , Test $ TestInstance (runQuickCheck sendCheck)
                                      "sendCheck" [] [] undefined
               , Test $ TestInstance (runQuickCheck lengthCheck)
                                      "lengthCheck" [] [] undefined
               ]
