{-# LANGUAGE NoImplicitPrelude #-}

module BCSpec where

import           Test.Hspec
------------------------------------------------------------------------------
import           BC
import           GCoin (testIsValidCoinBase)

spec :: Spec
spec = do
  t01
  t02
  t03
  testMine
  testProofOfWork
  testEvidence
  testLongestChain1
  testLongestChain2
  testLongestChain3
  testLongestChainNegative
  testIsValidChain
  testIsValidBlock
  ----------------
  testIsValidTX
  testIsValidCoinBase isValidCoin addToChainBCSpec emptyChainBCSpec
  testMkUTXO1 addToChainBCSpec emptyChainBCSpec
  testMkUTXO2 addToChainBCSpec emptyChainBCSpec

