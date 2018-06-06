module GCoinSpec where

import           Test.Hspec
------------------------------------------------------------------------------
import           GCoin

spec :: Spec
spec = do
  testCreateCoin
  testVerifyCreatedCoin
  testTransferCoin
  testIsValidCoin
  testIsValidCoinBase isValidCoinForTest addToChainForTest emptyChainForTest
