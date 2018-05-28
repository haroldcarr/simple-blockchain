module GCoinSpec where

import           Test.Hspec
------------------------------------------------------------------------------
import           GCoin

spec :: Spec
spec = do
  testCreateCoin
  testVerifyCreatedCoin
  testTransferVerifyCoin
  testIsValidCoin isValidCoinForTest addToChainForTest emptyChainForTest
