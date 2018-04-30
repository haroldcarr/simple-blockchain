{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans            #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PackageImports     #-}
{-# LANGUAGE StandaloneDeriving #-}

module GCoin where

import qualified "cryptonite" Crypto.Hash as H
import qualified Crypto.PubKey.RSA        as RSA
import qualified Data.ByteArray           as BA
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.Either.Combinators  as E
import qualified Data.Map.Strict          as M
import qualified Data.Serialize           as S
import qualified Data.UUID                as U
import qualified Data.UUID.V4             as U
import qualified Prelude
import           Test.Hspec               as HS
import           Universum
------------------------------------------------------------------------------
import           Crypto

-- Hlint complain about DeriveAnyClass, but it is needed to S.Serialize RSA.PublicKey
{-# ANN module ("HLint: ignore Unused LANGUAGE pragma"::Prelude.String) #-}

newtype UUID    = UUID    { getUuid    :: ByteString } deriving (Eq, Generic, Show)
newtype STXHash = STXHash { getSTXHash :: ByteString } deriving (Eq, Generic, Ord, Show)
newtype Hash    = Hash    { getHash    :: ByteString } deriving (Generic, Show)
instance S.Serialize UUID
instance S.Serialize STXHash
instance S.Serialize Hash
deriving instance Generic     RSA.PublicKey
deriving instance S.Serialize RSA.PublicKey

data TX
  = CreateCoin UUID
  | TransferCoin STXHash RSA.PublicKey
  deriving (Generic, Show)
instance S.Serialize TX

data SignedTX = SignedTX
  { sTX  :: TX
  , sSig :: Signature
  } deriving (Generic, Show)
instance S.Serialize SignedTX

createUUID :: IO UUID
createUUID = do
  u <- U.nextRandom
  return (UUID (BSL.toStrict (U.toByteString u)))

signTX :: RSA.PrivateKey -> TX -> Either RSA.Error SignedTX
signTX sk tx = E.mapRight (SignedTX tx) (signMsg sk (Msg (S.encode tx)))

verifyTXSig :: RSA.PublicKey -> SignedTX -> Bool
verifyTXSig pk stx = case stx of
  (SignedTX tx@(CreateCoin _)     sig) -> v tx sig
  (SignedTX tx@(TransferCoin _ _) sig) -> v tx sig
 where v tx = verifyMsgSig pk (Msg (S.encode tx))

createCoinIO :: RSA.PrivateKey -> IO (Either RSA.Error SignedTX)
createCoinIO sk = do
  u <- createUUID
  return (createCoin u sk)

createCoin :: UUID -> RSA.PrivateKey -> Either RSA.Error SignedTX
createCoin u sk = signTX sk (CreateCoin u)

transferCoin :: SignedTX -> RSA.PrivateKey -> RSA.PublicKey -> Either RSA.Error SignedTX
transferCoin fromCoin ownerSK toPK = do
  let fromHash = hashSignedTX fromCoin
      toCoin   = TransferCoin fromHash toPK
  signTX ownerSK toCoin

hashSignedTX :: SignedTX -> STXHash
hashSignedTX = STXHash . getHash . hash . encodeSTX

hash :: BS.ByteString -> Hash
hash = Hash . BA.convert . H.hashWith H.SHA256

encodeSTX :: SignedTX -> ByteString
encodeSTX = S.encode

decodeSTX :: ByteString -> SignedTX
decodeSTX bs = case S.decode bs of
  Right s -> s
  _       -> error "decodeSignedTX" -- TODO

testIt = do
  u                      <- runIO createUUID
  (creatorPK, creatorSK) <- runIO generatePKSKIO
  (alicePK  , aliceSK)   <- runIO generatePKSKIO
  (bobPK    , bobSK)     <- runIO generatePKSKIO
  (jimPK    , _jimSK)    <- runIO generatePKSKIO
  let Right cc    = createCoin   u     creatorSK
  let Right cToA  = transferCoin cc    creatorSK alicePK
  let Right aToB1 = transferCoin cToA  aliceSK   bobPK
  let Right aToJ  = transferCoin cToA  aliceSK   jimPK
  let Right bToA  = transferCoin aToB1 bobSK     alicePK
  let Right aToB2 = transferCoin bToA  aliceSK   bobPK
  let chain       = addToChain M.empty [cc, cToA, aToB1, bToA]
  let chainBad    = addToChain M.empty     [cToA, aToB1, bToA]
  describe "GoofyCoin" $ do
    it "isValidCoin createCoin" $
      isValidCoin creatorPK M.empty cc `shouldBe` Right True
    it "isValidCoin chain1 aToJ" $
      isValidCoin creatorPK chain aToJ `shouldBe` Right True
    it "isValidCoin chain1 aToJ (double spend)" $
      isValidCoin creatorPK chain aToB1 `shouldBe` Right True
    it "isValidCoin bad sig" $
      isValidCoin creatorPK M.empty (cc { sSig = Signature "BAD" }) `shouldBe` Right False
    it "isValidCoin aToB2" $
      isValidCoin creatorPK chain aToB2 `shouldBe` Right True
    it "isValidCoin chainBad aToB2" $
      isValidCoin creatorPK chainBad aToB2 `shouldBe`
      Left ("No TX for : " <> show ((\(TransferCoin h _) -> h) (sTX cToA)))
 where
  addToChain :: M.Map STXHash SignedTX -> [SignedTX] -> M.Map STXHash SignedTX
  addToChain = foldr (\x cc -> M.insert (hashSignedTX x) x cc)
  isValidCoin
    :: RSA.PublicKey          -- ^ creator public key
    -> M.Map STXHash SignedTX  -- ^ the "chain"
    -> SignedTX               -- ^ TX to verify
    -> Either Text Bool
  isValidCoin cpk m stx = case stx of
    (SignedTX (CreateCoin _) _) ->
      return $ verifyTXSig cpk stx
    (SignedTX (TransferCoin txh _) _) ->
      case M.lookup txh m of
        Nothing ->
          Left ("No TX for : " <> show txh)
        Just   cc@(SignedTX (CreateCoin _) _) ->
          isValidCoin cpk m cc
        Just next@(SignedTX (TransferCoin _ opk) _) ->
           if verifyTXSig opk stx
           then isValidCoin cpk m next
           else return False
