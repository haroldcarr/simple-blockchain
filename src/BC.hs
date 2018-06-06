{-
Ledger
../examples/scenario-0/1-single-threaded-log.png
../examples/scenario-0/2-multi-threaded-log.png
../examples/scenario-0/3-multi-threaded-communication-log.png
../examples/scenario-0/4-multi-threaded-communication-ordered-log.png
../examples/scenario-0/5-distributed-log.png
../examples/scenario-0/6-smart-contract.png
-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module BC where

import qualified Control.Monad                        as CM
import qualified Crypto.Hash.SHA256                   as SHA
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Lazy                 as BSL
import qualified Data.ByteString.Builder              as BSB
import qualified Data.ByteString.Char8                as BSC8
import qualified Data.ByteString.Lazy.Char8           as BSLC8
import           Data.List                            ((\\), last)
import qualified Data.Hex                             as Hex
import qualified Data.IORef                           as IOR
import qualified Data.Map                             as M
import           Data.Monoid                          ((<>))
import qualified Data.Set                             as S
import qualified Data.Text                            as T
import qualified Network.HTTP.Types                   as H
import qualified Network.HTTP.Client                  as H
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified Prelude                              as P
import           Prelude                              ((!!))
import           Protolude                            hiding (hash)
import qualified System.Log.Logger                    as Log
import           Test.Hspec
------------------------------------------------------------------------------
import           Crypto -- for test
import           GCoin

debug = False
lBC   = "BC" :: P.String

------------------------------------------------------------------------------
data Block = Block
  { bPrevHash :: !BHash
  , bIndex    :: !BIndex
  , bTXs      :: !TXs
  , bProof    :: !Proof -- aka "nonce"
  } deriving (Eq, Read, Show)

type BHash       = BS.ByteString
type BIndex      = Int
type Transaction = BS.ByteString
type TXs         = [Transaction] -- TODO : Data.Sequence
type Proof       = Integer

genesisBlock :: Block
genesisBlock  = Block "1" 0 [] 100

------------------------------------------------------------------------------
data BCState = BCState
  { bcTXPool          :: !TXs
  , bcChain           :: !Chain
  , bcProofDifficulty :: !ProofDifficulty
  } deriving (Eq, Show)

type Chain           = [Block] -- TODO : Data.Sequence
type ProofDifficulty = Int

initialBCState :: BCState
initialBCState = BCState [] [genesisBlock] 4

-- stack test --test-arguments "-m t01-initialBCState"
t01 = describe "t01-initialBCState" $ it "has empty pool and only genesisBlock in chain" $
  initialBCState `shouldBe`
  BCState { bcTXPool = []
          , bcChain = [Block {bPrevHash = "1", bIndex = 0, bTXs = [], bProof = 100}]
          , bcProofDifficulty = 4}

------------------------------------------------------------------------------
-- ../examples/scenario-0/5-distributed-log.png
addTxToPool :: BCState -> Transaction -> BCState
addTxToPool s tx =
  if txInPoolOrChain tx s then s
  else s { bcTXPool = bcTXPool s ++ [tx] } -- TODO

-- stack test --test-arguments "-m t02-addTxToPool-initialBCState"
t02 = describe "t02-addTxToPool-initialBCState" $ it "adds new to pool" $
  addTxToPool (addTxToPool initialBCState "TX1") "TX2" `shouldBe`
  BCState { bcTXPool = ["TX1","TX2"]
          , bcChain = [Block {bPrevHash = "1", bIndex = 0, bTXs = [], bProof = 100}]
          , bcProofDifficulty = 4}

txInPoolOrChain :: Transaction -> BCState -> Bool
txInPoolOrChain tx = fst . searchPoolAndChain (==tx) "JUNK"

-- stack test --test-arguments "-m t03-addTxToPool"
t03 = describe "t03-addTxToPool" $ it "does not add duplicates to pool" $ do
  let s  = addTxToPool (addTxToPool initialBCState "TX1") "TX2"
  let s' = addTxToPool (addTxToPool s              "TX1") "TX3"
   in s' `shouldBe`
      BCState
      { bcTXPool = ["TX1","TX2","TX3"]
      , bcChain = [Block {bPrevHash = "1", bIndex = 0, bTXs = [], bProof = 100}]
      , bcProofDifficulty = 4}

searchPoolAndChain
  :: (Transaction -> Bool)
  -> Transaction -- fake "zero" TX
  -> BCState
  -> (Bool, Transaction)
searchPoolAndChain f tx s =
  let r@(b, _) = searchPool  f tx (bcTXPool s)
  in if b then r
     else        searchChain f tx (bcChain  s)

searchPool
  :: (Transaction -> Bool)
  -> Transaction -- fake "zero" TX
  -> TXs
  -> (Bool, Transaction)
searchPool f z = foldr (\tx txs -> if f tx then (True, tx) else txs)
                       (False, z)

searchChain
  :: (Transaction -> Bool)
  -> Transaction -- fake "zero" TX
  -> Chain
  -> (Bool, Transaction)
searchChain f z = foldr go (False, z)
 where
  go block blocks = let r@(b,_) = searchPool f z (bTXs block)
                     in if b then r else blocks

------------------------------------------------------------------------------
-- ../examples/scenario-0/5-distributed-log.png
mine :: BCState -> (BCState, Block)
mine s =
  let lastBlock = last (bcChain s) -- TODO
      pd        = bcProofDifficulty s
      proof     = proofOfWork pd lastBlock
      pHash     = hashBlock lastBlock
   in addBlock s pHash proof

-- | add new block containing all TXs in pool to Chain
addBlock :: BCState -> BHash -> Proof -> (BCState, Block)
addBlock s pHash proof =
  let b = Block pHash
                (length (bcChain s)) -- TODO
                (bcTXPool s)
                proof
      s' = s { bcTXPool = [], bcChain = bcChain s ++ [b] } -- TODO
   in (s', last (bcChain s')) -- TODO

-- stack test --test-arguments "-m testMine"
testMine = describe "testMine" $ do
  it "addBlock" $
    let (s, _) = mine (addTxToPool (addTxToPool initialBCState "TX1") "TX2") in
    s `shouldBe`
     BCState { bcTXPool = []
             , bcChain = [Block { bPrevHash = "1", bIndex = 0, bTXs = [], bProof = 100}
                         ,Block { bPrevHash = "\202\169KU\139\ETX\212\&3W\145`\229\224S\159\177\253\nF\167\158\227\250\255\244\v\207\228z\233\171\237"
                                , bIndex = 1
                                , bTXs = ["TX1","TX2"]
                                , bProof = 134530}]
             , bcProofDifficulty = 4}
  it "TXs in chain are not added to pool" $ do
    let (s, _) = mine (addTxToPool (addTxToPool initialBCState "TX1") "TX2")
    let  s'    =       addTxToPool (addTxToPool s              "TX1") "TX2"
    s' `shouldBe`
     BCState { bcTXPool = []
             , bcChain = [Block { bPrevHash = "1", bIndex = 0, bTXs = [], bProof = 100}
                         ,Block { bPrevHash = "\202\169KU\139\ETX\212\&3W\145`\229\224S\159\177\253\nF\167\158\227\250\255\244\v\207\228z\233\171\237"
                                , bIndex = 1
                                , bTXs = ["TX1","TX2"]
                                , bProof = 134530}]
             , bcProofDifficulty = 4}

------------------------------------------------------------------------------
-- | SHA-256 hash of a Block
hash :: BS.ByteString -> BHash
hash = SHA.hash

hashBlock :: Block -> BHash
hashBlock = BC.hash . BSC8.pack . show

------------------------------------------------------------------------------
-- | Proof of Work Algorithm:
--   - Find a number p' such that
--       hash(hash-last-block <> last-proof <> new-proof)
--     contains N leading zeroes (where N is "ProofDifficulty")
proofOfWork :: ProofDifficulty -> Block -> Proof
proofOfWork proofDifficulty lastBlock = foldr go 0 [0 .. ]
 where
  lastProof = bProof    lastBlock
  lastHash  = hashBlock lastBlock
  go newProof next =
    if validProof proofDifficulty lastHash lastProof newProof
    then newProof
    else next

(bcTX1,_) = mine (addTxToPool initialBCState "TX1")

-- stack test --test-arguments "-m proofOfWork"
testProofOfWork = describe "proofOfWork" $ do
  it "from genesisBlock" $
    proofOfWork 4 genesisBlock `shouldBe` bProof (bcChain bcTX1 !! 1) -- get last block
  it "from genesisBlock + 1" $
    proofOfWork 4 (bcChain bcTX1 !! 1) `shouldBe` 52668

-- | Validates the Proof
--   lastHash  : hash of previous block
--   lastProof : proof of previous block
--   proof     : new proof
validProof :: ProofDifficulty -> BHash -> Proof -> Proof -> Bool
validProof proofDifficulty lastHash  lastProof newProof =
 let guess = evidence lastHash lastProof newProof
  in BS.take proofDifficulty guess == BSC8.replicate proofDifficulty '0'

evidence :: BHash -> Proof -> Proof -> BHash
evidence lastHash lastProof newProof =
 let guess = BSC8.pack (show lastProof) <> BSC8.pack (show newProof) <> lastHash
 in Hex.hex (BC.hash guess)

-- stack test --test-arguments "-m evidence"
testEvidence = describe "evidence" $ do
  it "from genesisBlock" $
    evidence (hashBlock genesisBlock) (bProof genesisBlock) (proofOfWork 4 genesisBlock)
    `shouldBe`
    "0000DDF9FB09F9A9C5A0EF57DC3E2916633BEDB95B38D54BDBFFF0B7D4D6E515"
  it "from genesisBlock + 1" $
    evidence (hashBlock (bcChain bcTX1 !! 1)) (bProof (bcChain bcTX1 !! 1)) 52668
    `shouldBe`
    "000072456DC7CC3975C0CC3543B6BA201E6F4D056679970C3644D2DDEB4EEA67"

------------------------------------------------------------------------------
-- ../examples/scenario-0/5-distributed-log.png
-- | CONSENSUS ALGORITHM
--   Chooses longest chain in the network.
--   Returns (updated-environment, (True , ""))             if chain was replaced.
--   Returns (given-environment  , (False, ""))             if chain was NOT replaced.
--   Returns (given-environment  , (False, failure-reason)) if a chain was not valid
longestChain :: BCState -> [Chain] -> (BCState, (Bool, P.String))
longestChain s chains = go
 where
  go = let chain' = foldl (\a b -> if length a > length b then a else b) (bcChain s) chains
       in if bcChain s /= chain' then
            case isValidChain (bcProofDifficulty s) chain' of
              Right _  ->
                ( s { bcChain  = chain'
                    , bcTXPool = resolveTXs s chain'
                    }
                , (True, ""))
              Left err ->
                ( s , (False, "longestChain: invalid chain " <> T.unpack err))
          else  ( s , (False,  ""))
  txsInChain :: Chain -> TXs
  txsInChain = foldl (\txs b -> txs ++ bTXs b) []
  resolveTXs :: BCState -> Chain -> TXs
  resolveTXs myBCState theirChain =
    let myPool   = bcTXPool myBCState
        myTXs    = txsInChain (bcChain myBCState)
        theirTXs = txsInChain theirChain
    in (myPool \\ theirTXs) ++ -- remove TXs from my pool that are in their chain
       (myTXs  \\ theirTXs)    -- add TXs from my chain that are not in their chain

sTX0 :: BCState
(sTX0,_) = mine (addTxToPool initialBCState "TX-0")

-- stack test --test-arguments "-m longestChain1"
testLongestChain1 = describe "longestChain1" $ do
  it "found longer chain" $
    longestChain initialBCState  [bcChain sTX0]
    `shouldBe` (sTX0 , (True , ""))
  it "no    longer chain" $
    longestChain sTX0          [bcChain initialBCState]
    `shouldBe` (sTX0 , (False, ""))

eLongerChainAndPoolUpdateIn, e1LongerChainAndPoolUpdateOut :: BCState
eLongerChainAndPoolUpdateIn   = initialBCState { bcTXPool = ["TX-0","TX-should-stay"] }
e1LongerChainAndPoolUpdateOut = sTX0 { bcTXPool = ["TX-should-stay"] }

-- stack test --test-arguments "-m longestChain2"
testLongestChain2 = describe "longestChain2" $
  it "found longer chain and pool update" $
    longestChain eLongerChainAndPoolUpdateIn  [bcChain sTX0]
    `shouldBe` (e1LongerChainAndPoolUpdateOut, (True , ""))

s1NotLost :: BCState
(s1NotLost,_) = mine (addTxToPool (addTxToPool initialBCState "TX1") "TX2")

s2NotLost :: BCState
s2NotLost =
  let (etx1,_) = mine (addTxToPool initialBCState "TX1")
      (etx2,_) = mine (addTxToPool etx1           "TX3")
   in  etx2

s1NotLastAfterLongestChain :: BCState
s1NotLastAfterLongestChain = s2NotLost { bcTXPool = ["TX2"] }

-- stack test --test-arguments "-m longestChain3"
testLongestChain3 = describe "longestChain3" $ it "should not drop TX" $
  longestChain s1NotLost   [bcChain s2NotLost]
  `shouldBe` ( s1NotLastAfterLongestChain
             , (True, ""))

e1BadPHash :: BCState
e1BadPHash = sTX0 { bcChain = makeChain "X" 658 }

e1BadProof :: BCState
e1BadProof = sTX0 { bcChain = makeChain (hashBlock genesisBlock) 0 }

makeChain :: BHash -> Proof -> Chain
makeChain ph p =
  [ genesisBlock
  , Block { bPrevHash = ph
          , bIndex = 1, bTXs = ["TX-0"]
          , bProof = p}]

-- stack test --test-arguments "-m longestChainNegative"
testLongestChainNegative = describe "longestChainNegative" $ do
  it "invalid previous hash" $
    longestChain initialBCState  [bcChain e1BadPHash]
    `shouldBe` (initialBCState
               , (False, "longestChain: invalid chain invalid bPrevHash at 1"))
  it "invalid proof of work" $
    longestChain initialBCState  [bcChain e1BadProof]
    `shouldBe` (initialBCState
               , (False, "longestChain: invalid chain invalid bProof at 1"))

-- | Determine if a given blockchain is valid
isValidChain :: ProofDifficulty -> Chain -> Either T.Text ()
isValidChain pd bc = do
  CM.when (null bc)                                     (Left "empty blockchain")
  CM.when (length bc == 1 && P.head bc /= genesisBlock) (Left "invalid genesis block")
  -- `sequence_` causes function to return on/with first `Left` value
  sequence_ (map (isValidBlock pd) (P.zip3 [1 .. ] bc (P.tail bc)))
  return ()

-- stack test --test-arguments "-m isValidChain"
testIsValidChain = describe "isValidChain" $ do
  it "invalid empty" $
    isValidChain 4 [] `shouldBe` Left "empty blockchain"
  it "  valid genesis" $
    isValidChain 4 [genesisBlock] `shouldBe` Right ()
  it "invalid genesis" $
    let bg = genesisBlock { bIndex = 6 }
    in isValidChain 4 [bg] `shouldBe` Left "invalid genesis block"
  it "  valid sTX0" $
    isValidChain 4 (bcChain sTX0) `shouldBe` Right ()
  it "invalid previous hash" $
    isValidChain 4 (bcChain e1BadPHash) `shouldBe` Left "invalid bPrevHash at 1"
  it "invalid proof" $
    isValidChain 4 (bcChain e1BadProof) `shouldBe` Left "invalid bProof at 1"

-- | Given a valid previous block and a block to check.
--   Returns `Just ()` if valid.
--   Otherwise `Left reason`.
isValidBlock :: ProofDifficulty -> (Int, Block, Block) -> Either T.Text ()
isValidBlock pd (i, validBlock, checkBlock) = do
  CM.when   (hashBlock validBlock /= bPrevHash checkBlock)
            (Left ("invalid bPrevHash at " <> T.pack (show i)))
  CM.unless (validProof pd (bPrevHash checkBlock) (bProof validBlock) (bProof checkBlock))
            (Left ("invalid bProof at "    <> T.pack (show i)))
  return ()

-- stack test --test-arguments "-m isValidBlock"
testIsValidBlock = describe "isValidBlock" $ do
  it "  valid sTX0" $
    isValidBlock 4 (1, genesisBlock, bcChain sTX0 !! 1)
    `shouldBe` Right ()
  it "invalid previous hash" $
    isValidBlock 4 (1, genesisBlock, bcChain e1BadPHash !! 1)
    `shouldBe` Left "invalid bPrevHash at 1"
  it "invalid proof" $
    isValidBlock 4 (1, genesisBlock, bcChain e1BadProof !! 1)
    `shouldBe` Left "invalid bProof at 1"

------------------------------------------------------------------------------
-- IO

data IOState = IOState
  { ioBCState  :: !BCState
  , ioNodes    :: ![Address]
  , ioThisNode :: !Address
  } deriving (Eq, Show)

type Address = P.String
type IOEnv   = IOR.IORef IOState

{-
../examples/scenario-1/1-initial.png
./examples/scenario-1/0-start
./examples/scenario-1/1-initial
-}
initializeIOEnv :: Address -> IO IOEnv
initializeIOEnv thisNode =
  IOR.newIORef (IOState initialBCState [] thisNode)

getIOState :: (IOState -> a) -> IOEnv -> IO a
getIOState f = fmap f . IOR.readIORef

getBCState :: (BCState -> a) -> IOEnv -> IO a
getBCState f = fmap (f . ioBCState) . IOR.readIORef

setBCState :: IOState -> BCState -> IOState
setBCState i b = i { ioBCState = b }

{-
../examples/scenario-1/2-txs.png
./examples/scenario-1/2-txs
../examples/scenario-1/3-after-txs.png
./examples/scenario-1/3-after-txs
-}
addTxToPoolIO :: IOEnv -> Transaction -> IO ()
addTxToPoolIO env tx =
  atomicModifyIORef_ env $ \s ->
    setBCState s (addTxToPool (ioBCState s) tx)

{-
../examples/scenario-1/4-three-mines-block-3B1.png
./examples/scenario-1/4-three-mines-block-3B1
-}
-- | add new block containing all TXs in pool to Chain
mineIO :: IOEnv -> IO Block
mineIO env =
  IOR.atomicModifyIORef' env $ \s ->
    let (s',b) = mine (ioBCState s)
     in (setBCState s s', b)

{-
../examples/scenario-1/5-longest-chain-after-3B1.png
./examples/scenario-1/5-longest-chain-after-3B1
../examples/scenario-1/6-tx31-arrives.png
./examples/scenario-1/6-tx31-arrives
-}
longestChainIO :: IOEnv -> IO Bool
longestChainIO env = do
  nodes  <- getIOState ioNodes env
  chains <- CM.forM nodes $ \n -> do
    (status, body) <- httpRequest ("http://" <> n <> "/chain-only")
    return $ if status == 200
             then P.read (BSLC8.unpack body)
             else []
  (b, err) <- IOR.atomicModifyIORef' env $ \s ->
    let (s', be) = longestChain (ioBCState s) chains
     in (setBCState s s', be)
  if b then return b else do Log.infoM lBC err; return b

-- | Add a new node address to the list of nodes.
--   e.g., "192.168.0.5:5000"
registerNodeIO :: IOEnv -> Address -> IO ()
registerNodeIO env address =
  atomicModifyIORef_ env $ \s ->
    s { ioNodes = address:ioNodes s }

run :: [P.String] -> IO ()
run args = do
  Log.updateGlobalLogger lBC (Log.setLevel Log.DEBUG)
  let port = case args of
       ("-p":p:_) -> P.read p
       ("-h":_)   -> P.error "'-p', '--port', default=3001, 'port to listen on'"
       _          -> 3001
  env <- initializeIOEnv (show port)
  run' port env

run' :: Int -> IOEnv -> IO ()
run' httpPort env = do
  Log.infoM lBC ("starting httpServer on port " <> show httpPort)
  tn <- fmap ioThisNode (IOR.readIORef env)
  Wai.run httpPort $ Wai.logStdoutDev $
    \req s -> do
      CM.when debug $ Log.infoM lBC (tn <> " received request " <> show req)
      case Wai.rawPathInfo req of
        "/tx" -> -- POST
          case getQ req of
            Right tx -> do
              addTxToPoolIO env tx
              sendTxToPeers env tx
              send200 s tn ("/tx " <> show tx)
            Left x ->
              badQ s tn "/tx" x
        "/tx-no-forward" -> -- POST
          case getQ req of
            Right tx -> do
              addTxToPoolIO env tx
              send200 s tn ("/tx-no-forward " <> show tx)
            Left x ->
              badQ s tn "/tx-no-forward" x
        "/mine" -> do
          b <- fmap show (mineIO env)
          send200 s tn ("mine " <> b)
        "/longest-chain" -> do
          b <- longestChainIO env
          send200 s tn ("/longest-chain " <> show b)
        "/chain-only" -> do
          chain <- getBCState bcChain env
          send' s H.status200 (show chain)
        "/chain" -> do
          chain <- getBCState bcChain env
          send200 s tn ("chain " <> show (length chain) <> " " <> show chain)
        "/register" ->
          case getQ req of
            Right n -> do
              registerNodeIO env (BSC8.unpack n)
              send s tn H.status200 ("/register " <> show n)
            Left x ->
              badQ s tn "/register" x
        "/state" -> do
          st <- getBCState P.id env
          send200 s tn (show st)
        x ->
          send s tn H.status400 ("received unknown " <> BSC8.unpack x)
 where
  send200 s tn = send s tn H.status200
  send s tn sc r = send' s sc (tn <> " " <> r)
  send' s sc rsp = do
    CM.when debug $ Log.infoM lBC rsp
    s $ Wai.responseBuilder sc [] (BSB.byteString (BSC8.pack rsp))
  getQ r =
    case Wai.queryString r of ((q,_):_) -> Right q; x -> Left x
  badQ s tn msg q = do
    let rsp = tn <> " " <> msg <> " with bad query" <> show q
    CM.when debug $ Log.infoM lBC rsp
    send s tn H.status400 rsp

httpRequest :: P.String -> IO (Int, BSL.ByteString)
httpRequest url = do
  manager  <- H.newManager H.defaultManagerSettings
  request  <- H.parseRequest url
  response <- H.httpLbs request manager
  return ( H.statusCode (H.responseStatus response)
         , H.responseBody response )

sendTxToPeers :: IOEnv -> BS.ByteString -> IO ()
sendTxToPeers env tx = do
  nodes <- getIOState ioNodes env
  CM.forM_ nodes $ \n ->
    httpRequest ("http://" <> n <> "/tx-no-forward?" <> BSC8.unpack tx)

atomicModifyIORef_ :: IOR.IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ i f =
  IOR.atomicModifyIORef' i (\a -> (f a, ()))

{-
../examples/scenario-2/
./examples/scenario-2/0-start
./examples/scenario-2/1-initial
./examples/scenario-2/s2
-}

-- ===========================================================================
-- ./GCoin.hs

-- ===========================================================================
------------------------------------------------------------------------------

-- This only validates coin.  Does not prevent double spending.
isValidCoin
  :: BCState   -- TODO: this is BCState (instead of Chain) because of addToChain in tests
               --       could pass one more function to test to convert test view of chain
               --       to what is expected by the particular 'isValidCoin' impl
  -> PK        -- ^ creator public key
  -> SignedTX  -- ^ TX to verify
  -> Either Text ()
isValidCoin c = isValidCoinBase lookup
 where
  lookup :: STXHash -> Either Text SignedTX
  lookup stxHash =
    let (b, stx) = searchChain (\x -> stxHash == hashSignedTX (decodeSTX x)) "JUNK" (bcChain c)
     in if b then return (decodeSTX stx)
        else Left (isValidCoinErrMsg <> show stxHash)

-- for test
addToChain :: BCState -> [Transaction] -> BCState
addToChain = foldr (\tx bcs -> let (s,_) = mine (addTxToPool bcs tx) in s)

addToChainBCSpec :: BCState -> [SignedTX] -> BCState
addToChainBCSpec s = addToChain s . map encodeSTX

-- for test
emptyChainBCSpec = initialBCState

------------------------------------------------------------------------------
-- ../examples/scenario-gcoin/4-double-spend.png
-- This prevents double spending because it uses the blockchain.
-- But it is inefficient since it searches every time.
isValidTX :: BCState -> SignedTX -> Either Text ()
isValidTX s tx = case tx of
  (SignedTX (CreateCoin l u) _) -> do
    CM.when (ccInPool  u       (bcTXPool s)) $ Left ("CC already in pool: " <> l)
    CM.when (ccInChain u       (bcChain  s)) $ Left ("CC already in chain: "<> l)
    return ()
  (SignedTX (TransferCoin l stxHash _) _) -> do
    CM.when (tcInPool  stxHash (bcTXPool s)) $ Left ("TC already spent in pool: " <> l)
    CM.when (tcInChain stxHash (bcChain  s)) $ Left ("TC already spent in chain: "<> l)
    return ()
 where
  ccInPool  u         = fst . searchPool  (ccTest u) "JUNK"
  ccInChain u         = fst . searchChain (ccTest u) "JUNK"
  ccTest    u       x = case decodeSTX x of
    (SignedTX (CreateCoin _ u') _) -> u == u' -- SAME UUID
    _                              -> False
  -- does pool have TransferCoin containing same STXHash as given stx?
  tcInPool  stxHash   = fst . searchPool  (tcTest stxHash) "JUNK"
  -- does chain have STX whose STXHash field matches given STXHash?
  tcInChain stxHash   = fst . searchChain (tcTest stxHash) "JUNK"
  tcTest    stxHash x = case decodeSTX x of
    (SignedTX (TransferCoin _ stxHash' _) _) -> stxHash == stxHash' -- already spent
    _                                        -> False

-- stack test --test-arguments "-m isValidTX"
testIsValidTX = do
  u                  <- runIO createUUID
  (_PK, cSK)         <- runIO generatePKSKIO
  (aPK,_aSK)         <- runIO generatePKSKIO
  (bPK,_bSK)         <- runIO generatePKSKIO
  let Right cc        = createCoin   "cc"   u   cSK
  let Right cToA      = transferCoin "cToA" cc  cSK aPK
  let Right cToB      = transferCoin "cToB" cc  cSK bPK
  let sCCInPool       = addTxToPool initialBCState (encodeSTX cc)
  let (sCCInChain, _) = mine sCCInPool
  let sTCInPool       = addTxToPool sCCInChain (encodeSTX cToA)
  let (sTCInChain, _) = mine sTCInPool
  describe "isValidTX" $do
    it "  valid : CC not in pool nor chain" $
      isValidTX initialBCState cc `shouldBe` Right ()
    it "invalid : CC already pool" $
      isValidTX sCCInPool      cc `shouldBe` Left "CC already in pool: cc"
    it "invalid : CC already chain" $
      isValidTX sCCInChain     cc `shouldBe` Left "CC already in chain: cc"
    --------------------------------------------------
    it "  valid : TC not in pool nor chain" $
      isValidTX initialBCState cToA `shouldBe` Right ()
    it "invalid : TC already pool" $
      isValidTX sTCInPool      cToA `shouldBe` Left "TC already spent in pool: cToA"
    it "invalid : TC already chain" $
      isValidTX sTCInChain     cToB `shouldBe` Left "TC already spent in chain: cToB"

------------------------------------------------------------------------------
-- ../examples/scenario-gcoin/5-utxo1.png
mkUTXO :: Chain -> M.Map STXHash SignedTX
mkUTXO c = go l m
 where
  l = map decodeSTX (concatMap bTXs c)
  m = foldr (\stx a -> M.insert (hashSignedTX stx) stx a) M.empty l
  go    []  m0 = m0
  go (x:xs) m0 = case x of
    (SignedTX CreateCoin{}               _) -> go xs m0
    (SignedTX (TransferCoin _ stxHash _) _) -> go xs (M.delete stxHash m0)

-- stack test --test-arguments "-m mkUTXO1"
testMkUTXO1 addToChainX emptyChain = do
  u              <- runIO createUUID
  (_PK, cSK)     <- runIO generatePKSKIO
  (aPK, aSK)     <- runIO generatePKSKIO
  (bPK, bSK)     <- runIO generatePKSKIO
  let Right cc    = createCoin   "cc"    u     cSK
  let Right cToA  = transferCoin "cToA"  cc    cSK aPK
  let Right aToB1 = transferCoin "aToB1" cToA  aSK bPK
  let Right bToA  = transferCoin "bToA"  aToB1 bSK aPK
  let Right aToB2 = transferCoin "aToB2" bToA  aSK bPK
  let chain       = addToChainX emptyChain [cc, cToA, aToB1, bToA, aToB2]
  let chainBad    = addToChainX emptyChain     [cToA, aToB1, bToA, aToB2]
  describe "mkUTXO1" $ do
    it "empty" $
      mkUTXO (bcChain emptyChain) `shouldBe` M.empty
    it "chain" $
      map getLabel (M.elems (mkUTXO (bcChain chain)))    `shouldBe` ["aToB2"]
    -- this shows that it doesn't check validity (i.e., coins "rooted" in CreateCoin)
    -- it just checks what has not been spent
    it "chainBad" $
      map getLabel (M.elems (mkUTXO (bcChain chainBad))) `shouldBe` ["aToB2"]

{-
../examples/scenario-gcoin/6-utxo2.png
stack test --test-arguments "-m mkUTXO2"
-}
testMkUTXO2 addToChainX emptyChain = do
  (u1,u2,u3)
    <- runIO ((,,) <$> createUUID     <*> createUUID     <*> createUUID)
  ((_,c1SK),(_,c2SK),(_,c3SK))
    <- runIO ((,,) <$> generatePKSKIO <*> generatePKSKIO <*> generatePKSKIO)
  ((aPK,aSK),(bPK,bSK),(jPK,_))
    <- runIO ((,,) <$> generatePKSKIO <*> generatePKSKIO <*> generatePKSKIO)
  let Right c1     = createCoin   "c1" u1 c1SK
  let Right c2     = createCoin   "c2" u2 c2SK
  let Right c3     = createCoin   "c3" u3 c3SK
  let Right c1ToA  = transferCoin "c1ToA"  c1     c1SK aPK
  let Right c1AToB = transferCoin "c1AToB" c1ToA  aSK  bPK
  let Right c1BToA = transferCoin "c1BToA" c1AToB bSK  aPK
  let Right c2ToA  = transferCoin "c2ToA"  c2     c2SK aPK
  let Right c2AToJ = transferCoin "c2AToJ" c2ToA  aSK  jPK
  let chain        = addToChainX emptyChain
                     [c1,c2,c3,c1ToA,c1AToB,c1BToA,c1BToA,c2ToA,c2AToJ]
  describe "mkUTXO2" $
    it "chain" $
      S.fromList (map getLabel (M.elems (mkUTXO (bcChain chain))))
      `shouldBe` S.fromList ["c1BToA", "c2AToJ", "c3"]

------------------------------------------------------------------------------
-- TODO : ensure UTXO are rooted in CreateCoin
-- TODO : hook up to BC operation as "Smart Contract"
-- TODO : alternate smart contract (e.g., key/value store)
-- ../examples/scenario-0/6-smart-contract.png
-- stack test

