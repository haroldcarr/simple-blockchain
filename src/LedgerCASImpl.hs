{-# OPTIONS_GHC -Wno-unused-do-bind    #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}

module LedgerCASImpl
  ( Ledger
  , initLedger
  , ledgerContents
  , commitToLedger
  )
  where

import qualified Control.Concurrent                   as CC
import qualified Control.Monad                        as CM
import qualified Data.Atomics                         as A
import qualified Data.IORef                           as IOR
import qualified Data.Sequence                        as Seq
import           RIO
import qualified System.IO.Unsafe                     as SIOU
import qualified System.Random                        as Random
------------------------------------------------------------------------------
import           Config

newtype Ledger a = Ledger { _ledgerRef :: IOR.IORef (Seq.Seq a) }

initLedger
  :: IO (Ledger a)
initLedger = do
  r <- IOR.newIORef Seq.empty
  return $ Ledger r

ledgerContents
  :: Ledger a
  -> IO (Seq.Seq a)
ledgerContents = IOR.readIORef . _ledgerRef

commitToLedger
  :: (HasConfig env, HasLogFunc env)
  => env
  -> Ledger a
  -> a
  -> IO (Ledger a)
commitToLedger e l@(Ledger i) a = A.atomicModifyIORefCAS i $ \existing ->
  SIOU.unsafePerformIO $ do
    CM.when (cDOSEnabled (getConfig e)) $ do
      d <- Random.randomRIO (1,10::Int)
      CM.when (d == 10) $ do
        runRIO e $ logInfo "BEGIN commitToLedger DOS"
        CC.threadDelay (1000000 * cDOSDelay (getConfig e))
        runRIO e $ logInfo "END commitToLedger DOS"
    return (existing Seq.|> a, l)
