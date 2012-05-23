{-# LANGUAGE GADTs #-}

-- | BCross is the implementation of cross for B and BCX.
--
-- It also contains types associated with partitions and
-- crossB, i.e. used in BCX.
module Sirea.Internal.BCross 
    ( crossB
    , GobStopper(..)
    , TC(..)
    , tcToStepper
    ) where

import Data.Typeable
import Data.Function (fix)
import Data.IORef
import Control.Applicative
import Control.Exception (mask_, assert)
import Sirea.Internal.BTypes
import Sirea.Internal.STypes
import Sirea.Internal.LTypes
import Sirea.Internal.BImpl (fwdB, phaseUpdateB)
import Sirea.Partition
import Sirea.PCX
import Sirea.Behavior

-- crossB will:
--    add the update as a task to perform in another partition.
--    allow updates on input to buffer and batch (bounded buffer)
-- crossB is eliminated if typeOf p1 == typeOf p2
--
crossB :: (Partition p1, Partition p2) => PCX w -> B w (S p1 x) (S p2 x)
crossB cw = fix $ \ b ->
    let (p1,p2) = getPartitions b in
    if (typeOf p1 == typeOf p2) then jumpB else
    sendB cw p1 p2 >>> receiveB cw p2

-- this is a total hack in haskell to access type system data
getPartitions :: B w (S p1 x) (S p2 x) -> (p1,p2)
getPartitions _ = (undefined,undefined)

-- jumpB is used if we're moving to the same partition we started in
-- It simply ignores the partitions and forwards the signal. Jump is
-- free after compilation.
jumpB :: B w (S p1 x) (S p2 x)
jumpB = B_mkLnk tr_fwd lnkJump
    where lnkJump = MkLnk { ln_build = return . fnJump
                          , ln_tsen = False, ln_peek = 0 }

fnJump :: Lnk (S p a) -> Lnk (S p' a) 
fnJump LnkDead = LnkDead
fnJump (LnkSig lu) = LnkSig lu

-- sendB will actually delay the output from p1 to p2.
-- signal updates are processed in batches.
sendB :: (Partition p1, Partition p2) => PCX w -> p1 -> p2 -> B w (S p1 x) (S p2 x)
sendB cw p1 p2 = B_mkLnk tr_fwd lnkSend
    where lnkSend = MkLnk { ln_build = buildSendLnk cw p1 p2
                          , ln_tsen  = False
                          , ln_peek  = 0    }


buildSendLnk :: (Partition p1, Partition p2) => PCX w -> p1 -> p2 
             -> Lnk (S p2 x) -> IO (Lnk (S p1 x))  
buildSendLnk _ _ _ LnkDead = return LnkDead
buildSendLnk cw p1 p2 (LnkSig lu) =  
    initPartition p1 cw >> -- ensure p1 exists
    initPartition p2 cw >> -- ensure p2 exists
    let doSend = mkTCSender cw p1 p2 in
    -- sender is: IO () -> IO ()
    -- the IO
    -- when we receive an update, we'll want to
    -- prepare `send` element in p1 for sending to p2
    let c1 = getTC p1 cw in
    let c2 = getTC p2 cw in
    --
    undefined

-- receiveB is used on the receive side of bcross. Serves two roles:
--   * combines multiple updates to one signal
--   * touches output to efficiently combine multiple receives
receiveB :: (Partition p) => PCX w -> p -> B w (S p x) (S p x)
receiveB cw p = phaseUpdateB $ addTCWork $ getTC p cw 

getTC :: (Typeable p) => p -> PCX w -> TC p
getTC _ = findInPCX

getPCX :: (Typeable p) => p -> PCX w -> PCX p
getPCX _ = findInPCX

-- | the GobStopper is collection of all the Stopper values in an
-- application; it is used to shut down the individual partitions
-- during graceful shutdown. 
newtype GobStopper = Gob { unGob :: IORef [Stopper] }
instance Typeable GobStopper where
    typeOf _ = mkTyConApp tycGS []
        where tycGS = mkTyCon3 "Sirea" "Partition.Internal" "GobStopper"
instance Resource GobStopper where
    locateResource _ = Gob <$> newIORef []

-- initPartition is performed when building behaviors (or dynamic 
-- behaviors) when we cross into another partition. It ensures the
-- partition exists before we step into it. Idempotent.
initPartition :: (Partition p) => p -> PCX w -> IO ()
initPartition p cw =
    let tc = getTC p cw in
    atomicModifyIORef (tc_init tc) (\x->(True,x)) >>= \ bInit ->
    if bInit then return () else
    let cp = getPCX p cw in
    newPartitionThread cp (tcToStepper tc) >>= \ stopper ->
    let gs = unGob $ findInPCX cw in
    atomicModifyIORef gs (\stoppers->(stopper:stoppers,()))
    

-- | TC is the thread context, which is basically a couple IORefs 
-- with some metadata about the thread. 
--    tc_init :: for initialization; atomic
--    tc_recv :: either event or work; atomic
--    tc_work :: phased tasks, repeats until empty; not atomic
--    tc_send :: tasks to perform at end of round; not atomic
--
-- These are not heavily optimized; they don't need to be, since
-- there are a bounded number of tasks in any queue at once, and
-- received operations are grouped in batches.
data TC p = TC 
    { tc_init :: IORef Bool
    , tc_recv :: IORef (Either Event Work)
    , tc_work :: IORef (Maybe Work)
    , tc_send :: IORef TCSend
    }
type Event = IO ()
type Work = IO ()

instance Typeable1 TC where
    typeOf1 _ = mkTyConApp tycTC []
        where tycTC = mkTyCon3 "Sirea" "Partition.Internal" "TC"
instance (Typeable p) => Resource (TC p) where
    locateResource _ = newTC

newTC :: IO (TC p)
newTC = TC <$> newIORef False
           <*> newIORef (Left (return ()))
           <*> newIORef Nothing
           <*> newIORef tcsend_zero

-- | In each runStepper round:
--    recv tasks are emtied (atomically) then processed
--    work tasks are created by recv, then handled in group
--       will repeat so long as work exists, but usually just one
--    send tasks performed at end of stepper round.
-- Note that this means a finite amount of work is performed in any
-- given round, since recv is not re-emptied. S
tcToStepper :: TC p -> Stepper
tcToStepper tc = Stepper 
    { runStepper = runTCStep tc
    , addStepperEvent = addTCEvent tc
    }
 
runTCStep :: TC p -> IO ()
runTCStep tc = mask_ $
    runTCRecv (tc_recv tc) >>
    runTCWork (tc_work tc) >>
    runTCSend (tc_send tc)

-- TCRecv has either event or work.
runTCRecv :: IORef (Either Event Work) -> IO ()
runTCRecv rfRecv =
    atomicModifyIORef rfRecv swapZero >>=
    either ignoreEvent performWork
    where swapZero x = (Left (return ()),x)
          ignoreEvent _ = return ()
          performWork work = work

-- TCWork will execute multiple phases.
-- Usually there is only one phase.
runTCWork :: IORef (Maybe Work) -> IO ()
runTCWork rfw = 
    readIORef rfw >>= \ mbWork ->
    writeIORef rfw Nothing >>
    maybe done doWork mbWork
    where doWork work = work >> runTCWork rfw
          done = return ()

addTCRecv :: TC p -> Work -> IO ()
addTCRecv tc op = atomicModifyIORef (tc_recv tc) putOpTakeEvent >>= id
    where putOpTakeEvent (Left event) = (Right op, event)
          putOpTakeEvent (Right work) = (Right (work >> op), return ())

addTCEvent :: TC p -> Event -> IO ()
addTCEvent tc ev =
    atomicModifyIORef (tc_recv tc) addOrExecEvent >>= id
    where addOrExecEvent (Left event) = (Left (ev >> event), return ())
          addOrExecEvent (Right work) = (Right work, ev)

-- work is not modified atomically.
addTCWork :: TC p -> Work -> IO ()
addTCWork tc newWork = modifyIORef (tc_work tc) addWork
    where addWork Nothing = Just newWork
          addWork (Just oldWork) = Just (oldWork >> newWork)


-- | TCSend - outbox for a Sirea thread.
-- 
-- Updates to each remote thread are grouped, batch per partition.
-- To control number of batches in-flight, using a semaphore for 
-- each destination. This control prevents starvation of threads;
-- if one thread cannot keep up with inputs, will wait on it.
--
-- 
-- 
data TCSend = TCSend
    {
    }
tcsend_zero :: TCSend
tcsend_zero = TCSend

mkTCSender :: (Partition p1, Partition p2) 
           => PCX w -> p1 -> p2 -> Work -> IO ()
mkTCSender cw p1 p2 =
    assert (typeOf p1 /= typeOf p2) $
    error "TODO: Build sender"

runTCSend :: IORef TCSend -> IO ()
runTCSend = error "TODO: Run sender"




