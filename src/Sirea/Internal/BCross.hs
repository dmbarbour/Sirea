{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances,
             DeriveDataTypeable
  #-}

-- | BCross is the implementation of cross for B. To avoid cyclic
-- module dependencies, it is represented as an unwrapped B type.
--
-- Sirea makes a promise called "snapshot consistency", which means
-- that partitions operate on a frozen picture of other partitions.
-- This isn't global consistency, since it is only pairwise between
-- partitions. But it does eliminate most malign glitches common to
-- naive implementations of concurrent reactive models. 
--
-- To achieve this consistency, a batching mechanism is used between
-- partitions. Updates for a remote partition from a given step are
-- delivered only when the step is complete. This is modeled via an
-- outbox between any two partitions, managed by `bcross` behaviors.
--
-- BCross is a potential point of choke for updates. In Sirea, every 
-- update has an arrive-by date. If that date is far in the future by
-- many seconds, then it could be delayed a little. For snapshot 
-- consistency, a whole batch could be delayed, or nothing. However,
-- the whole-batch choke isn't very robust to extension; developers
-- shouldn't depend on it, so I've decided to eliminate it. BCross
-- will, however, drop insignificant stability updates (less than a
-- heartbeat) to avoid trivial activations.
--
-- BCross uses a bounded buffer - a limited number of batches may be
-- "in flight" between any two partitions. This limit helps ensure 
-- Sirea's soft-realtime and soft-bounded memory properties. The
-- receiving partition will drain the full buffer at runStepper, so
-- there is no risk of cyclic waits.
--
-- On receipt, it is possible for multiple batches to piggyback by
-- eliminating defunct signal updates and combining touches. This
-- helps regulate performance: a partition that falls behind becomes
-- more efficient, enabling it to catch back up.
-- 
module Sirea.Internal.BCross 
    ( crossB0
    , phaseUpdateB0
    , GobStopper(..)
    , runGobStopper
    ) where

import Prelude hiding (cycle)

import Data.Typeable
import Data.Function (fix)
import Data.IORef
import Control.Applicative
import Control.Exception (assert)
import Control.Concurrent.MVar 
import Control.Monad (join, unless, when)

import Sirea.Internal.B0Type
import Sirea.Internal.B0Impl()
import Sirea.Internal.STypes
import Sirea.Internal.LTypes
import Sirea.Internal.PTypes
import Sirea.Internal.PulseSensor (initPulseListener)
import Sirea.Internal.Tuning ( batchesInFlight, tAncient)
import Sirea.Partition
import Sirea.PCX
import Sirea.Behavior
import Sirea.Signal
import Sirea.Time

type Event = IO ()
type Work = IO ()

-- crossB will:
--    add the update as a task to perform in another partition.
--    allow updates on input to buffer and batch (bounded buffer)
-- crossB is eliminated if typeOf p1 == typeOf p2
--
crossB0 :: (Partition p1, Partition p2) => PCX W -> B0 IO (S p1 x) (S p2 x)
crossB0 cw = fix $ \ b ->
    let (p1,p2) = getPartitions b in
    if (typeOf p1 == typeOf p2) -- are we sitting still
      then falseCrossB0
      else sendB0 cw p1 p2 >>> phaseUpdateB0 

-- falseCross is called when crossB start and stop partitions are
-- the same. Basically, it is an id function. 
falseCrossB0 :: (Monad m) => B0 m (S p1 x) (S p2 x)
falseCrossB0 = B0_mkLnk falseCross (const (return . falseCross))

falseCross :: LnkW s (S p x) -> LnkW s (S p' x)
falseCross (LnkSig sa) = LnkSig sa
falseCross ln = assert (ln_dead ln) LnkDead

-- this is a total hack in haskell to access typesystem data
getPartitions :: (Partition p1, Partition p2) 
              => B0 IO (S p1 x) (S p2 x) -> (p1,p2)
getPartitions _ = (undefined,undefined)


-- phaseUpdateB0 is the receiver-side for `bcross`. Multiple updates
-- may be received due to multiple batches in-flight (i.e. when slow
-- consumer receives from fast sender). phaseUpdateB0 causes all the
-- updates to "piggyback" into one larger update, which is processed
-- with greater efficiency (due to eliminating rework) compared to
-- processing the updates independently. This should result in subtle
-- self-regulation of partitions for fairness. I've not measured the 
-- impact, and bounded buffers have a much stronger effect.
--
phaseUpdateB0 :: (Monad m) => B0 m (S p x) (S p x)
phaseUpdateB0 = B0_mkLnk id mkLnPhase

mkLnPhase :: (Monad m) => LCapsM m (S p x) -> LnkM m (S p x) -> m (LnkM m (S p x))
mkLnPhase (LnkSig (LCX lc)) (LnkSig lu) =
    let cc = lc_cc lc in    
    cc_getSched cc >>= \ pd ->
    cc_newRef cc NoUpdate >>= \ rfSu ->
    let lu' = mkLuPhase rfSu pd lu in 
    return (LnkSig lu')
mkLnPhase _ _ = return LnkDead

data UpdateRecord a 
    = NoUpdate 
    | IdleUpdate !StableT 
    | FullUpdate !StableT !T !(Sig a)

mkLuPhase :: (Monad m) => Ref m (UpdateRecord a) 
          -> Sched m -> LnkUpM m a -> LnkUpM m a
mkLuPhase rf pd lu = LnkUp touch update idle cycle where
    touch = return () -- touch happens on received update
    cycle _ = return () -- no cycles can cross partitions
    maybeSchedule NoUpdate = 
        -- first 'maybeSchedule' op in step
        onUpdPhase pd deliver >> 
        ln_touch lu
    maybeSchedule _ = return ()
    deliver =
        readRef rf >>= \ rec ->
        writeRef rf NoUpdate >>
        case rec of
            NoUpdate -> error "unexpected update record state!"
            IdleUpdate tS -> ln_idle lu tS
            FullUpdate tS tU su -> ln_update lu tS tU su
    idle tS =
        readRef rf >>= \ rec ->
        maybeSchedule rec >>
        let rec' = case rec of
                FullUpdate _ tU su -> FullUpdate tS tU su
                _ -> IdleUpdate tS
        in
        writeRef' rf rec'
    update tS tU su =
        readRef rf >>= \ rec ->
        maybeSchedule rec >>
        let rec' = case rec of
                FullUpdate _ tU0 su0 ->
                    if (tU0 >= tU) 
                        then FullUpdate tS tU su 
                        else FullUpdate tS tU0 (s_switch' su0 tU su)
                _ -> FullUpdate tS tU su
        in
        writeRef' rf rec'


-- sendB0, if active (not dead code), is the output port between
-- partitions. It manages mailboxes, one for each remote partition.
-- All updates computed in a step will be added to the same box as
-- one 'batch' to be delivered at the end of the current step. Some
-- insignificant stability updates may be dropped. 
--
-- Partitions will be created if they don't already exist.
--
sendB0 :: (Partition p1, Partition p2) 
      => PCX W -> p1 -> p2 -> B0 IO (S p1 x) (S p2 x)
sendB0 cw p1 p2 = B0_mkLnk lcSend lnSend where
    lcSend LnkDead = LnkDead
    lcSend (LnkSig (LCX lc)) =
        -- primarily just need to change schedulers
        let getSched' = getPCX p2 cw >>= getPSched in
        let cc' = CC { cc_getSched = getSched', cc_newRef = newRefIO } in
        let lc' = lc { lc_cc = cc' } in
        LnkSig (LCX lc')
    lnSend _ LnkDead = return LnkDead
    lnSend _ (LnkSig lu) = 
        initPartition p1 cw >>
        initPartition p2 cw >>
        getOBSend cw p1 p2 >>= \ obSend ->
        newIORef (StableT tAncient) >>= \ rfT ->
        let lu' = luSend rfT obSend lu in
        return (LnkSig lu')

luSend :: IORef StableT -> OBSend -> LnkUp x -> LnkUp x
luSend rfT obSend lu = LnkUp touch update idle cycle where
    touch = return () -- touches stop at partition boundary
    cycle _ = return () -- cycles are partition-local
    idle tS =
        readIORef rfT >>= \ tS0 ->
        unless (tS0 == tS) $
            writeIORef rfT tS >>
            obSend (ln_idle lu tS)
    update tS tU su =
        tS `seq` writeIORef rfT tS >>
        obSend (ln_update lu tS tU su)

getTC :: (Partition p) => PCX p -> IO TC
getTC = findInPCX 

getPCX :: (Partition p) => p -> PCX W -> IO (PCX p)
getPCX _ = findInPCX

-- | the GobStopper is collection of all the Stopper values in an
-- application; it is used to shut down the individual partitions
-- during graceful shutdown. (The P0 Stopper is excluded.) 
newtype GobStopper = Gob { unGob :: IORef [Stopper] }
    deriving (Typeable)

instance Resource W GobStopper where
    locateResource _ _ = Gob <$> newIORef []

-- | runGobStopper will:
--     halt all active threads (at instant of runGobStopper)
--     register an event to call when all threads fully halt.
-- If new stoppers are later added, they'll be missed. Usually
-- this would be called only at shutdown, when nothing new should
-- or could be created.
runGobStopper :: GobStopper -> IO () -> IO ()
runGobStopper gs ev =
    readIORef (unGob gs) >>= \ lStoppers ->
    if (null lStoppers) then ev else
    newIORef (length lStoppers) >>= \ rfCD ->
    let onStop = readIORef rfCD >>= \ cd ->
                 writeIORef rfCD (cd - 1) >>
                 when (0 == cd) ev
    in 
    mapM_ (flip addStopperEvent onStop) lStoppers >>
    mapM_ runStopper lStoppers

-- initPartition is performed when building behaviors (or dynamic 
-- behaviors) when we cross into another partition. It ensures the
-- partition exists before we step into it. Idempotent and atomic.
-- (Must be atomic if dynamic behaviors may declare partitions).
initPartition :: (Partition p) => p -> PCX W -> IO ()
initPartition p cw =
    getPCX p cw >>= \ cp ->
    getTC cp >>= \ tc ->
    atomicModifyIORef (tc_init tc) (\x->(True,x)) >>= \ bInit ->
    unless bInit $ 
        newPartitionThread cp (tcToStepper tc) >>= \ s0 ->
        let stopInStepper = addTCRecv tc (runStopper s0) in
        let stopper = s0 { runStopper = stopInStepper } in 
        getPCX0 cw >>= \ cp0 ->
        initPulseListener cp0 cp >>
        unGob <$> getGS cw >>= \ gs ->
        atomicModifyIORef gs (\stoppers->(stopper:stoppers,()))
        -- TODO: add support to receive pulse.

getPCX0 :: PCX W -> IO (PCX P0) 
getPCX0 = findInPCX

getGS :: PCX W -> IO (GobStopper)
getGS cw = findInPCX cw

-- | OutBox - output from Sirea thread.
--
-- * Each partition has one outbox for each destination partititon.
-- * The `TC` object has one task for all non-empty outboxes.
-- * At the end of each round, all outboxes are emptied.
-- * Semaphore for bounded-buffer parallelism.
--
-- Partition threads should communicate only by use of RDP behavior.
-- Other forms of waits - e.g. communicating MVars - could result in
-- deadlock due to the bounded buffers. Bounds control resource use
-- and improve fairness.
--
-- Outboxes are modeled as resources under the sender's PCX. It is
-- manipulated single-threaded by the sender's thread, except for a
-- semaphore. Work to perform remotely is simply queued as an IO
-- task. Releasing the semaphore is added as a task to each batch, to
-- model the bounded buffer.
-- 
data OutBox p = OutBox 
    { ob_sem   :: !Semaphore -- bounds number of in-flight batches.
    , ob_state :: !(IORef [Work])
    } deriving (Typeable)

instance (Partition p0, Partition p) => Resource p0 (OutBox p) where
    locateResource _ _ = OutBox <$> newSemaphore batchesInFlight
                                <*> newIORef []

-- obSend - send work FROM p1 TO p2 (given the world PCX).
--   (not mt-safe, should be called only from p1's thread.)
--
-- obSend also becomes a choke-point for cyclic relationships. This
-- is achieved at the whole-batch level: if every update in a batch
-- may be delayed, then the whole batch is delayed. (Delaying at a
-- batch level enables  
--
type OBSend = Work -> IO ()
getOBSend :: (Partition p1, Partition p2) => PCX W -> p1 -> p2 -> IO OBSend
getOBSend cw p1 p2 = getOBSend' <$> tc1 <*> ob <*> tc2 where    
    tc1 = getPCX p1 cw >>= getTC
    ob  = getOB cw p1 p2
    tc2 = getPCX p2 cw >>= getTC

getOBSend' :: TC -> OutBox p2 -> TC -> OBSend
getOBSend' tc1 ob tc2 = obwAddWork where
      obwAddWork work =
        readIORef (ob_state ob) >>= \ lw ->
        let bFirst = null lw in
        let lw' = work:lw in
        writeIORef (ob_state ob) lw' >>
        when bFirst (addTCSend tc1 deliver)
      deliver =
        readIORef (ob_state ob) >>= \ lw ->
        writeIORef (ob_state ob) [] >>
        let work = (sequence_ . reverse) lw in
        let sem = ob_sem ob in
        sem_wait sem >> -- wait for batch to become available
        addTCRecv tc2 (sem_signal sem >> work) -- send batch

-- | obtain the outbox resource p1->p2 starting from the world context
--   (Each partition has a set of outboxes for each other partition)
getOB :: (Partition p1, Partition p2) => PCX W -> p1 -> p2 -> IO (OutBox p2)
getOB cw p1 _ = getPCX p1 cw >>= findInPCX

-- | a simple semaphore to model bounded-buffers between partitions. 
-- 
-- State is either a count of available resources or a list of tasks
-- each waiting on a resource. Signal either releases one thread or 
-- adds one to the count. Wait either decrements count or adds one
-- event to the list.
--
-- Sirea is designed to be wait-free modulo the batch-level pushback
-- buffers between threads. The semaphore is used to model pushback. 
-- Semaphore should not be used by any Sirea code outside this file. 
--
type Semaphore = IORef (Either Int [Event])

-- | create a new semaphore with a given initial count
newSemaphore :: Int -> IO Semaphore
newSemaphore nInit = 
    assert (nInit >= 0) $
        newIORef (Left nInit)

-- signal that a resource is available; will call an event if any
-- thread is waiting, otherwise will increment number available
sem_signal :: Semaphore -> IO ()
sem_signal s = join $ atomicModifyIORef s inc
    where inc (Left n) = (Left (succ n), return())
          inc (Right []) = error "invalid state for semaphore"
          inc (Right (op:[])) = (Left 0, op)
          inc (Right (op:ops)) = (Right ops, op)

-- sem_wait_event will set the semaphore to call an event when
-- the resource is available, and grants the resource with the
-- same call (must signal to release)
sem_wait_event :: Semaphore -> Event -> IO ()
sem_wait_event s ev = join $ atomicModifyIORef s dec
    where dec (Left 0) = (Right [ev], return ())
          dec (Left n) = assert (n > 0) $ (Left (pred n), ev)
          dec (Right ops) = (Right (ops++[ev]), return ())

-- synchronous wait for resource
sem_wait :: Semaphore -> IO ()
sem_wait s = 
    newEmptyMVar >>= \ mv ->
    sem_wait_event s (putMVar mv ()) >>
    takeMVar mv

