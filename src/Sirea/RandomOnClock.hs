{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -fno-excess-precision #-}

-- | Efficient pseudorandom data is convenient for many systems and
-- applications - probabilistic models, simulations and games, and
-- providing test data to hammer at a system. 
--
-- Traditional techniques for random data are stateful and eventful: 
-- keep a pseudorandom number generator, ask it for a value, receive
-- a value, update the generator so future requests return different
-- values. The state might be held by a mutable variable or threaded
-- through a function.
--
-- Those techniques are a poor fit for RDP, which lacks state (even
-- threaded state) internally, and lacks eventful observations. 
--
-- For RDP, a more suitable technique is to model external resources
-- that vary randomly over time, i.e. white-noise signals. Multiple
-- sources may be observed to influence simulation state or support
-- continuous probabilistic choice.
--
-- RandomOnClock follows this technique. Each behavior, identified
-- by seed and clock spec, describes a deterministic, pseudorandom 
-- source that varies in space and time.
--
--   * in time: different random values after each clock tick
--   * in space: multiple random values within each clock tick
--
-- Values generated on a given seed and clockspec will be consistent
-- even across processes. If unique values are needed, construct the
-- seed as a unique function of the application or process instance.
-- It may also vary with version of sirea-core, but should be stable
-- for most versions.
--
-- The basic random types for RandomOnClock are Bool and Double:
--
--   * generate Double with `rand` 
--   * provides full 53-bit precision for IEEE Doubles
--   * range 0 to 1, including 0, excluding 1 - i.e. [0.0,1.0)
--
-- This was chosen for simplicity, ease of use, and performance.
-- No need to manage random ranges, easy multiply-and-floor 
-- functions to understand ranges, etc.
--
module Sirea.RandomOnClock
    ( brandom, brandomSeed, brandomClockSeed
    , bRandMClockSeed, bRandMDynClockSeed
    , RandM, Rand(..), randD, randB, randSplit
    , rgenOnClock
    , RGen, integerToRGen, rgenToInteger, runRandM
    ) where

import Control.Arrow (first)
import Control.Applicative
import Control.Exception (assert)
import Data.Int (Int32)
import Sirea.Behavior
import Sirea.Clock
import Sirea.Time

-- | brandom: a "default" pseudorandom stream (for convenience)
-- Generates one pseudorandom value per second. All observers will
-- see the same stream, up to type.
brandom :: (Behavior b, HasClock b, Rand a) => b (S p ()) (S p a)
brandom = brandomSeed 0

-- | brandomSeed: allows specifying a seed. Each seed essentially
-- identifies a unique random stream. Generate one value per second.
brandomSeed :: (Behavior b, HasClock b, Rand a) => Integer -> b (S p ()) (S p a)
brandomSeed = brandomClockSeed csSeconds

-- | brandomClockSeed: allows specifying a clock. Both clock and seed
-- contribute to the stream's identifier. 
brandomClockSeed :: (Behavior b, HasClock b, Rand a) 
                 => ClockSpec -> Integer -> b (S p ()) (S p a)
brandomClockSeed = bRandMClockSeed rand

-- | bRandMClockSeed: allows specifying a generator, i.e. not bound
-- to the Rand typeclass. Generators do not affect the stream, so
-- all observers with common generators will see the same values.
bRandMClockSeed :: (Behavior b, HasClock b) => RandM a 
                -> ClockSpec -> Integer -> b (S p ()) (S p a)
bRandMClockSeed rm cs sd = rgenOnClock cs sd >>> bfmap (runRandM rm >>> fst)

-- | bRandMDynClockSeed: allows manipulating the generator at
-- runtime, e.g. as a function of other inputs.
bRandMDynClockSeed :: (Behavior b, HasClock b) 
                   => ClockSpec -> Integer -> b (S p (RandM a)) (S p a)
bRandMDynClockSeed cs sd = (bfmap run &&& load) >>> bzap 
    where run rm rg = fst (runRandM rm rg)
          load = bconst () >>> rgenOnClock cs sd

-- | rgenOnClock: generates an RGen at every clock step based
-- on the ClockSpec, the given seed, and the reported time.
rgenOnClock :: (Behavior b, HasClock b) 
            => ClockSpec -> Integer -> b (S p ()) (S p RGen)
rgenOnClock cs nSeed = rgenOnClock' cs hseed
    where hseed   = hashInteger hperiod nSeed
          hperiod = hashInteger hoffset $ dtToNanos $ clock_period cs
          hoffset = hashInteger hsInit $ dtToNanos $ clock_offset cs 


-- rgenOnClock after translating cs and seed into an initial hash
rgenOnClock' :: (Behavior b, HasClock b)
             => ClockSpec -> HS -> b (S p ()) (S p RGen)
rgenOnClock' !cs !h0 = bclock cs >>> bfmap tmToRGen
    where tmToRGen tm = 
            let hd = hashInteger h0 (tmDay tm) in
            let hn = hashInteger hd (tmNanos tm) in
            let (HS a b _) = hsFini hn in
            let s1 = a `mod` 2147483562 in
            let s2 = b `mod` 2147483398 in
            RGen (1 + s1) (1 + s2)


-- technique: hash clockspec, seed, and time. 
-- to consider: generate RGen on clock (simple, but expensive)

-- a simple hash to build a fresh RGen when it is needed.
data HS = HS {-# UNPACK #-} !Int32 {-# UNPACK #-} !Int32 {-# UNPACK #-} !Int32 

-- mix in a little noise to start
hsInit :: HS
hsInit = HS 35899 25577 70717

-- avalanche the last few entries.
hsFini :: HS -> HS
hsFini hs@(HS a b c) = hc
    where h0 = hashInt32 hs 0
          ha = hashInt32 h0 a
          hb = hashInt32 ha b
          hc = hashInt32 hb c


-- hopefully this is a decent hash (wrgt. security, if not speed).
-- rgen on clock doesn't need to be secure, but it'd be better if
-- seeds cannot be easily computed. Might need to switch to secure
-- random generator.
hashInt32 :: HS -> Int32 -> HS
hashInt32 (HS a b c) !n = HS a' b' c'
    where a' = n + a * 11 + b * 557 - c * 3119
          b' = 1 + b * 37 + c * 701 - a * 5477
          c' =     c * 23 + a * 421 - b * 7151


hashInteger :: HS -> Integer -> HS
hashInteger h !n =
    if (abs n) < 91019 then hashInt32 h (fromIntegral n) else
    let (q,r) = n `divMod` 91019 in
    hashInt32 (hashInteger h q) (fromIntegral r)
    


-- used as the internal time-sequence between clock steps.
--timeSeq :: DT -> T -> [T]
--timeSeq dt t0 = t0 `seq` (t0 : timeSeq dt (addTime t dt))

-- | RandM is a monad that allows developers to generate random
-- values and structures (with `rand`). It is essentially a state
-- monad that threads a random number generator. 
--
-- Allowing RandM generators within each timestep is a convenience.
-- Equivalent can be achieved by obtaining just a few random values
-- to seed another pseudorandom number generator.
newtype RandM a = RandM { _runRandM :: RGen -> (a,RGen) }

runRandM :: RandM a -> RGen -> (a,RGen)
runRandM = _runRandM

instance Functor RandM where
    -- fmap :: (a -> b) -> RandM a -> RandM b
    fmap f (RandM fn) = RandM (first f . fn)
instance Applicative RandM where
    pure c = RandM $ \ rg -> (c,rg)
    (<*>) (RandM mkF) (RandM mkA) = RandM $ \ g0 ->
        let (f,gf) = mkF g0 in
        let (a,ga) = mkA gf in
        (f a,ga)
    -- Don't bother generating unused values.
    (*>) _ rb = rb
    (<*) ra _ = ra
instance Monad RandM where
    return = pure
    (>>) = (*>)
    (>>=) (RandM mkA) fn = RandM $ \ g0 ->
        let (a,ga) = mkA g0 in
        runRandM (fn a) ga
    
-- | Rand enables Sirea developers to construct ad-hoc values out of
-- pseudo-random-valued Doubles in range [0.0,1.0), and Bools. These
-- values are near uniformly distributed, but other random types or
-- probability distributions could be built from them, overloading 
-- class Rand.
class Rand a where
    rand :: RandM a
instance Rand () where
    rand = pure ()
instance (Rand a, Rand b) => Rand (a,b) where
    rand = (,) <$> rsr <*> rand
instance (Rand a, Rand b, Rand c) => Rand (a,b,c) where
    rand = (,,) <$> rsr <*> rsr <*> rand
instance (Rand a, Rand b, Rand c, Rand d) => Rand (a,b,c,d) where
    rand = (,,,) <$> rsr <*> rsr <*> rsr <*> rand
instance (Rand a, Rand b, Rand c, Rand d, Rand e) => Rand (a,b,c,d,e) where
    rand = (,,,,) <$> rsr <*> rsr <*> rsr <*> rsr <*> rand
instance (Rand a, Rand b, Rand c, Rand d, Rand e, Rand f) => Rand (a,b,c,d,e,f) where
    rand = (,,,,,) <$> rsr <*> rsr <*> rsr <*> rsr <*> rsr <*> rand
instance (Rand a, Rand b, Rand c, Rand d, Rand e, Rand f, Rand g) => Rand (a,b,c,d,e,f,g) where
    rand = (,,,,,,) <$> rsr <*> rsr <*> rsr <*> rsr <*> rsr <*> rsr <*> rand
instance (Rand a, Rand b) => Rand (Either a b) where
    rand = rand >>= \ choice ->
           if choice then Left <$> rand
                     else Right <$> rand
instance (Rand a) => Rand (Maybe a) where
    -- basically `Left a | Right ()`
    rand = rand >>= \ choice ->
           if choice then Just <$> rand
                     else pure Nothing 
instance Rand Double where
    rand = randD
instance Rand Bool where
    rand = randB

-- analogy: roll two ten-sided dice (values 0 to 9)
--   multiply first by 0.1
--   multiply second by 0.01
--   add for uniform random value from 0.0 to 0.99
-- here, we use 2147483563-sided dice.
randD :: RandM Double
randD = RandM $ \ g0 ->
    let (a,ga) = rgenNext g0 in
    let (b,gb) = rgenNext ga in
    let d1 = fromIntegral a * rgenFirstDigit in
    let d2 = fromIntegral b * rgenSecondDigit in
    let roll = d1 + d2 in
    roll `seq` gb `seq` (roll,gb)

-- this isn't a perfectly fair roll, but it is fair within a few
-- parts in a billion.
randB :: RandM Bool
randB = RandM $ \ g0 ->
    let (a,ga) = rgenNext g0 in
    let b = even a in
    b `seq` ga `seq` (b,ga)



-- rsr, just shorthand for creating a random value on a separate generator.
rsr :: Rand a => RandM a 
rsr = randSplit rand

rgenFirstDigit, rgenSecondDigit :: Double
rgenFirstDigit  = 1.0 / fromInteger (d) 
rgenSecondDigit = assert (k < 10000) $ 1.0 / fromInteger ((d * d) + k)
    where k = leastValueThat preventsRoundingToOne

d :: Integer
d = 2147483563 -- from RGen

-- leastValueThat returns the lowest non-negative Integer that 
-- returns True for a given test, assuming that all values higher 
-- return True and all values lower return False. 
--
-- Note: diverges if no such value exists...
leastValueThat :: (Integer -> Bool) -> Integer
leastValueThat passesTest = fn 0 (ub 1)
    where ub k = if (passesTest k) then k else ub (k + k)
          fn l u = if (l == u) then u else
                   let m = (l + u) `div` 2 in
                   if (passesTest m) then fn l m
                                     else fn (m+1) u

-- Ideally, we could just use (d*d) in denominator for the second
-- digit when randomly generating doubles. However, between two 
-- digits of base 2147483563, we have more precision than a Double
-- can carry. This admits the possibility of rounding to 1.0, which
-- makes it more difficult to utilize the random value (i.e. with
-- proper use of `floor` and `ceiling` to generate integers). To 
-- avoid rounding errors, I'll be tweaking the second digit to 
-- ((d*d)+k) for the least such k that prevents rounding up to 1.0.
--
-- Rather than trying to compute this by hand, I'll let Haskell
-- run the tests itself.
--
-- Sanity check: least k should be less than 10000, since we're 
-- cutting only 3-4 digits. Effect on observable distribution is
-- negligible.
preventsRoundingToOne :: Integer -> Bool
preventsRoundingToOne k =
    let maxD1    = fromInteger (d - 1) * rgenFirstDigit in
    let sndDigit = 1.0 / fromInteger ((d*d)+k) in
    let maxD2    = fromInteger (d - 1) * sndDigit in
    let total    = maxD1 + maxD2 in
    (total < 1.0)

-- | randSplit will separate RandM computations by splitting the
-- underlying generator. This reduces sequential dependency between
-- the split component and the rest of the computation. This allows
-- `par` or independent lazy evaluation.
randSplit :: RandM a -> RandM a
randSplit ra = RandM $ \ g0 ->
    let (gl,gr) = rgenSplit g0 in
    let agl' = runRandM ra gl in
    (fst agl', gr)


{-  Regarding the following functions:
    These functions are modified from System.Random, package 'random-1.0.1.1', 2012 July.
    
    I decided to relocate these for several reasons:
        * to access full ~60-bit state for seeding (System.Random limits to 32 bits)
        * to modify the generator for performance (unpacked strict representations)
        * to ensure stability of the underlying random number generator across versions
        * reluctance to introduce more dependencies for sirea-core

    The following references were given for the computation generating random values:

        Pierre L'Ecuyer, /Efficient and portable combined random
        number generators/, Comm ACM, 31(6), Jun 1988, pp742-749.

        The Web site <http://random.mat.sbg.ac.at/> is a great source of information.

   The random-1.0.1.1 library is available under a BSD3 compatible license, with the 
   following attributions in its LICENSE file:

      * Code from the GHC project which is largely (c) The University of
        Glasgow, and distributable under a BSD-style license (see below),
      * Code from the Haskell 98 Report which is (c) Simon Peyton Jones
        and freely redistributable (but see the full license for
        restrictions).

    The full text of these licenses is reproduced below.  Both of the
    licenses are BSD-style or compatible.

    -----------------------------------------------------------------------------

    The Glasgow Haskell Compiler License

    Copyright 2004, The University Court of the University of Glasgow. 
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are met:

    - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
     
    - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
     
    - Neither name of the University nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission. 

    THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY COURT OF THE UNIVERSITY OF
    GLASGOW AND THE CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
    INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
    FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    UNIVERSITY COURT OF THE UNIVERSITY OF GLASGOW OR THE CONTRIBUTORS BE LIABLE
    FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
    OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
    DAMAGE.
 
    --------------------------------------------------------------------------------

    Code derived from the document "Report on the Programming Language
    Haskell 98", is distributed under the following license:

      Copyright (c) 2002 Simon Peyton Jones

      The authors intend this Report to belong to the entire Haskell
      community, and so we grant permission to copy and distribute it for
      any purpose, provided that it is reproduced in its entirety,
      including this Notice.  Modified versions of this Report may also be
      copied and distributed for any purpose, provided that the modified
      version is clearly presented as such, and that it does not claim to
      be a definition of the Haskell 98 Language.
    
-}

-- | RGen is a pseudorandom number generator (abstract)
-- (Use RGen to obtain RandM values)
data RGen = RGen {-# UNPACK #-} !Int32 {-# UNPACK #-} !Int32

-- | Construct an RGen from an Integer
integerToRGen :: Integer -> RGen
integerToRGen s = RGen (1 + fromInteger s1) (1 + fromInteger s2)
    where (q, s1) = s `divMod` 2147483562
          s2      = q `mod` 2147483398

-- | Extract an Integer from an RGen
--   integerToRGen . rgenToInteger = id
rgenToInteger :: RGen -> Integer
rgenToInteger (RGen s1 s2) = ((n2 - 1) * 2147483562) + (n1 - 1)
    where n1 = toInteger s1
          n2 = toInteger s2


rgenNext :: RGen -> (Int32, RGen)
rgenNext (RGen s1 s2) = z' `seq` (z', RGen s1'' s2'')
    where z'   = if z < 1 then z + 2147483562 else z
          z    = s1'' - s2''

          k    = s1 `quot` 53668
          s1'  = 40014 * (s1 - k * 53668) - k * 12211
          s1'' = if s1' < 0 then s1' + 2147483563 
                            else assert (0 /= s1') s1'
    
          k'   = s2 `quot` 52774
          s2'  = 40692 * (s2 - k' * 52774) - k' * 3791
          s2'' = if s2' < 0 then s2' + 2147483399 
                            else assert (0 /= s2') s2'



-- split allows some parallel generation of values.
-- according to System.Randm this isn't mathematically 
-- sound, which means the two generators might be 
-- observably entangled (i.e. predict too much about
-- one by observing the other). Hopefully this will be
-- good enough in practice.
rgenSplit :: RGen -> (RGen,RGen)
rgenSplit g0@(RGen s1 s2) = (left,right)
    where left = RGen new_s1 t2
          right = RGen t1 new_s2
          new_s1 = if (s1 == 2147483562) then 1 else s1+1
          new_s2 = if (s2 == 1) then 2147483398 else s2-1
          (RGen t1 t2) = snd (rgenNext g0)


