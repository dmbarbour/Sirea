
Sirea
=====

**Si**mply **Rea**ctive! Declarative orchestration in Haskell using the Reactive Demand Programming (RDP) model. 

_an RDP application is a complex symphony of signals and declarative effects orchestrated in space and time_

_an RDP application is one big, active declaration that can be modified at any time_

Features
--------

Sirea is not ready for its first release. It probably doesn't compile. It has no usable features yet.

Here are some features I look forward to:

* _Declarative effects._ Effects expressed by RDP are commutative, idempotent, continuous, and concurrent. These properties offer many of the reasoning and refactoring benefits associated with _pure_ programming styles (i.e. ability to move code around, abstract it, eliminate duplicates), while also supporting open composition, encapsulation, and dynamic acquisition of resources similar to an OOP model. There is no need to pipe data all the way through the application just to raise a window or manage a widget.

* _Declarative linking._ Easily attach your RDP behavior to the real world with a simple dependency-injection framework. Control.Make minimizes boiler-plate in the default case while enabling safe, non-invasive configuration when you need it. 

* _Predictable, composable performance._ Sirea is designed for soft real time applications. It will control the amount of in-flight or lazy computations at any given instant to keep memory footprint and incremental CPU costs under control. Sirea won't forbid expensive functions growing state, but an attentive developers should have very little difficulty managing performance and memory footprint. 

* Practical parallelism through two orthogonal, declarative mechanisms.
    1. Data Parallel Haskell (DPH) sparks are available by use of `bstrat`, which allows firing sparks a few milliseconds before you need the result. The tight bond between sparks and time of sampling helps control memory overhead and prevents sparks from "fizzling." 
    2. Sirea models partitioning and spatial distribution by use of `bcross`. Sirea will automatically create a thread for each partition reached by `bcross`. RDP communication between threads is performed in coarse-grained batches. The batches provide efficiency and snapshot consistency between partitions. Sirea may block on send to control performance, but should often be wait-free if every thread is keeping up with its computation burdens.

* _Persistence by default._ RDP requires modeling state as external to the RDP behavior. While a valid option is external _volatile_ state (doesn't survive the Haskell process), I see this as an opportunity. The state models provided with Sirea are persistent by default. Further, state has stable identity in source code (via types) so it can survive minor edits to code between runs of Sirea. 

* _Anticipation._ RDP does not make predictions, but it does _propagate_ them. Decent predictions at just a few locations can have a widespread effect in an RDP system, supporting optimistic computation and timely preparation of resources, e.g. loading a texture, or opening a window slightly ahead of requirement so they are available right when we need them. Anticipation is valuable even if it were only an implementation detail, but developers have access to anticipation by use of `bpeek`.

* _Embeddable._ Sirea doesn't take over the main loop. Instead, Sirea supplies a step function for starting and maintaining the behavior. These properties also apply partition threads created by Sirea. They can do useful work, such as managing state, network, or display in addition to processing RDP communication. Sirea supplies some simple mechanisms for communicating between threads and RDP, providing a frozen snapshot view of other threads (only updated between steps). Developers also have power to create new behavior primitives to attach new resources with `bUnsafeLnk`.


Reactive Demand Programming (in Sirea)
======================================

To understand Reactive Demand Programming, you must understand behaviors. To understand behaviors, you must understand signals. If you know arrowized functional reactive programming, much of this will be familiar. Though, RDP and FRP differ significantly in how they model state and integrate effects.

Signal Values
-------------

A **Signal** is a time-varying value that represents state. For example, I were to model the `w` key on my keyboard, it would be in an `up` state most of the time, and in a `down` state for the brief times I press the button, as when writing "down", "when", or "writing". A keystate signal is _not_ a keypress _event_. A keystate will have a positive, non-transcendal duration such as 5 milliseconds.

A signal in RDP will also vary in its _presence_. This allows me to represent the times I'm not looking at the keyboard, such as the time before the application started. A signal is called **active** while it is present, and **inactive** while it is absent. Caution: **inactive does not mean error!** A lot of people seem to misunderstand that. 

### Modeling Signals

A potential model of RDP signals might be:
    
    -- where T represents time
    type Sig a = T -> Maybe a -- not actually used

Here `Maybe a` represents activity of the signal. The signal is `Nothing` while inactive. 

This is very generic. It allows me to represent continuous, time-varying values such as the position of a thrown baseball over time (including interpolation). It's actually a bit too generic, since it also lets me represent _semantic garbage_ such as instantaneous, discontinuous values. 

Unfortunately, while continuous-varying signals are expressive, they are also expensive, and a lot of algorithms on them are non-deterministic (e.g. Euler method for integrals). 

Sirea, being _simply_ reactive, sticks with discrete-varying signals. The actual signal model in Sirea is conceptually closer to:

    type Sig a = (Maybe a, \[(T,Maybe a)\]) -- not actually used

Here the first value represents the signal state for all history (often `Nothing`), and the list describes (in monotonic time order) a sequence of discrete updates. This model is much less expressive, but also much more efficient. But it is still problematic. In practice, signals will contain redundant values (e.g. we map lossy functions to them, like `a -> Bool`), and we will want to filter those values to avoid redundant computation down the line:

    -- pre filter
    (Just True, \[(t0, Just True), (t1, Just True), (t2, Just False), ...\])
    -- post filter
    (Just True, \[(t2, Just False), ...\])

But filtering lists is problematic. It can diverge, if there are no False values. Even if I'm guaranteed a False value, the amount of computation to find it is unknown and thus would violate real-time computation properties. The actual Sirea implementation of concrete signal includes structure to support incremental processing of signals, support for filtering included.

### Updating Signals

Many reactive models update only one value at a time, then propagate changes. 

RDP updates whole signals. Not a _current value_, but an _entire future_ for one signal. In the most trivial case, this may reduce to updating one value. But even then the signal update carries important information about _when_ the update applies.

Each update comes with two time values and one signal:

1. **Update time**. This is the time to switch to the future signal. Ideally this time is slightly ahead of the wall-clock, such that it doesn't cause any rework and values can be calculated just in time for the future to become the present. Updates in the past are called _straggling updates_ and are the cause of inconsistency.

2. **Stability time**. A promise that all future Update times (excluding this update) will be no less than Stability time. Stability tells us how much history we can garbage-collect without risking loss of information. In some cases, stability might be the only property that updates.

3. **Future signal**. The future of the signal we are updating. This might represent one constant value or an infinite, complex fixpoint describing future state according to anticipated forces. Ideally, it's a good estimate for at least a little while so it won't need to be corrected immediately.

The application of an update is a trivial matter. Dig into the code if you're interested (type `SigUp`). 

The design matters. Since updates occur in logical time, the RDP system is much less sensitive to actual arrival order of updates. Since they occur in the future, they rarely interfere with present computations. The future signals become the basis for _anticipation_. Continuous improvement of future estimates provides a brief window of tolerance to disruption or communication hiccups, buying time for graceful failover. Finally, processing whole signals can be much more efficient in general than one update at a time.

For a network or a distributed system, we might limit updates to a few seconds more or less (based on heuristics or annotations). We only have freedom to update _infinite_ futures within the process. But the advantages still apply even if our estimates are only a few seconds long.

### Where are the Signals?

RDP users never touch signals directly. RDP prevents direct access to ensure properties about how signals are used. But "RDP user" is not the only role of a Sirea user. If a Sirea developer is adapting new resources or services via `bUnsafeLnk`, the signal values will be directly accessible. 


Signals in Space and Time
-------------------------

Simple signals are effective in-the-small, for tightly coupled data that changes all at once. But they don't scale well. More structure is needed for efficiency, flexibility, and scalability. Sirea uses three structural notations for signals.

1. `(S p x)` - represents a concrete signal (`Sig a`) located spatially at partition `p`. The purpose of this annotation is to prevent accidental (or illegal) communication of signals between partitions. In general, one could also restrict what functions are available in different partitions, but Sirea assumes that all partitions are Haskell partitions.

2. `(x :&: y)` - describes an asynchronous product of signals. The `x` and `y` signals must have equal duration. When synchronized, `(S p a :&: S p b)` is similar to `(S p (a,b))`, but the former is often more efficient and flexible because the two values can update and be processed independently. OTOH, if you need to map a function that takes both `a` and `b`, you'll need the zipped version.

3. `(x :|: y)` - describes an asynchronous sum of signals. The active duration is partitioned between the `x` and `y` signals. When synchronized, `(S p a :|: S p b)` is similar to `(S p (Either a b))`, but the former allows independent updates and processing. Often, you'll get the Either type by mapping a function, then split it to the asynchronous sum. 

The _asynchronous_ property for products and sums of signals means that the elements may be offset in latency, such that they don't line up perfectly or transition neatly. This happens due to _independent processing_ of the left and right signals. It is natural that processing adds latency, and independent processing adds independent latency. 

It is possible to _logically synchronize_ the asynchronous signals by applying a pure delay function to match latencies. Logical synchronization is more efficient and scalable than shared state synchronization, but you need to know the latencies. RDP supports logical synchronization by tracking latency statically. (Ideally, latency would be part of the concrete signal's type signature, but Haskell does not support this.)

Introducing Behaviors
---------------------

A **Behavior** in RDP is a _signal transformer_ with potential for _declarative effects_.

A _signal transformer_ is an abstract process that takes a signal as input and generates a signal as output by modifying the first. Signal transformers cannot create or destroy signals - a property formalized as **duration coupling**: active periods of the output signal must correspond to active periods of the input signal. There may be a small delay between input and output. 

### Basic Behaviors

All behaviors operate on _signals in space and time_. Many behaviors are data plumbing or pure operations on signals. A few examples of simple behaviors:

    bfmap  :: (a -> b) -> B (S p a) (S p b)
    bfirst :: B x x' -> B (x :&: y) (x' :&: y) 
    (>>>)  :: B x y -> B y z -> B x z
    bzip   :: B (S p a :&: S p b) (S p (a,b))
    bsplit :: B (S p (Either a b)) (S p a :|: S p b)
    bdup   :: B x (x :&: x)
    bmerge :: B (x :|: x) x
    bswap  :: B (x :&: y) (y :&: x)
    bmirror:: B (x :|: y) (y :|: x)
    bfst   :: B (x :&: y) x
    binl   :: B x (x :|: y)

There are around thirty or forty of these that must be learned to use RDP effectively. Fortunately, many of those are simple symmetries, dualities, or combinations that make them easier to remember. There are a few hundred shorthand composites defined in [FRP.Sirea.Bdeep](src/FRP/Sirea/Bdeep.hs) but they have consistent naming so you only need to learn a few to know them all. A few useful behaviors serve as performance annotations, to force lazy thunks, parallelize future computations, eliminate redundant updates, or simplistic memoization.

The most common behavior I use is simple sequencing:
    
    bxy >>> byz

Since behaviors operate on continuous signals, the above actually represents a pipeline with potential for parallelism: fresh `x` values can be continuously computed in parallel with older `y` values. A vertical slice through the pipeline is effectively the signal type at that point, in this case `y`.

RDP models concurrency in terms of task-parallel pipelines, i.e. using `bdup` to create a parallel pipeline, then `bfirst` and `bswap` to add different tasks to different pipelines. A couple useful composite behavior is `(***)`:

    (f *** g) = bfirst f >>> bswap >>> bfirst g >>> bswap

The use of `bswap` does obscure the idea a bit, but RDP allows us to simplify a lot. Consider the behavior:

    bdup >>> (f *** g) >>> bzip


Graphically, the above expression is (with `X` representing swap):

             __f__   __g__   ___
            /     \ /     \ /   \
        bdup       X       X     bzip
            \_____/ \_____/ \___/

But Sirea guarantees that swap is free at runtime; it only is necessary in the textual representation (since it is one-dimensional). It can be eliminated in the graphical representation.

             ___f___    
            /       \   
        bdup         bzip
            \___g___/   


Similarly for: `bdup >>> (f *** g) >>> (h *** k)`:

             ___f___     ___h__
            /       \   /      
        bdup         >>>        
            \___g___/   \___k__

Sirea ensures this data plumbing is free. And RDP, like other arrow models, guarantees it is semantically equivalent to:

             ___f >>> h__
            /            
        bdup              
            \___g >>> k__

Thus, simple behaviors such as `bdup`, `bfirst`, and `bswap` ultimately allow us to construct and extend concurrent pipelines. And this concurrency becomes another opportunity for parallelism.


### Effectful Behaviors

RDP behaviors may be effectful. Consider the following:

    bmousepos :: B (S p ()) (S p MousePos)
    bgetfile  :: B (S p FileName) (S p FileState)
    bcamctl   :: B (S p PanTiltZoom) (S p ())

Using effectful behaviors, we can observe the mouse position, obtain file states, and control a camera - just to name a few. There can be effectful behaviors for each resource, and for collections and views of resources. A consequence is a simple type such as: 
   
    bmain     :: B (S p ()) (S p ())

is a potential signature for RDP application, much like `void main()` is a potential signature for procedural applications. Behind that trivial signature, an RDP application is a complex symphony of signals and declarative effects orchestrated in space and time.

Effects are valuable for programming open and extensible systems, but can make code unclear or more difficult to reuse. RDP avoids or mitigates many problems of effectful and concurrent programming. See discussion below on _Declarative Effects and Concurrency_.

### Spatial Behavior

Effectful behaviors are often bound to specific partitions. For example, the mouse position shouldn't be available from just any arbitrary partition. You must ask a UI thread. We can classify partitions the same way we classify any other Haskell type: use a typeclass.

    bmousepos :: (HasUI p) => B (S p ()) (S p MousePos)

In this case Haskell would require some extra information to infer _which_ HasUI partition to access. Anyhow, assuming we start elsewhere, we'll need to send a signal over to the UI thread in order to apply the bmousepos behavior. The `bcross` behavior allows developers to move signals between process-local partitions:

    bcross :: (Partition p, Partition p') => B (S p x) (S p' x)

This would allow us to send a signal to the UI thread, request the mouse position. After we have the mouse position, we could cross again to process it elsewhere. Or we could process it in place - for example, using `bfmap` to translate MousePos into something that can immediately control the UI. 

Developers should treat `bcross` with some care. Even between Haskell's lightweight threads, `bcross` can introduce non-trivial latencies due to scheduling. Sirea can introduce implicit logical delay at each crossing based on configuration. 

Also, while Sirea ensures snapshot consistency between partitions, there is still some indeterminism regarding exactly which snapshot you'll be using. For determinism, it is often preferable to keep operations within one thread. Sirea does support a notion of "scopes" - hierarchical sub partitions - which can be useful for representing multiple locations, objects, or resources within a single, internally deterministic thread.

### Temporal Behavior

TODO
    bsynch :: B x x 
    bdelay :: DT -> B x x
Why would you model delay? (consistency; state anomalies in a pipeline)
How much delay?



### Dynamic Behavior

Behaviors may be defined at runtime then evaluated in context. This is useful for a wide variety of programming patterns, such as modeling plug-ins, service brokering, staged programming, progressive disclosure, mutable views, and capability security patterns. The typical behavior to evaluate dynamic behaviors is `beval`:

    beval :: DT -> B (S p x :&: S p (B (S p x) (S p y))) (S p y :|: S p ())

This is a complicated signature. I'll break it down a bit:

    type DynBeh p x y = B (S p x) (S p y)
    type ResErr p y = (S p y :|: S p ())
    beval :: DT -> B (S p x :&: S p (DynBeh p x y)) (ResErr p y)

The `DT` argument is the delay value for `beval`. Ideally, it could be inferred from the _type_ for `DynBeh`. Alas, Haskell doesn't support dependent types. Any result from the dynamic behavior will be padded out to `DT`. The error option for output is necessary if the dynamic behavior would have too much latency, or fails for any other reason. 

All arguments are logically synchronized by `beval`. However, they are still represented in an asynchronous product because this allows the signals for `x` and for `DynBeh` to change independently. This can model compilation, for example, if we have a slow-changing dynamic behavior and a fast-changing parameter. (_Note:_ `beval`'s limit of one argument and one response is _not_ essential. Sirea will provide a few more options for performance and flexibility.)

Note that everything happens in just one partition. This is a necessary restriction to avoid implicit communication between partitions. This is also the reason I say "dynamic" instead of "first class" - first class connotes that all types are supported. The dynamic behavior itself may cross into other partitions to actually do things.


### Implementation and Extension of Behaviors

Sirea provides one concrete behavior type, a GADT simply named `B`. The GADT symbolically represents implementation for many of the basic data-plumbing behaviors, such that they have no runtime costs. But most interesting behaviors - even `bfmap` - are defined with a `MkLnk` primitive. The essential operations and data are:

    data MkLnk x y = MkLnk {
      ln_build :: Lnk y -> IO (Lnk x)
    }

    data Lnk x where
      LnkSig  :: LnkUp a -> Lnk (S p a)
      LnkProd :: Lnk s a -> Lnk s b -> Lnk (a :&: b)
      LnkSum  :: Lnk s a -> Lnk s b -> Lnk (a :|: b)

    data LnkUp a = LnkUp {
      ln_update :: SigUp a -> IO ()
    }

    data SigUp a = SigUp {
      su_stable :: T
      su_update :: Maybe (T, Sig a)
    }
    
    bUnsafeLnk :: MkLnk x y -> B x y

The `ln_build` operation is executed when the behavior is first built. The behavior is built backwards - given the output target, you return the input target. Once built, `ln_update` will be called after each update, allowing the concrete signal values to be processed. _(Optimization is left out of the above representation; see the code for details.)_

Sirea clients can add new primitive behaviors via `bUnsafeLnk`, but must be cautious to avoid breaking RDP abstractions. Clients should be able to adapt most new resources using simple combinations of existing behaviors.

Declarative Effects and Concurrency
-----------------------------------

TODO


