
Sirea
=====

**Si**mply **Rea**ctive! Declarative orchestration in Haskell using the Reactive Demand Programming (RDP) model. 

_an RDP application is a complex symphony of signals and declarative effects orchestrated in space and time_

_an RDP application is one big, active declaration that can be modified at any time_

Features
--------

Sirea is not ready for its first release. It probably doesn't compile. It has no usable features yet.

Here are some features I aim to have by version 1.0:

* _Declarative effects._ Effects expressed by RDP are commutative, idempotent, continuous, and concurrent. These properties offer many of the reasoning and refactoring benefits associated with _pure_ programming styles (i.e. ability to move code around, abstract it, eliminate duplicates), while also supporting open composition, encapsulation, and dynamic acquisition of resources similar to an OOP model. There is no need to pipe data all the way through the application just to raise a window or manage a widget.

* _Declarative linking._ Attach your RDP behavior to the real world with a simple dependency-injection model. Volatile resources, such as a GLUT thread or cache, can be created and controlled declaratively. No need for boiler-plate creation or explicitly hooking callbacks. External services, such as a filesystem or database or HTTP internet access, are controlled indirectly through volatile resources. Simple configuration options may also be represented as dependencies. Multiple implementations for dependencies may coexist, supporting defaults, overrides, fallbacks, and preferences. The basis for Sirea's dependency model is developed as a separate library, `control-make`.

* _Predictable, composable performance._ Sirea is designed for soft real time applications. It will control the amount of in-flight or lazy computations at any given instant to keep memory footprint and incremental CPU costs under control. Sirea won't forbid expensive functions growing state, but an attentive developers should have very little difficulty managing performance and memory footprint. 

* _Parallelism._ Through two practical, orthogonal, declarative mechanisms.
    1. Data Parallel Haskell (DPH) sparks are available by use of `bstrat`, which allows firing sparks a few milliseconds before you need the result. The tight bond between sparks and time of sampling helps control memory overhead and prevents sparks from "fizzling." 
    2. Sirea models partitioning and spatial distribution by use of `bcross`. Sirea will automatically create a thread for each partition reached by `bcross`. RDP communication between threads is performed in coarse-grained batches. The batches provide efficiency and snapshot consistency between partitions. Sirea may block on send to control performance, but should often be wait-free if every thread is keeping up with its computation burdens.

* _Persistence by default._ RDP requires modeling state as external to the RDP behavior. While a valid option is external _volatile_ state (doesn't survive the Haskell process), I see this as an opportunity. The state models provided with Sirea are persistent by default. Further, state has stable identity in source code (via types) so it can survive minor edits to code between runs of Sirea. 

* _Anticipation._ RDP does not make predictions, but it does _propagate_ them. Decent predictions at just a few locations can have a widespread effect in an RDP system, supporting optimistic computation and timely preparation of resources, e.g. loading a texture, or opening a window slightly ahead of requirement so they are available right when we need them. Anticipation is valuable even if it were only an implementation detail, but developers have access to anticipation by use of `bpeek`.

* _Embeddable, Extension Language._ Sirea doesn't take over the main loop. Instead, Sirea supplies a step function for starting and maintaining the behavior. These properties also apply partition threads created by Sirea. They can do useful work, such as managing state, network, or display in addition to processing RDP communication. Sirea supplies some simple mechanisms for communicating between threads and RDP, providing a frozen snapshot view of other threads (only updated between steps). Developers also have power to create new behavior primitives to attach new resources with `unsafeLnkB`.

* _Extensible and Live Programming._ Sirea (via separate library, `sirea-plugin`) provides a runtime plugin framework. Plugins primarily provide service modules. Service modules can fulfill dependencies for _declarative linking_. Plugins may also provide application behavior modules, allowing multiple _main_ behaviors to run concurrently and interact through shared services. Plugins can be recompiled on the fly and hot-swapped, ensuring application behavior is consistent with code in the editor. Live programming with plugins provides a beautiful alternative to REPL loops, and also a viable route for live DSL programming (by live compilation to a Sirea plugin). Sirea as an application platform can run as a thin wrapper for the live plugin programming framework.


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

This is very generic. It allows me to represent continuous-varying values such as the position of a thrown baseball over time (including interpolation). It's actually a bit too generic, since it also lets me represent _semantic garbage_ such as instantaneous, discontinuous values. 

Unfortunately, while continuous-varying signals are expressive, they are also expensive, and a lot of algorithms on them are non-deterministic (e.g. Euler method for integrals). 

Sirea, being _simply_ reactive, sticks with discrete-varying signals. The actual signal model in Sirea is conceptually closer to:

    type Sig a = (Maybe a, \[(T,Maybe a)\]) -- not actually used

Here the first value represents the signal state for all history (often `Nothing`), and the list describes (in monotonic time order) a sequence of discrete updates. This model is much less expressive, but also much more efficient. But it is still problematic. In practice, signals will contain redundant values (e.g. we map lossy functions to them, like `a -> Bool`), and we will want to filter those values to avoid redundant computation down the line:

    -- pre filter
    (Just True, \[(t0, Just True), (t1, Just True), (t2, Just False), ...\])
    -- post filter
    (Just True, \[(t2, Just False), ...\])

But filtering lists is problematic. It can diverge, if there are no False values. Even if I'm guaranteed a False value, the amount of computation to find it is unknown and thus would violate real-time computation properties. The actual Sirea implementation of concrete signal includes structure to support incremental processing of signals, support for filtering included.

### Continuous-Varying Signals?

Continuous-varying signals are valuable for modeling physics, collision detection, and smooth animations (with temporal anti-aliasing and motion blur). Sirea rejects first-class support for continuous-varying signals because they're too difficult to work with in general. But very effective second-class support is feasible.

We could model a continuous signal as: `Sig (T -> a)`. But this is still excessively expressive - we actually only desire *piecewise continuous* signals, i.e. smooth and curvy lines, with potential jumps at discrete steps. Further, we will need *symbolic analysis* for efficient processing, for precise zero-crossing and integrals, and for serialization. Zero crossing is important for collision detection, integrals for continuous state models, and serialization (e.g. send to GPU for tweening, or to remote browser for interpolation). So follow a pattern developed years ago by Conal Elliott for [Compiling Embedded Languages](http://conal.net/papers/jfp-saig/): rather than `T -> a`, use some variation of `Expr T -> Expr a`. 

Once the signals are in place, RDP will require a few dedicated behaviors to split at zero-crossings or support continuous state. Further, specialized variations for `bdelay` and `bpeek` would be necessary to account for the time shift on the contents - i.e. if we delay the outer discrete-varying layer by 100ms, we also need to delay the internal continuous-varying layer by 100ms.

Sirea should eventually provide a simple variation on continuous signals, but it's very low priority.

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
    bzap   :: B (S p (a -> b) :&: S p a) (S p b))
    bsplit :: B (S p (Either a b)) (S p a :|: S p b)
    bdup   :: B x (x :&: x)
    bmerge :: B (x :|: x) x
    bswap  :: B (x :&: y) (y :&: x)
    bmirror:: B (x :|: y) (y :|: x)
    bfst   :: B (x :&: y) x
    binl   :: B x (x :|: y)

There are around thirty or forty of these that must be learned to use RDP effectively. Fortunately, most of those include symmetries, dualities, or combinations that make them easier to remember. There are a few hundred shorthand composites defined in FRP.Sirea.Bdeep but they have consistent naming so you only need to learn a few to know them all. A few useful behaviors serve as performance annotations, to force lazy thunks, parallelize future computations, eliminate redundant updates, or simplistic memoization.

If you are unfamiliar with Arrows, I strongly recommend reading [Understanding Arrows](http://en.wikibooks.org/wiki/Haskell/Understanding_arrows) in the Haskell wikibook. 

The basic data plumbing behaviors of RDP is essentially an arrows model, albeit somewhat more restrained in order to support asynchronous products and sums, and asynchronous updates of effectful signals. For example, there is no variation of `arr` that applies over all elements in an asynchronous product. Instead there is `bfmap` that applies a function to just one signal, and `bzip` or `bzap` to combine signals that happen to be in the same partition.

All the traditional Arrow and ArrowChoice composition operators are provided:

    (>>>) :: B x y  -> B y z  -> B x z                   -- pipeline
    (***) :: B x x' -> B y y' -> B (x :&: y) (x' :&: y') -- parallel
    (&&&) :: B x y  -> B x z  -> B x (y :&: z)           -- duplication
    (+++) :: B x x' -> B y y' -> B (x :|: y) (x' :|: y') -- choice, case
    (|||) :: B x z  -> b y z  -> B (x :|: y) z           -- merge

RDP supports a high level of potential parallelism. There is pipeline parallelism from (>>>) - computing a newer y and an older z in parallel. There is task parallelism from (***) - computing x' and y' at the same time. Data parallelism is also readily supported. 

Due to continuous semantics, RDP allows a great deal of parallelism from the first three compositions. Pipeline parallelism, via (>>>), involves pipeline parallelism (from >>>, computing a newer y and an older z in parallel), task parallelism (from *** or &&&; computing x' and y' in parallel), 

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

If behaviors were pure, we could pretend they were instantaneous. But effects make timing very relevant. We cannot perform an effect based on a value that is still being calculated earlier in the pipeline, or that must be communicated from a distant partition. So RDP models simple, static delays for communication and computation. Delays are important for consistency, to protect against *straggling* updates (updates that arrive too late). Delay is also useful to control feedback when interacting with shared state.

There are three primitive temporal behaviors:

    bdelay :: DT -> B x x
    bsynch :: B x x
    bpeek  :: DT -> B (S p x) (S p (Either x ()))

The `bdelay` behavior will add a constant delay to every concrete signal in `x`. In general, developers should add a small delay after any expensive operation based on a rough estimate, except for `bcross` and other operations that can be responsible for their own delay values. Sirea provides a simple _delay aggregation_ optimization so a lot of small delays can often be applied as one big delay just before time-sensitive effects. By adding delay, a straggling update might no longer be straggling, or even arrive slightly ahead of time (thus providing slack for concurrent operations or underestimates later in the pipeline). 

The slack is more important than the accuracy of delay estimates. Developers only need to get delays in the right ballpark - low enough to meet latency constraints, high enough that updates aren't straggling too badly. Many problem domains can even tolerate straggling updates, depending instead on Sirea's snapshot consistency between partitions or eventual consistency.

The `bsynch` behavior will logically synchronize signals. Different branches of `(x :&: y)` or `(x :|: y)` tend to accummulate different amounts of delay, which is why they are called asynchronous products and sums. Use of `bsynch` will add delay to each concrete signal that would arrive earlier than the highest latency branch, such that the delays are equal on every branch. This works even across partitions, though the extent to which logical synchronization corresponds to physical synchronization is subject to whimsy of schedulers or clocks. It is best to have a little slack in the model to buffer effects ahead of time (sound or video or animation).

The `bpeek` behavior allows developers to peek a small distance into the future, to *anticipate* future demands, or to compare the future against the present to detect changes (without accumulating state). The result is either a future value or an indication that the signal will be inactive at that time in the future. Use of `bpeek` tends to sacrifice stability to improve latency, but is explicit about it and thus more readily constrained to problems that can tolerate straggling updates. Typically use of `bsplit` is applied after `bpeek`.

### Dynamic Behavior

Behaviors may be defined at runtime then evaluated in context. This is useful for a wide variety of programming patterns, such as modeling plug-ins, service brokering, staged programming, progressive disclosure, mutable views, and capability security patterns. The behavior to evaluate dynamic behaviors is:

    beval :: (SigInP p x) => DT -> B (x :&: S p (B x y)) (y :|: S p ())

The essential `beval` is to take complex signal `x` and produce complex signal `y` through the runtime-provided behavior `B x y`.

The `SigInP` constraint enforces that every component of `x` starts in the same partition. This is necessary because whenever we receive a new dynamic behavior we'll need to switch all the input signals to that behavior. That can't be done remotely without magic. There is no constraint that all behaviors *end* in the same partition because we're just updating the signals. 

In actual implementation, `beval` will maintain *multiple* behaviors at once for different periods of time. Future dynamic behaviors are *anticipated* and speculatively installed. Older dynamic behaviors are uninstalled when they are no longer necessary (which depends on when all the parameter signals stabilize). This use of anticipation smooths transition between dynamic behaviors (they'll be warmed up) and improves anticipation everywhere else.

The `DT` argument to `beval` specifies the maximum latency for the dynamic behavior, and sets the latency for `beval` as a whole. (RDP requires static latency at each dynamic layer.) If the dynamic behavior would have larger latency, or if there are other *static* (compile-time) problems with the dynamic behavior, the error output is provided, potentially with much smaller latency than the `DT` argument. 

Developers should take care: for performance, dynamic behaviors need to be *stable* - change rarely, change with a fair amount of advanced knowledge (e.g. via `bdelay`). This need for stability was the initial reason those *asynchronous signals* were developed. 

*NOTE:* Dynamic behaviors may be recursive. This is valuable for a lot of patterns (e.g. recursive view of an XML file with includes or frames). But, at least in Sirea's implementation, a change in a lower layer requires rebuilding everything above it. Developers must pay attention to stability when building large recursive dynamic reactive structures.

*NOTE:* Dynamic behaviors also hinder dead-code elimination. For Sirea, inputs are assumed live at all times, even if the current dynamic behavior doesn't need that input. 

Better optimizations for stability and performance of dynamic behaviors would be motivating arguments for a dedicated RDP language. Sirea aims to remain simple, so provides only the easiest optimizations.

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
    
    unsafeLnkB :: MkLnk x y -> B x y

The `ln_build` operation is executed when the behavior is first built. The behavior is built backwards - given the output target, you return the input target. Once built, `ln_update` will be called after each update, allowing the concrete signal values to be processed. _(Optimization is left out of the above representation; see the code for details.)_

Sirea clients can add new primitive behaviors via `unsafeLnkB`, but must be cautious to avoid breaking RDP abstractions. Clients should be able to adapt most new resources using simple combinations of existing behaviors.

Declarative Effects and Concurrency in RDP
------------------------------------------

RDP is effectful, but is limited to *declarative effects* - by which I mean effects that are (at any given logical instant, i.e. *spatially*) commutative and idempotent, and that hold over for a time. Declarative effects offer many of the reasoning and refactoring benefits associated with pure code: expressions can easily be rearranged, duplicate expressions can be eliminated, or duplicates can be introduced so overlapping subprograms can be abstracted. 

RDP is not the only paradigm with declarative effects. If you are unfamiliar with [Temporal Logic](http://www.eecs.berkeley.edu/Pubs/TechRpts/2009/EECS-2009-173.html), [Event Calculus](http://en.wikipedia.org/wiki/Event_calculus), [Synchronous Programming](http://en.wikipedia.org/wiki/Synchronous_programming_language), or [Concurrent constraint programming](http://en.wikipedia.org/wiki/Concurrent_constraint_logic_programming) then I do recommend researching some of those techniques to broaden your horizons. The [Berkeley Orders Of Magnitude](http://boom.cs.berkeley.edu/) project is making good use of temporal logic for similar reasons that RDP focuses on declarative effects - for scalability to distributed systems, and to simplify reasoning. 

Due to the prevalence of message-passing concurrency, a common misconception is that concurrency is incompatible with synchronous behaviour. But concurrency just means that computations overlap in time in a semantically relevant way - i.e. they interact with each other. RDP behaviors interact by influencing and observing shared services and resources, including shared state.

RDP differs from other declarative effects models in the following aspects:

* RDP is *continuous* in time, rather than discrete time. Sirea models only discrete-varying behaviors, and actually uses a fixpoint representation of time (with nanosecond precision), but treats time as continuous with respect to semantics and logic.
* RDP is *internally stateless*. State is regressed to the edges of the system, to external resources like databases. This is an important property for runtime upgrades, live programming, and resilience. It also simplifies orthogonal persistence.
* RDP is uniformly *bidirectional*. Observable resources are aware of observers, and may be influenced by them. This is useful for robust resource management, e.g. activating services only when they are needed, and deactivating services that are no longer necessary. 
* RDP enforces a *duration coupling* constraint. This ensures that behaviors, and effects they control, can be halted. This is important for process control and security in distributed systems. Unlike mobile computing, it is easy to ensure an RDP application will never escape its human administrators.
* RDP is *open*. An application may be extended at runtime (via dynamic behavior) with access to new services and resources. RDP uses a universal time model to ensure behaviors compose properly in open systems. Resources and services are external and might be influenced or observed by parties outside local control.
* RDP is *securable*, via [capability](n.wikipedia.org/wiki/Capability-based_security) patterns. Behaviors provide access and authority to specific external resources or services. There is no implicit data space like in traditional logic programming. [Ambient authority](http://en.wikipedia.org/wiki/Ambient_authority) can be eliminated, and securely emulated with context objects providing behaviors.

Between *declarative effects* and RDP's own unique properties, developers will need new idioms for problems that might have been trivial in imperative code. Consider imperative state manipulation: `(x := x + 1)`. If we tried to hold this effect over continuous time, over one millisecond say, would it mean we increment once? or a million times for a million nanoseconds? or infinite times? A developer would instead need to express an effect of the form `(x := x + 1 with 60ms cooldown)` to account for how an effect applies over time. (See the section on _Declarative State Models for RDP_, below.)

OTOH, many RDP features or patterns would be difficult to express in imperative code. RDP is not *less* expressive than imperative, but is *differently* expressive.

Effects in RDP are controlled by input signals to effectful behaviors. These signals are called *demands* in a similar sense that messages intended to cause effects are often called *commands*. But where commands are processed one at a time, demands that hold at any given time are processed simultaneously - as a set. Consider the earlier example for controlling a camera:

    bcamctl   :: B (S p PanTiltZoom) (S p ())

This behavior receives a PanTiltZoom signal that might control actuator efforts or provide a carrot for end-goal positions (depending on how PanTiltZoom is specified). But the same behavior could be invoked from multiple locations in an RDP behavior (possibly from multiple applications), resulting in simultaneous PanTiltZoom demands on the actuators. In many cases those demands will be compatible such as `pan left` and `zoom in` simultaneously. 

However, RDP provides no guarantee that demands are conflict free. It is possible to express systems that demand `pan left` and `pan right` on the same camera at the same time. Similar can be expressed in message passing systems, of course, and an implicit, indeterministic race would determine which command wins. For RDP, there are no implicit races. There is only a set of demands. Resolution must be continuous, commutative, idempotent - declarative. *But resolution may still be arbitrary.* For example:

* take no action, report conflicts via another behavior
* favor inertia, whichever direction the camera is already moving
* combine as new behavior, e.g. pan left, pan right, repeat
* favor left arbitrarily because it is lower in lexicographic order

The holistic, set-based view of simultaneous demands will also force developers to confront the issue of conflicts early in their designs. Developers can aim to avoid conflicts before they happen or make them easier to resolve - e.g. control distribution of the `bcamctl` behavior, or express camera commands in terms of weighted constraints. Between avoiding conflicts early in design and resolving them with holistic intelligence (able to resolve many concurrent demands at once), RDP *should* more readily support open, pluggable, extensible programs than many other paradigms - though I've yet to verify this property.

### Declarative State

Traditional state models have been developed for imperative programs - *serializable* updates, mutual exclusion, discrete time. RDP needs state models suitable for control by signals - continuous, concurrent, collaborative, commutative, compositional, and idempotent. Support for speculation and `bpeek` is also very desirable.

Surprisingly, that broad set of constraints allows a very large design space, which includes many simple, familiar, and usable state models. Here are a few valid approaches to state with RDP:

* simple set manipulation: 
    * signals may add and remove records from a set
    * add and remove simultaneously is resolved arbitrarily
    * can augment with expiration, then can eliminate deletion
    * can augment with clustering (machine learning + indexing)
* reactive state transition:
    * state is integer
    * signals contribute possible transitions
    * transition whenever opportunity exists
    * can augment with animation model
* animated reactive term rewriting:
    * state is (term, time debt)
    * signals add term-rewrite rules
    * each rule has attributes (max debt, new debt), each > 0.
    * when a rule is applied, new debt is added to time debt.
    * rules are not applied unless max debt >= time debt
    * time debt towards zero, linearly over continuous time
    * can augment with built in rules (for performance)
    * can augment with ownership types (for security, control)
    * can augment with delayed tasks (e.g. to model expiration)
    * can augment with implicit, inferred (max debt, new debt)
    * natural variations: graph rewriting

Since all state models are *external* to RDP (i.e. modeled as external services), it is not difficult to add orthogonal persistence. It is also not difficult to dream up new ones and add them to an application via library or plugin. There are interesting state models one could create for continuous signals (e.g. based on bezier curves, follow the carrot, integrals of forces). 

Sirea shall provide a few simple state models, including the three described above, complete with *persistence by default*. But none have been implemented yet. And they won't be part of the core package.

A related, interesting possibility is to seek stability without *stateful semantics*. The "stateful semantics" refers to ability to preserve information over time, from the past into the present (even for one nanosecond). Stability doesn't require information be preserved, only that avoid changing unnecessarily. Stability is suitable in cases where you don't strongly care *what* the information is so long as it's consistent and stable - such as UI layout, or path planning systems. Constraint logic programming and many machine learning techniques offer [effective approaches](http://awelonblue.wordpress.com/2012/03/14/stability-without-state/) to stability.

Stateless stability can help alleviate need for real state, and thus simplify reasoning about systems (especially during partial failure and recover).

*NOTE:* If you need *differences* between past and present values (e.g. to redraw the dirty rectangles), then RDP offers a stateless alternative: use `bpeek` to instead find differences between present and future values. This works up to a small constant time, though can work longer if preceded by `bdelay` to recover stability.

*NOTE:* Sirea and RDP will automatically remember values where necessary. No state is needed to combine signals with `bzip`, and use of `badjeqf` can help filter updates that would be identical in order to improve stability. If *memoization* is desired, you should either use [lazy Haskell techniques](http://hackage.haskell.org/package/MemoTrie-0.5) (at risk of space leak!) or use a dedicated state model.

*NOTE:* State IS necessary if you want to model choking of updates, e.g. such that you see at most one update each second. Choking uses state to record the selected value. Animated state, such as *animated reactive term rewriting*, can model choking quite elegantly. *Not that choking is elegant.* Be cautious about choking; it can mask feedback issues without solving them (often better to take the hit, *find* the problem, solve it by design).

### Feedback and Delay

A major concern with reactive programming in continuous time is *feedback* - when a signal output affects its own input. This is a natural concern, with physical cause - e.g. if we place a microphone near its associated amplifier, we often get very shrill feedback. Feedback is unstable and can consume a lot of energy very quickly.

In RDP, you cannot directly express a closed loop. (No equivalent to ArrowLoop.) So feedback is impossible for pure RDP behaviors. But feedback is still possible when interacting with external services and state. Consider:

    getState s >>> bfmap fn s >>> writeState s

In this case, we might be trying to writeState at the same *instant* for which we're reading state, and thus `getState` would be modified by the results of `writeState`, creating a feedback loop through s. 

Feedback is RDP's equivalent to divergent computation. 

Symbolic analysis or laziness could potentially eliminate feedback loops in some cases. But RDP is designed for *open systems*, where those techniques are not generally available. Sirea does not assume them. Good state models for RDP will help, both in avoidance and resolution of the problem. For example, animated reactive term rewriting eliminates most need for such loops (rewrite rules include simple functions) and helps stabilize the result (limiting updates via time debt, and not all rewrite rules will apply). But those are of limited scope.

Since you can't design feedback entirely out of complex systems, add `bdelay`. This slows the feedback down, changing instantaneous feedback into a well-defined incremental feedback process. Potentially, a natural one.

### Bridging Paradigms

Animated reactive term rewriting is incredibly expressive. If you are unfamiliar with term rewrite systems, look up Maude and broaden your horizons. The *reactive* variation on term rewriting flips when rules and terms are provided (and who provides them). The *animated* variation models incremental processing and makes the timing of state updates deterministic and declarative. But animated reactive term rewriting has the same amazing expressiveness as the traditional version.

Expressing a Turing machine with reactive term rewriting is no challenge at all. 

A few well-chosen, built-in rules and an application expressed within the term rewrite node could achieve performance competitive with native applications of the same paradigm. (Consider specializations for large vector and matrix ops, plus ability to compile terms representing simple functions into OpenCL.)

State also serves as a powerful basis for IO and side-effects in open systems. 

To achieve this, use the [blackboard metaphor](http://en.wikipedia.org/wiki/Blackboard_system). Agents use signals to write a task into shared state representing the board, e.g. "print this document". A printer agent could then modify the request to claim it, e.g. adding "agent Orion has accepted this task". Orion would then print the document, and update the term with status information as it runs. When finished, the original writer can remove the term. 

Observing and influencing intermediate declarative state in a reactive paradigm has many advantages over shared state in other paradigms (such as message passing, procedure calls):

* no blind waiting; intermediate status observable
* no polling, never miss an update
* precise job control; achievable by modifying state 
* orthogonal asynchronous and even offline interactions
* ad-hoc and compositional "conditions" (waiting on combination of states)
* ad-hoc control-flow and [workflow patterns](http://en.wikipedia.org/wiki/Workflow_patterns)
* precise timing (for schedulers, simulations) via *animated* state

Use of intermediate state is essential to many design patterns in RDP. RDP is ultimately about open, declarative orchestration of *stateful* systems - sensors, actuators, UI, and databases.

*NOTE:* the notion of logically deterministic timeouts initially struck me as terribly odd, as though counter to their purpose. But timeouts still serve a valuable role when modeling incremental computations in shared state, e.g. computing a better move in a game. And deterministic timeouts makes for reproducible errors, easy maintenance, and better reasoning about timing.

### Metacircular

It's RDP all the way down. That is the *ideal* view an RDP developer should of the system. 

A "main" behavior is essentially a dynamic behavior activated by an unspecified lower level RDP behavior. It runs concurrently with other "main" behaviors, and might interact with them via shared services. Dynamic behaviors must be internally stateless because they're logically swapped out at every instant. The "main" behavior must be internally stateless because it is, in essence, a dynamic behavior.

This has advantages:

* For metaprogramming. Much greater consistency and uniformity of code. It is easy to interpret text and a few capabilities into a dynamic behavior, and it won't be second class.
* For separation of concerns. External state models are easier to persist, and it is easy to try new state models to account for challenges like partitioning.
* For live programming. The "main" behavior can be upgraded or replaced without disturbing state. An RDP application is one big, active declaration that can be modified at any time. 

Haskell [plugins](http://hackage.haskell.org/package/plugins) could make live programming a reality. But much design work is needed to make it livable (reactive dependency injection and linking; automatic management of threads; switch to fallback plugins for while the one you're editing is broken). This will be pursued in a sirea-plugins package.


