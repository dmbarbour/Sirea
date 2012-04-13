
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

* _Predictable, composable performance._ Sirea is designed for soft real time applications. It will control the amount of in-flight or lazy computations at any given instant to keep memory footprint and incremental CPU costs under control. Sirea won't forbid expensive functions growing state, but an attentive developers should have very little difficulty managing performance and memory footprint. 

* Practical parallelism through two orthogonal, declarative mechanisms.
    1. Data Parallel Haskell (DPH) sparks are available by use of `bstrat`, which allows firing sparks a few milliseconds before you need the result. The tight bond between sparks and time of sampling helps control memory overhead and prevents sparks from "fizzling." 
    2. Sirea models partitioning and spatial distribution by use of `bcross`. Conveniently, Sirea will also create a thread for each partition according to a typeclass. Communication between threads is performed in coarse-grained batches for efficiency and _snapshot consistency_ (enough to prevent most malign glitches). Sirea may block on send to control performance but can be wait free if threads don't fall behind.

* _Persistence by default._ RDP requires modeling state as external to the RDP behavior. While a valid option is external _volatile_ state (doesn't survive the Haskell process), I see this as an opportunity. The state models provided with Sirea are persistent by default. Further, state has stable identity in source code (via types) so it can survive minor edits to code between runs of Sirea. 

* _Anticipation._ RDP does not make predictions, but it does _propagate_ them. Decent predictions at just a few locations can have a widespread effect in an RDP system, supporting optimistic computation and timely preparation of resources, e.g. loading a texture, or opening a window slightly ahead of requirement so they are available right when we need them. Anticipation is valuable even if it were only an implementation detail, but developers have access to anticipation by use of `bpeek`.

* _Embeddable._ Sirea leaves developers in charge of their own main loop by providing a step function rather than taking control of the thread. Sirea supports simple patterns for communication between the thread and the RDP behavior. Sirea users can create new extensions and adapters with `bUnsafeLnk`, though it does take some care. Developers also have some generic control over the threads Sirea creates - e.g. create a thread that manages an OpenGL context and displays at 60 fps some data supplied by RDP. 


Reactive Demand Programming (in Sirea)
======================================

To understand Reactive Demand Programming, you must understand behaviors. To understand behaviors, you must understand signals. Reactive demand programming is about composing behaviors to orchestrate signals, which may control effects declaratively.

If you know arrowized functional reactive programming, much of this will be familiar. RDP and FRP vary considerably in how they model state and 

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
1. **Stability time**. A promise that all future Update times (excluding this update) will be no less than Stability time. Stability tells us how much history we can garbage-collect without risking loss of information. In some cases, stability might be the only property that updates.
1. **Future signal**. The future of the signal we are updating. This might represent one constant value or an infinite, complex fixpoint describing future state according to anticipated forces. Ideally, it's a good estimate for at least a little while so it won't need to be corrected immediately.

The application of an update is a trivial matter. Dig into the code if you're interested (type `SigUp`). 

The design matters. Since updates occur in logical time, the RDP system is much less sensitive to actual arrival order of updates. Since they occur in the future, they rarely interfere with present computations. The future signals become the basis for _anticipation_. Continuous improvement of future estimates provides a brief window of tolerance to disruption or communication hiccups, buying time for graceful failover. Finally, processing whole signals can be much more efficient in general than one update at a time.

For a network or a distributed system, we might limit updates to a few seconds more or less (based on heuristics or annotations). We only have freedom to update _infinite_ futures within the process. But the advantages still apply even if our estimates are only a few seconds long.

### Where are the Signals?

RDP users never touch signals directly. RDP prevents direct access to ensure properties about how signals are used. But "RDP user" is not the only role of a Sirea user. If a Sirea developer is adapting new resources or services via `bUnsafeLnk`, the signal values will be directly accessible. 


Signals in Space and Time
-------------------------

Simple signals are effective in-the-small, but they don't scale well. More structure is needed for efficiency, flexibility, and scalability. Sirea uses three structural notations for signals.

1. `(S p x)` - this is the same as `Sig x` but it has a spatial annotation `p` to identify a _partition_. The primary purpose of this annotation is to prevent accidental or illegal communication of signals between partitions. In Sirea, different partitions generally correspond to different threads.

2. `(x :&: y)` - describes an asynchronous product of signals. The `x` and `y` signals must have equal duration. When synchronized, `(S p a :&: S p b)` is similar to `(S p (a,b))`, but the former is often more efficient and flexible because the two values can update and be processed independently. OTOH, if you need to map a function that takes both `a` and `b`, you'll need the zipped version.

3. `(x :|: y)` - describes an asynchronous sum of signals. The active duration is partitioned between the `x` and `y` signals. When synchronized, `(S p a :|: S p b)` is similar to `(S p (Either a b))`, but the former allows independent updates and processing. Often, you'll get the Either type by mapping a function, then split it to the asynchronous sum. 

The _asynchronous_ property for products and sums of signals means that the elements may be offset in latency, such that they don't line up perfectly or transition neatly. This happens due to _independent processing_ of the left and right signals. It is natural that processing adds latency, and independent processing adds independent latency.

It is possible to _logically synchronize_ these signals by applying a delay function (pure, no actual delay) to the low-latency signal values. You can even logically synchronize signals that are in different partitions. But you would need to know how much logical delay to add. RDP statically tracks how much delay has been introduced in each branch.


Introducing Behaviors
---------------------

A **Behavior** in RDP is a _signal transformer_ with potential for _declarative effects_.

A _signal transformer_ is an abstract process that takes a signal as input and generates a signal as output by modifying the first. Signal transformers cannot create or destroy signals - a property formalized as **duration coupling**: active periods of the output signal must correspond to active periods of the input signal. There may be a small delay between input and output. 

Sirea provides one concrete behavior type, simply named `B`.



--
-- > bxy :: B (S p x) (S p y)

needs a home
-------------

* Multi-layer defense against inconsistency. It is difficult to eliminate inconsistency without sacrificing performance, scalability, or security. So RDP is designed to control, tolerate, and recover from it. 
    1. Within each partition, signals propagate deterministically. 
    2. Modeling logical delay prevents straggling updates, which are the cause of inconsistency. This is especially important when multiple stages of the same pipeline interact with a shared state resource. 
    3. Snapshot consistency between relatively coarse-grained _partitions_ is achieved by a simple batching mechanism. This resists most malign glitches. 
    4. Anticipation. The future is continuously improved until it becomes the past. Even if the future is inconsistent, it can be a reasonable estimate to cover for a communication hiccup. 
    5. Eventual consistency. A safety net - difficult to traverse, but nice to have below. State can be retroactively corrected a small amount.


* _Easy consistency model._ Updates are commutative, idempotent, and eventually consistent. Snapshot consistency between partitions eliminates most risk for malign glitches. If delay is modeled adequately, with `bdelay`, it can eliminate inconsistencies by shifting them to the future and limiting feedback cycles for interaction with shared state.


* A function-like entity. (A variation of Arrow.)
* The 
* That takes 

 an abstract process that takes a signal as input, produces a signal as output, with potential for _declarative effects_ in between. 



Behaviors compose. They compose in parallel `(b1 *** b2)`. They compose in pipelines `(bxy >>> byz)`. They compose as alternatives `(b1 +++ b2)`. They compose by staged computing and dynamic towers `(beval bx)`. They compose indirectly, via shared services and state. 


In Sirea, type `Sig` is the concrete signal model. But another signal type is used for behaviors, which is named 

Partitions...


-- | Behaviors for Reactive Demand Programming (RDP)
--
-- Simple RDP behaviors are signal transformers. But they compose.
-- An RDP application is a complex symphony of signals orchestrated
-- in space and time. Dozens of independent, declarative pipelines
-- can can interact indirectly but deterministically through shared 
-- services or state. Delay and synchronization are logical, free of
-- waits and inefficient shared state. Computation can be recombined
-- only where necessary. Redundant calculations are easy to avoid.
--
-- SIMPLE BEHAVIORS INTRODUCTION
--
-- The RDP behavior type for Sirea is named 'B'. Each behavior is
-- a signal transformer, meaning it takes a signal as input and
-- generates a signal as output. The input is called `demand` and
-- the output `response`.
--
-- > bxy :: B (S p x) (S p y)
--
-- This describes a behavior that takes a signal of type `x` and
-- generates a signal of type `y`. Further, the `p` value represents
-- partition - an abstract location for the signal. Type S is not
-- an actual Haskell data type (it is an empty data decl). Instead
-- it represents that this is a concrete signal. At runtime, type
-- (S p x) will be represented by (Sig x) from FRP.Sirea.Signal.
-- Type x is an actual Haskell data type.
--
-- The response signal, (S p y) is analogous to a return value from
-- a procedure call. However, the general idiom is to pipeline the
-- response as a demand to the next behavior, as with:
--
-- > bxy >>> byz
--
-- These pipelines represent continuous computations, which may be 
-- active for long durations (often hours or days). RDP enforces a
-- duration coupling property that means z values are output for the
-- same duration as x values are input, regardless of composition.
-- There is potential for much pipeline parallelism here, with newer
-- y values being computed in parallel with older z values.
--
-- The simplest Sirea behaviors apply functions to their values:
--
-- > bfmap :: (a -> b) -> B (S p a) (S p b)
-- > bconst :: c -> B (S p a) (S p c)
--
-- These serve as useful `glue` between effectful behaviors.
--
-- DECLARATIVE EFFECTS
--
-- RDP behaviors are not pure. In bxz, z may depend on x, but may
-- also depend on services or state accessed by bxy or byz behavior.
-- Further, those external services or state may be influenced based
-- on the x signal. In a sense, RDP is bidirectional. Encapsulation
-- of effects simplifies open composition and orchestration.
-- 
-- In other modules you will find libraries of effectful behaviors
-- and service adapters. Simplified examples might include:
--
-- > bmousepos :: (S p ()) (S p MousePos) -- observe mouse position
-- > bgetfile :: (S p FileName) (S p FileState) -- observe a file
-- > bcamctrl :: (S p PanTiltZoom) (S p ()) -- control a camera
--
-- I discourage direct use of these behaviors. Good RDP design obeys
-- the same SOLID principles as good OO design. However, Sirea does
-- not attempt to legislate against bad design.
--
-- Note that some behaviors may use a unit signal (S p ()) as source
-- or sink. These are often used for simple queries or side-effects.
-- In Sirea, we  model RDP applications as effectful behaviors:
--
-- > bMain :: B (S p ()) (S p ())
--
-- Effects in RDP, however, are different than the side-effects you
-- are familiar with from imperative programming styles. RDP lacks
-- events and one-off commands. The traditional models of mutable
-- state no longer apply. We cannot say: 
--
-- > x := x + 1
--
-- Because it would need to be a demand signal, with a duration. If
-- the duration is one millisecond, it is not clear that we should
-- increment the variable once, or a thousand times, or an infinite
-- amount. 
--
-- That said, there are new state models suitable for RDP. Some are
-- simple variations on traditional imperative state models. As one
-- example, we could express some accumulators with a notion of time
-- debt or cooldown:
--
-- > x := x + 1 with cooldown 3 ms
--
-- Sirea provides simple state models in another module.
--
-- Effects in RDP are declarative, not imperative. Continuity is one
-- constraining property among several:
--
--   1. Continuous, like the demand signals that control them.
--   2. Concurrent, demands overlap in time and space.
--   3. Idempotent, identical demands count only once.
--   4. Commutative, demand origin and arrival order irrelevant.
--   5. Stateless locally, behaviors do not accumulate history.
--
-- The idea is to ensure an RDP application is effectively one big,
-- active declaration that can be modified at any time. Declarations
-- of the form "The cat is in the cradle" are naturally continuous,
-- concurrent, idempotent, and commutative. Elimination of local
-- state keeps application behavior tightly coupled to source code,
-- and supports live programming and resilience.
--
-- RDP applications are not stateless. They borrow external state as
-- needed from databases, filesystems, runtime services. Orthogonal
-- persistence is easy to achieve.
--
-- 


that has
-- The commutativity and idempotence properties allow eliminating

-- Continuity forbids modeling events. 
--
-- addition to being continuous like signals, effects are commutative
-- and idempotent. 
--
-- Because this would be in a signal and would hold for
-- is impossible with RDP. This is because RDP controls effects by
-- use of signals. Signals are eventless, continuous in time. Demand
-- to increment x will have a duration, perhaps one millisecond. How
-- much do we increment x during one millisecond? Once? A thousand
-- times? Infinity?
--
-- RDP developers are expected to learn new idioms and patterns to
-- solve problems. In most cases, we do not need counters like the
-- one expressed above. But if we do need a counter, there are ways
-- to express something perhaps close enough. For example:
--
-- > x := x + 1 with cooldown 1 ms
--
-- The idea of using cooldown or time-debt is one possibility among
-- many. Sirea shall come with several state models designed for 
-- continuous, reactive control by RDP behaviors. 
--
-- Effects are further constrained to support declarative reasoning 
-- and composition: Concurrent demands on a resource or service are 
-- idempotent and commutative, at least spatially (at any instant),
-- which makes RDP expressions commutative and idempotent at source
-- code level. 
-- 
-- NO LOCAL STATE
--
-- RDP is designed for resilience, orthogonal persistence, and live
-- programming in distributed systems. A sacrifice to achieve these
-- properties was local state - i.e. RDP behaviors do not accumulate
-- state over time, nor can they observe their own past. The absence
-- of state means that RDP behaviors are never in a bad state, and
-- that they never need to be restarted after edits or changes. 
--
-- Semantically, RDP behaviors are stateless. RDP applications must
-- borrow state from external resources (like databases) as needed.
-- The implementation of RDP behaviors does use state to support 
-- efficient update processing. RDP implementations are push-based,
-- i.e. updates to any signal are immediately pushed to observers.

--
-- RDP behaviors are deehaviors are semantically stateless: an accumulator
-- cannot be expressed within an RDP behavior. 

idempotence and commutativity at the source code 
-- level. Spatially (i.e. at any given instant) RDP demands must be
-- idempotent and commutative. RDP behaviors are also stateless. A

There is no exclusive access in RDP,
-- unless developers explicitly
--
-- TEMPORAL ORCHESTRATION AND REASONING
--
-- As mentioned earlier, pipelines allow computing older values late
-- in a pipeline in parallel with new values early in the pipeline.
-- For example, we can compute new y values at the same time as old
-- z values:
--
-- > bxy >>> byz
--
-- Not mentioned above is how this impacts side-effects. If we were
-- to naively represent this as
--
-- This can compute new y values at the same time as older z values.
--
-- Due to interaction with shared state and resources, pipelines of
-- RDP behaviors need to be annotated with timing properties:
--
-- > bxy >>> bdelay 0.01 >>> byz
--
-- 
--
-- COMPLEX SIGNALS

-- Duration coupling means that RDP behaviors are transformers only.
-- They cannot create or destroy signals. (They may, however

--
-- Concrete signals are simplistic, insufficient for real world
-- applications. We can place arbitrary resources into one signal:
--
-- > S p (MousePos, FileState)
--
-- But this would mean our runtime signal is just a series of pairs
-- (mousePos, fileState). We would not know which has updated except
-- by keeping a lot of state for comparisons. This would result in a
-- lot of redundant computation or comparisons. Further, putting the
-- values into pairs in the first place requires synchronization.
--
-- So RDP and Sirea provide distributed, asynchronous signals. Most
-- common is product, (x :&: y) indicates two signals are active for
-- the same durations.
--
-- > (S p1 MousePos) :&: (S p2 FileState)
--
-- These signals are distributed because they can exist in separate
-- partitions. They are asynchronous because we can put e
-- These signals are asynchronous in two senses. 

At runtime, updates
-- for mouse and file state are independent, so we avoid redundant
-- computations on file state when only mouse position has changed.
-- Further, they may represent stages of independent pipelines. 
--
-- These signals are not only async
-- So Sirea provides a notion of complex, asynchronous sign
-- 
--
-- Concrete signals are too simplistic for real-world applications.
-- We could represent arbitrary concrete resources in one concrete
-- signal, such as (S p (mousePos,fileState)). But then we'd need to
-- observe a sequence of pairs and each time determine whether the
-- file state or mouse position updated. Or recompute everything.
-- Further, to create these pairs in the first place requires the
-- mouse positions and file states be part of the same
--  
--    (S
-- 
-- 
-- Each RDP behavior is a signal transformer. Similar to a Haskell
--
--
-- RDP behaviors can route, duplicate, split, recombine, transform, 
-- and generally orchestrate a system of signals across partitions
-- and continuously over time. RDP behaviors often represent many
-- mostly independent pipelines of behavior. Signals are processed
-- sometimes only for side-effects, dropping the result. While RDP
-- can drop parts of a signal, RDP cannot create or entirely destroy
-- a signal: a duration coupling property requires response signals 
-- be active for the same periods as the demand signals.
--
-- Signals have state that varies over time. 
--
-- The duration coupling property doubles as an effective basis for
-- resource control, i.e. the demand signal also serves as a keep
-- alive signal for external resources or services. 
--
-- ARROWS
--
-- Composition of behavior in RDP will be familiar to people who
-- grok arrows (e.g. Control.Arrow) or arrowized FRP styles.  tiny 
-- fraction of people
-- who grok Arrows or arrowized FRP. 
--  
-- THIS MODULE
--
-- This module describes the basic RDP behaviors - data plumbing,
-- functional transforms, performance annotations, synchronization,
-- distribution. These behaviors are pure, lacking observable side 
-- effects or state - excepting bUnsafeChoke, which sacrifices RDP
-- safety properties for performance (use with caution!).
--
-- Additionally, bUnsafeLnk is provided, which allows developers to
-- hook external resources and legacy libraries. bUnsafeLnk is not
-- intended for direct use; rather, developers can build libraries 
-- of RDP-safe behaviors (i.e. that meet idempotence, commutativity,
-- locally stateless, eventless, duration coupling requirements).
-- Enforcing safety is left to developer discipline. Developers are
-- encouraged to name Unsafe behaviors that fail RDP's principles.
--
-- Other Sirea modules will leverage bUnsafeLnk to provide access to
-- useful resources - state, user input, video, sound, etc.
--
-- There is one concrete behavior type in Sirea, simply named `B`.
-- However, most behaviors are described in typeclasses. Typeclasses
-- are convenient for grouping and documenting behaviors. They also
-- support behavior transformers (e.g. Reader or Error arrows).
--
--
-- 
-- is locally stateless, and behaviors cannot be stored externally,
-- so first-class RDP behaviors are volatile - implicitly revoked 
-- when we no longer share them. Volatility is a valuable property
-- for security and live programming. 
--
-- To actually evaluate a dynamic behavior requires 

-- Sirea leaves safety to developer discipline. 
-- Sirea does
-- 
Sirea leaves safety to developer discipline.
--
-- Sirea does not enforce t
--
-- 
- (with the exception of bUnsafeChoke).  
--
-- It seems to take a Eureka moment to really grasp RDP. I suggest
-- playing with it, work through some tutorials that will eventually
-- exist. If you are familiar with arrowized FRP, you have a head 
-- start on grokking RDP composition and idioms, though RDP handles
-- state, events, and effects differently than FRP. 
--
-- RDP behaviors are eventless. There are no zero-duration `events`
-- in an RDP application. Traditionally eventful tasks are modeled
-- by signal - i.e. for button pressing, we model the button down
-- for a short time then back up. Avoiding events reduces need for
-- stateful accumulators. Where necessary, developers may recognize
-- events by an idiom using bpeek to compare future signal against
-- present signal.

-- in the sense that independent agents can interact by directly or
-- indirectly sharing behaviors.   
-- t
-- effects based on the current input signals. 
-- RDP behaviors are arrows, though not quite Control.Arrow or 
-- Megacz's generalized arrows. As arrows, behaviors support much
-- point-free (not pointless) data plumbing
--
-- In Sirea, most data plumbing is essentially free at runtime. But
-- there are exceptions; bzip, bmerge, bconjoin, bsplit have runtime 
-- overheads, for example.
--
-- Listed here are several `generic` behaviors, excluding resource
-- specific behaviors (e.g. state, clock, mouse, display, network,
-- filesystem, etc.) which are provided by dedicated modules.
--
--  CATEGORY
--   bfwd - identity behavior
--   (>>>) - forward composition (from Control.Category)
--
--  PRODUCTS
--   bfirst b - apply b on first element in product
--   bsecond b - apply b on second element in product
--   (b1 *** b2) = bfirst b1 >>> bsecond b2
--   bswap - flip first and second signals
--   bdup - duplicate signal
--   bfst - keep first signal, drop second
--   bsnd - keep second signal, drop first
--   bassoc(l|r)p - associate left or right on product
--   bzipWith, bzip - combine two concrete signals
-- 
--  SUM (CHOICE)
--   bleft b - apply b on left option in sum
--   bright b - apply b on right option in sum
--   (bl +++ br) - bleft bl >>> bright br
--   bmirror - flip left and right signals
--   bmerge - combine two choices into one signal (implicit synch)
--   binl - static choice of left option (~ if true) 
--   binr - static choice or right option (~ if false)
--   bassoc(l|r)s - associate left or right on sum
--   bsplit - lift a decision in a signal to asynchronous layer
--
--  ARROW MISC
--   bfmap - apply function to a concrete signal (when active)
--   bconst - map a constant value to a concrete signal (when active) 
--   bvoid - execute one behavior but drop the result
--   bconjoin - partial merge on a product of sums
--   bdisjoin - distribute a sum into a product
--
--  SPATIAL-TEMPORAL
--   bdelay - delay a signal by a fixed amount
--   bsynch - synch an asynchronous signal (to slowest)
--   bDelayBarrier - force application of aggregated delay
--   bcross - communicate a signal between partitions
--            see FRP.Sirea.Partition
--
--  EXTENSIONS AND ADAPTERS
--   bUnsafeLnk - hook FFI, legacy services, external resources
--
--  PERFORMANCE (maybe another module)
--   bforce  - apply sequential strategy relative to stability
--   bstrat  - apply parallel strategy relative to sampling
--   badjeqf - eliminate adjacent equal updates
--   bUnsafeChoke - skip minor frames when updates too fast
--

