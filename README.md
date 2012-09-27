
Sirea
=====

**Si**mply **Rea**ctive! Declarative orchestration in Haskell using the Reactive Demand Programming (RDP) model. 

_an RDP application is a complex symphony of signals and declarative effects orchestrated in space and time_

_an RDP application is one big, active declaration that can be modified at any time_

Everybody knows *"side-effects aren't compositional"*. But that statement is only true in general. There are useful subsets of side-effects that have nice properties - e.g. idempotence, commutativity, monotonicity. For a small subset of effects, one can achieve equational reasoning and refactoring on par with the very best _pure, law abiding_ programming models. I call these *declarative effects*. Any pure functional programming advocate also knows effects are rarely essential, i.e. that sufficient cleverness will find a wonderfully pure solution most problems. How much more could be achieved with just a little cleverness and a few, highly constrained, declarative effects?

Reactive Demand Programming was developed as an answer to this question. RDP's particular effect model was inspired from a symmetric *observer effect* in physics and psychology: one cannot generally observe or query a system without altering its behavior. Of course, by ignoring the response, one can "observe" a system with the sole purpose of influence. In RDP, resources, state, and agents may indirectly react to a set of signals that may represent active queries, requests, or commands (collectively, *demands*). Signal processing is local, modular, and composable. Treating the signals as a *set* provides spatial idempotence and commutativity. 

The answer: *Anything worth doing* can be done with declarative effects. (Or, rather: *Anything worth being* can become with declarative effects.) This includes state, video, music, sensor networks, control systems, user interface. All can be adequately observed and influenced via the RDP paradigm. It does take new idioms and abstractions (especially new state models) to effectively adapt and utilize external resources.

Reactive Demand Programming simultaneously delivers:

* the reasoning and composition benefits of pure dataflow models
* the flexibility and adaptability of ad-hoc IO
* the encapsulation and dynamic reconfigurability of OOP
* the robust open composition of object capability model
* the extensibility of publish-subscribe or blackboard systems
* the concurrency and parallelism of multi-agent systems
* the utilization and scalability of lightweight time-warp

RDP is not the first paradigm with highly declarative effects. Synchronous reactive programming, temporal logic programming, discrete event systems, and concurrent constraint systems each precede RDP by more than a decade. Use of sub-structural types for uniqueness or linearity have also been utilized to great effect. However, RDP is the only paradigm I am aware of that supports declarative effects in an open, extensible system.

This README file is intended to serve as an introduction to both Sirea and RDP. It attempts to be concrete, to expose enough of the implementation aspects that a developer might have some confidence in RDP and feel comfortable with integrating their own resources. Feedback is welcome.

Sirea Status
------------

Sirea is now in an alpha state. All the primitive behaviors (everything in Sirea.Behavior) have been implemented, plus partitions and a few trivial resources (clock, printing to console). Demand monitors are coming soon. 

### Haskell Packages

Several packages are planned for Sirea. This toplevel directory is **sirea-core**.

* **sirea-core** (75%) primitive behaviors, demand monitor, resources, utilities, startup and shutdown. 
* **sirea-glfw** keyboard, mouse, joystick, GL window
* **sirea-filesys** simplistic reading and writing of files with reactive updates
* **sirea-state** term rewriting, state transition system, tuple space, history
* **sirea-stable** a stateless stable model based on weighted constraint logic
* **sirea-webapp** live web apps; pages as loading agents; serve mixed client/server dynamic RDP behaviors
* **sirea-plugins** live loading and constraint-based linking of plugins
* **sirea-live** a blank-slate application for live programming, plugin based

I'd also like to get some useful sounds (SuperCollider, Euterpa, or maybe microsounds?), likely something for video input, and probably something for wxWidgets (but shifting state outside the widgets). Some of these might become separate projects, too, rather than just separate packages.

### Specific Features

Reactive Demand Programming offers many wonderful, high level features that make Sirea a pleasure to use. RDP also offers interesting state models that would not have been envisioned for imperative systems. These are discussed in later sections. Here, I'll aim for a short list of Sirea specific features.

* _Type Driven Resource Locators._ Users of Sirea have almost zero overhead for connecting to arbitrary resources and using them. Resources and threads are named by types. Data.Typeable and `typeOf` are leveraged for initialization, sharing, and resource management. Any `IO` boiler plate is easily shifted to a few typeclasses in adapter libraries. The plugins module will take this feature further, replacing Haskell's name-based import system with a type-driven import model.

* _Orthogonal Persistence._ Reactive Demand Programming necessarily models state as an external resource. Sirea takes full advantage of this, by making all state resources persistent (excepting those that are *semantically* volatile, such as short windowed history). 

* _Live Programming._ Live programming has been done in many languages, including Haskell. However, it has generally been ad-hoc, requiring a highly constrained programming style. Reactive Demand Programming offers a sound foundation for live programming, including dependency management and state. Sirea will support live programming through a blank slate application combined with runtime compilation and reloading of plugins. *You will be able to start your Sirea/Haskell application before writing it.* This has potential beyond RDP, to become a primary way to develop Haskell applications.

* _Multi-Agent Systems._ If an RDP behavior is specified primarily for its effects, it is easy to consider it an *agent* for a multi-agent system. Sirea favors this idea for the higher level application behaviors: the initial `SireaApp` is an agent; the module Sirea.AgentResource allows unique agents to be modeled as resources via a typeclass; the *live programming* model will be agent-based, allowing multiple plugins to each contribute resources.

* _Performance._ Sirea is designed to perform competitively and predictably. This includes a dynamic solution for the update ordering problem, efficient batching of updates between threads, use of stateful caches as needed, speculative evaluation and resource acquisition, bounded buffers to control memory and ensure fairness, a few simple optimizations, and behaviors for data parallelism. Sirea is suitable for soft real-time applications. 

* _Batteries Included._ Sirea is intended to be a complete application framework, a valid option for industry work. Convenience functions, documentation, and utility behaviors are included. Planned packages will cover many common resources - user input, graphics, filesystem, web services. Ultimately, Sirea should be a serious, mature application framework - the first of its kind for truly declarative programming.

### Weaknesses

The weaknesses of Sirea RDP belong more to Sirea than to RDP. 

* Developers must use a point-free style for RDP behaviors in Sirea. This a very rewarding style, and the full power of Haskell functions is still available for static information (clean, staged programming). But newcomers seem to find point-free intimidating. While I'd like to answer this concern with a higher layer language that can compile to behaviors, it is low priority for Sirea.

* Sirea does not track resource usage for safety. It is easy to express open feedback loops, analogous to placing microphone near its amplifier. There are idioms to avoid such loops (e.g. for blackboard metaphor, don't put replies onto same board as requests), and there are idioms to dampen loops (e.g. `bdelay` or clever use of `badjeqf`). But Sirea provides no static warnings. Developers must learn the idioms and use discipline. (Though, a few of Sirea's constructs will dynamically detect local cycles and forcibly dampen them, emitting a runtime warning.)

* RDP is designed for [object capability model](http://en.wikipedia.org/wiki/Object-capability_model) systems, using dynamic behaviors as runtime composable capabilities. (This is reflected, for example, in having fine-grained capabilities for many resources.) Sirea, however, is designed to be convenient in context of Haskell's module and type systems, and uses types to obtain ambient resources. I am not entirely comfortable with this tradeoff; it isn't necessary if one reifies the module and linking system (e.g. service registry and matchmaker). 


Reactive Demand Programming (in Sirea)
======================================

The Reactive Demand Programming (RDP) view divides the world into three kinds of things: **resources**, **behaviors**, and **signals**. 

* **Resources** might broadly be classed into sensors, actuators, and state. Specific examples include keyboard, mouse, joystick, webcam, microphone, monitor, speaker, filesystem, databases, network, printers. By nature, resources are external to RDP, but may be accessed by RDP behaviors. Resources cannot be created in RDP: there is no equivalent to OOP `new`. However, resources may be dynamically discovered at runtime, and clever manipulations of stateful resources (such as a filesystem) can model creation in terms of discovery (e.g. by computing a unique filename).

* **Behaviors** describe computation-rich data plumbing between resources. Some behaviors will represent access to a resource, providing a capability to observe or influence it. But the majority of behaviors in an RDP application are often simple data plumbing and pure transforms (cf. Sirea.Behavior). RDP behaviors cannot accumulate state; all state is kept in external resources. A simple, linear behavior might gain a joystick signal from GLFW, transform that signal into controls for a robotic arm, `bcross` over to a partition representing the robot resource, then push the signal to the robotic arm. (`getJoyData >>> bfmap joyToRobotArmCtrl >>> bcross >>> controlArm`). RDP can express many independently concurrent behaviors, e.g. using `|*|`. Behaviors can be dynamic, i.e. there is a behavior to evaluate behaviors (`beval`).

* **Signals** describe values as they change over time. Those values typically represent states - e.g. the position of a mouse, frames from a webcam, content for a video display. Future states are not entirely predictable, so signals must be updated over time. Propagating those updates and transforming the signals are the primary roles of behaviors. It is not possible to observe a signal's history, and consequently signals are constrained by durations of explicit, active observation. For example, a signal describing the position of a joystick is only active while a behavior is actively observing it. Behaviors cannot create or destroy signals, but can manipulate existing signals in flexible ways.

RDP programming directly concerns only the behaviors. An RDP "application" is a behavior analogous to `IO ()` or `void main()`, with trivial inputs and outputs and activated entirely for declarative effects. Signals and resources are not *programmable abstractions* within RDP. They are not first class. However, signals and resources are useful concepts for motivating, understanding, and explaining behaviors. Also, many Sirea users - especially early adopters - will develop adapters to resources and services. Developing these adapters requires a very concrete understanding of the signal and resource models.


Signals and resources are concretely relevant for developing resource adapters and FFI. 

Resources and signals are useful concepts for motivating and understanding the behaviors, but are not programming abstractions. (Signals and resources *are* concretely relevant if developing an adapter between Sirea and an ad-hoc resource, such as a joystick.) An RDP application in Sirea is a behavior that is installed between resources and bootstrapped by the Sirea runtime (e.g. via `runSireaApp`). 

A few short examples of resources, behaviors, and signals:

* A mouse is a resource. A mouse might be accessed by a behavior called `glfwMousePos`. The `glfwMousePos` behavior will respond to a unit-typed input signal (representing a long-lived query, in particular the duration and time of the query) with an output signal (representing the mouse's position, updated over the lifetime of the query). The output signal can be transformed and transported by more behaviors to do something useful, such as influence state that affects a display.

* An SQL database is a resource. An SQL database might be accessed by a behavior called `queryDB`. The input signal might contain a `SELECT * FROM foo WHERE (bar > 10)` query string. The output signal would contain the query result, a list of entries from `foo`. Alternatively, the result might contain some error information, saying that there is no `foo`, or that the connection could not be established. It is possible for both the query string and the output signal to change over time, e.g. to update the demand to `(bar > 12)`, or to receive update indicating changes due a DB update. In general, these changes are independent - i.e. it is possible that two different query strings result in the same data, and it is possible that some database changes are irrelevant to the query result. 

* When RDP programmers need a signal with the constant value 3, they will use `bconst 3`. There is a concept of *"the 3 signal"* - `s_always 3`, which is, was, and forever will be 3. But RDP cannot create the 3 signal. RDP can only transform some existing signal into a 3, maintaining the start and stop time and partition of the existing signal. This protects duration coupling. There are many other operations on pure signals that are illegal for direct use in RDP (fold, mask, etc.), which is why RDP developers do not have direct access to signals.

RDP toes more than one line to achieve its compositional properties and declarative effects. Those lines result in several [software fences](http://www.johndcook.com/blog/2012/09/04/software-fences/) that initially seem limiting to developers unfamiliar with the declarative style. Fortunately, it should not take long to learn idioms and patterns to work within these constraints.

The best way to learn RDP is probably to use it, and to implement examples a few lines at a time. I'll make an effort to provide many example as I make progress in implementing the RDP utilities. The following sections discuss signals, behaviors, and resources more deeply.

Signal Values
-------------

**Note:** RDP developers do not use first-class signals. RDP developers do not use second-class signals. RDP developers do not directly use signals. However, an understanding of signals is essential for understanding RDP. Concrete knowledge of signals is also necessary to adapt Sirea to more external resources. 

A **Signal** is a value. A signal describes another value as it changes over time. Often, the the time-varying value will represent state of an external resource. For example, if I were to continuously observe the state of the "w" key on my keyboard, it would be in an up state most of the time, and in a down state for the brief times I press the button, as when writing "down", "when", or "writing". I could represent this in a signal by mapping the position of the "w" key on a precise timeline.

        ________  _________  ____________  Up ("w" not pressed)
              t1__t2     t3__t4            Down ("w" pressed)

One might represent this discrete-varying signal in a data structure as follows:

        signal= (Up, [(t1, Down), (t2, Up), (t3, Down), (t4, Up)])
                 |   ^ known updates
                 ^ value for all history

Note that the states can be stored as a sequence of discrete updates, but full signal still *represents* a time-varying value. That is, the signal is `Up` until `t1`, then `Down` until `t2`, then `Up` until `t3`, etc.. The meaning of this representation is important. For example, the meaning suggests that the time values had better be in some strictly increasing monotonic order. 

(*Aside:* RDP does not use events. Events very strongly favor an imperative effects model, and require excessive use of internal state [1](http://awelonblue.wordpress.com/2012/07/01/why-not-events/). However, RDP has no difficulty representing short-lived states like the position of a key on the keyboard. Further, an RDP adapter to a keyboard resource may translate a stream of keyboard events into an RDP signal. Sirea will not "miss" any key states while you're observing the keyboard.)

For RDP, the above signal is incomplete. Signals have a lifespan - a definite start time, and indefinite end time. This lifespan is not a property of the keyboard, but rather a property of the *observer* of the keyboard. An observer must start observing at some definite time. In Sirea, lifespan is represented by wrapping every signal value in a `Maybe` type, and use the `Nothing` value to indicate durations where no observation occurs.

            ________  _________  ____________     Just Up ("w" not pressed)
            t0    t1__t2     t3__t4        t5     Just Down ("w" pressed)
        NNNN                                 NNNN Nothing
       
        signal= (Nothing, [(t0, Just Up), (t1, Just Down), (t2, Just Up), 
                           (t3, Just Down), (t4, Just Up), (t5, Nothing)])

The use of `Maybe` allows sequential signals to be combined on a single update stream, and supports efficient implementation of the asynchronous sum type (`x :|: y`) (see below).

**Note:** The `Nothing` value used here does not represent failure. Failure must always be represented by an active signal. E.g. if I want to represent the possibility that the keyboard is inaccessible, then I'd need to use a type like `Either KeyState Error` for the active signal. This is necessary for RDP's *duration coupling* invariant, and is preferable because it allows RDP behaviors to observe failure and take appropriate action.

### Representing Concrete Signals

Above I provided a potential representation for signals:

        (Nothing, [(t0, Just Up), (t1, Just Down), (t2, Just Up), 
                   (t3, Just Down), (t4, Just Up), (t5, Nothing)])

This was actually one of my earlier representations for concrete signals in Sirea, but it has a severe weakness. Consider processing a signal with `bconst 3`. The result is a structure like:

        (Nothing, [(t0, Just 3), (t1, Just 3), (t2, Just 3), 
                   (t3, Just 3), (t4, Just 3), (t5, Nothing)])

Here, all but the first and last of these updates are redundant. This has a space cost, but it also has consequences for downstream processing (e.g. to zip this signal with another). Efficiency can be improved by filtering the redundant updates. The same signal would be more efficiently represented as:

        (Nothing, [(t0, Just 3), (t5, Nothing)])

Assuming `t0` and `t5` are very close to one another, they can be filtered from the signal with no ill effects. However, in the general case the `t5` value is indefinite. Attempting to compute the first "change" after `t0` can take an unbounded amount of time, which would be in violation of Sirea's soft real-time goals. 

To support filtering, Sirea uses a more sophisticated structure in place of the list:

        newtype DSeq a = DSeq { dstep :: (T -> DStep a) }

        data DStep a 
            = DSDone
            | DSWait !(DSeq a)
            | DSNext {-# UNPACK #-} !T a !(DSeq a)


The `DSeq` constrains the search for the next change based on the given `T` argument. `DSNext` and `DSDone` correspond to a list element and end-of-list respectively. `DSWait` indicates that the next update (if one exists) is beyond the upper limit of the search, and returns the partially processed signal. This structure allows incremental processing and filtering of signals.

The concrete signal type in Sirea is `Sig a`,

        newtype Sig a = Sig !(Maybe a) !(DSeq (Maybe a))

### Updating Signals

RDP is generally push-based in the implementation. I.e. push updates for an SQL query signal, get query results pushed back. 

Sirea will push signal updates through the behavior network. When RDP updates a signal, it updates the whole projected future of that signal. In the degenerate case, this might mean updating one value at a time. However, it is not unusual that multiple future values can be anticipated.

        data SigUp a = SigUp 
            { su_state  :: !(Maybe (Sig a , T)) -- signal future after given time
            , su_stable :: !(Maybe T)           -- update stability; like a heartbeat
            }

The `su_stable` associated with a signal update is a promise that future update times have a lower bound set by the stability value, where `Nothing` is forever (indicating the signal has received its last update). The main role of update stability is to support garbage collection of cached signals. (Caches are necessary when signals are combined, e.g. with `bzip` or `bmerge`, since the different signals may update at different times.)

at `bzip`, `bmerge`, `beval`, and other behaviors that must internally cache signals. A secondary role of update stability is a rough estimate of real-time for the less time-sensitive operations; it's a lot cheaper and more convenient than looking up a wall-clock time, and sets a bound for the amount of rework a system might need to perform. 

The `su_state` indicates the signal has a new value starting at a given time, or `Nothing` if there was no change in the signal state (just a stability update). The task of applying a `SigUp a` to a `Sig a` is rather trivial, just an `s_switch` at the given time. It is very common for state and stability updates to piggyback. 

Signal updates have several nice properties in addition to those of general message passing:

* updates are idempotent; useful over a shoddy network connection
* updates are composable by "piggybacking"; useful when fast source feeds slow consumer.
* updates allow takebacks (limited by stability); optimistic concurrency, eventual consistency
* updates can target future; masks variability in latency, simplifies scheduling, avoids rework
* garbage collection is built into the signal update model; no need to estimate live references

These are in addition to the many benefits associated with anticipation and speculative evaluation. 

In Sirea, a behavior is compiled into a network with all signals inactive. The first signal update will activate the behavior.

### Continuous Signals?

There are many problem domains where support for continuous-varying signals would be useful: motion, sound, animation, charting, physics, probability. Sirea currently supports only discrete-varying signals, which is quite suitable for digital information (boolean values, integers, text). However, RDP in general can support piecewise-continuous signals. 

I am interested in eventually developing support for continuous signals, most likely in a transformative layer above Sirea RDP. 

* Upon `bdelay` or `bpeek` need to time-shift the signal contents. 
* Instead of `bsplit` need some support for zero-crossings. 

Viable candidates include [trigonometric polynomials](http://en.wikipedia.org/wiki/Trigonometric_polynomial) or [plain old polynomials](http://en.wikipedia.org/wiki/Polynomial). These models are symbolic, can be represented as a simple vector of coefficients, are easy to serialize or utilize on a GPU, and multi-dimensional variations are feasible. Use of Haskell functions (`T -> a`) is also feasible, though opaque and full of semantic garbage (such as discontinuous signals).

Signals in Space and Time
-------------------------

Concrete signals are effective for tightly coupled values that are processed and updated together. But more structure is necessary for flexibility, efficiency, and scalability. Sirea uses three structural notations for complex signals.

1. `(S p a)` - represents a concrete signal (`Sig a`) located spatially at partition `p`. Unless subjected to dead code elimination, this results in a signal value being represented at partition `p`. Use of the `p` type prevents implicit communication of signals between partitions. Developers must use `bcross` or similar to cross partitions, which simplifies Sirea's communication model and improves its efficiency. 

2. `(x :&: y)` - describes an asynchronous product of signals. Both elements, `x` and `y`, are active at roughly the same time, depending on differences in delay. Both have the same active durations. By modeling a product in different partitions, i.e. `((S p1 a) :&: (S p2 b))`, an asynchronous product can model a distributed computation.

3. `(x :|: y)` - describes an asynchronous sum of signals, like a switching network. In general, the `x` and `y` are active at different times, but differences in latency on the different branches can cause brief overlap for transition from the faster to the slower. The active duration is split between `x` and `y`. 

The _asynchronous_ property is a consequence of constructs like `bleft (bdelay 0.02)` or `bfirst (bdelay 0.01)`, resulting in component signals having different latencies. These are _logical_ delays, achieved by increasing the `su_state` and `su_stability` times, and thus have no significant performance consequence. Modeling of delays is important semantically, both to dampen feedback loops and to allow signals time to propagate through a complex behavior. This is a different notion of asynchrony than is common to multi-threading, promises, actors. RDP can model the latter form of asynchrony by use of shared state communication, e.g. blackboard models, task queues. But latencies for asynchronous products and signals are statically known, making them suitable for real-time systems.

Use of `bsynch` will logically synchronize complex signals by logically delaying the lower-latency concrete signals. This results in signals being logically aligned. Since this is static, it works even if they are distributed across multiple partitions. Logical synchronization is very efficient, and can be more precise than state-based synchronization. Whether logically synchronous operations will be physically synchronized depends on the ability to synchronize clocks or at least estimate clock offsets.

There are two special signals, which were developed to better fit various theoretic models. 

* **S0** - a signal that is useless because it does not exist
* **S1** - a signal that is useless because is stuck in an abstract partition

While RDP developers cannot destroy signals, they can dump them someplace irretrievable (**S1**). Similarly, while RDP developers cannot create signals, they can invent unused signal types in a choice (**S0**). Most Sirea users will stick to `binl` or `bfst` rather than mess with these types.


Behaviors
---------

Behaviors describe computation-rich dataflow networks that manipulate signals. A behavior will be compiled and installed, establishing a *concrete instance* of the behavior, a concrete dataflow network. Installing a behavior has no effect other than to prepare a few unique names and caches. The initial signal is inactive. Once installed, a behavior is activated by signal update. 

Some behaviors provide access to effectful resources such as sensors, actuators, or shared state. For concrete instances of those behaviors, updates to the input signal are often independent of updates to the output signal. E.g. one does not need to update a query signal for a mouse or keyboard or video camera to receive many updates to a signal representing observed values. Conversely, a stateful resource or an `OK|Error` signal from an actuator might be very stable despite many changes to inputs. Effects on resources are declarative, based on the *set* of active signals at any given time.

Different resources tend to update at different times and frequencies. Use of the complex signal types `(x :&: y)` and `(x :|: y)` enable developers to describe subnets that update at different rates. This is critical for efficiency. Developers can easily control and reason about which behavior subnets might be updated based on changes upstream. By arranging for some signals to be more stable (e.g. by utilizing an intermediate state resource) developers can protect subsystems that cannot handle rapid or short-term variations in input.

A Sirea RDP "application" is a behavior whose activation is bootstrapped by `runSireaApp`. The behavior has trivial signal types, and thus is activated only for its declarative effects. Otherwise, there is nothing special about the application behavior. Developers can easily pretend the application behavior is dynamically activated by an even larger behavior, or may even compose it into a larger behavior. 

RDP is metacircular: it's behaviors *"all the way down"*.

### Behaviors as Dataflow Networks

Behaviors are composed as dataflows across an open system. Abstractly, those dataflows are signals. For the concrete, installed behavior, those dataflows are signal updates (of type `SigUp`). 

The simplest dataflow is linear and is appropriately called a "pipeline". The constructor for pipelines is `>>>`. This operator is from `Control.Category`, where `(>>>) = flip (.)`. Composition with `>>>` is associative, chainable, and models dataflow from left to right. This results in code that is relatively read and document with English.

        (>>>) :: (Category b) => b x y -> b y z -> b x z
        f >>> g >>> h -- a pipeline of three behaviors

Developers may be familiar with pipelines from using command line interfaces, modern graphics processing, or other flow programming models. However, many dataflow systems are based on *streams* rather than *signals*. A signal describes one value as it varies over time. Streams are not so constrained, often representing iteration over collections or sequence of independent events. (It is not difficult to model a signal in a stream, or a stream in a signal. But it still takes state and labor. The default has a significant impact on programmer experience.) In Reactive Demand Programming, these pipelines will often represent continuous, effectful observation and influence of time-varying values. For example:

        load "myImage.svg" >>> renderJPG >>> save "myImage.jpg"

This behavior, while active, would *continuously* load "myImage.svg", render it to a JPG value, then save that result to "myImage.jpg". Of course, this is continuous only in the logical, abstract sense; the implementation would only fully load, render, and save when first activated and when "myImage.svg" changes. (*Note:* To activate this behavior, it must be part of a larger behavior. 

The notion of behaviors as dataflow *"networks"* is due to use of the complex signal types. Each concrete signal type `(S p a)` will have its own stream of signal updates (`SigUp`). Different concrete signals may also be distributed across partitions. The signal type `(S P1 x :&: S P2 y)` describes a pair of concrete signals distributed across two partitions. Composition of distributed signals `(x :&: y)` involves composing `x` and `y` independently. This is supported by three primitive behaviors:

        bfirst   :: (BProd b) => b x x' -> b (x :&: y) (x' :&: y)
        bswap    :: (BProd b) => b (x :&: y) (y :&: x)
        bassoclp :: (BProd b) => b (x :&: (y :&: z)) ((x :&: y) :&: z)

The `bfirst` constructor allows operating on just the `x` signal, while `bswap` and `bassoclp` allow ad-hoc dataplumbing to shift any subset of `(:&:)` signals into the `x` position. Above these, several utility functions can be built, such as `bsecond`, `bassocrp`, and the `(***)` operator:

        bsecond  :: (BProd b) => b y y' -> b (x :&: y) (x :&: y')
        bsecond f = bswap >>> bfirst f >>> bswap

        bassocrp :: (BProd b) => b ((x :&: y) :&: z) (x :&: (y :&: z))
        bassocrp = (bfirst bswap >>> bswap) >>> bassoclp >>> (bfirst bswap >>> bswap)

        (***)    :: (BProd b) => b x x' -> b y y' -> b (x :&: y) (x' :&: y')
        f *** g = bfirst f >>> bsecond g

Reactive Demand Programming provides many useful declarative properties. Developers can consequently collapse complex constructs into simple, equivalent concepts. I don't wish to start proving theorems in this README, but developers can reason that `(f1 *** g1) >>> (f2 *** g2)` is equal to `(f1 >>> f2) *** (g1 >>> g2)` due to associativity of `(>>>)` and spatial commutativity. 

        __f__   __g__   ______     __f__     ______   __g__   __f__
             \ /     \ /                           \ /     \ /
        bswap-X       X         =  (***)  =         X       X
        _____/ \_____/ \______     __g__     ______/ \_____/ \_____

        bfirst f >>> bsecond g  = f *** g  = bsecond g >>> bfirst f -- spatial commutativity
        f >>> (g >>> h) = (f >>> g) >>> h

        __f1__     __f2__     __f1 >>> f2__
              \   /  
        (***)  >>>  (***)  =      (***)
        __g1__/   \__g2__     __g1 >>> g2__

        (f1 *** g1) >>> (f2 *** g2) = (f1 >>> f2) *** (g1 >>> g2)
        bswap >>> bswap = id
        bfirst f1 >>> bfirst f2 = bfirst (f1 >>> f2)

These properties make it easy to understand `(***)` as describing concurrent composition of pipelines. These networks can be much more complex, e.g. `(foo *** ((bar *** baz) *** qux))` would describe four concurrent behaviors on potentially distributed signals. Use of `bswap`, `bassoclp`, and `bassocrp` also allow developers to treat `(***)` as at least conceptually associative and commutative. Those operations are explicit because Haskell encodes the current permutation and association of signals in the type system. 

To initially construct a concurrent dataflow, one will generally use the behavior `bdup` to duplicate a signal. Reactive Demand Programming cannot create a signal from scratch, but can duplicate a signal. Duplicating a signal will also duplicate the signal updates. (The branches won't begin to update *independently* until interacting with other resources.) Conversely, one can terminate a pipeline with `bfst`. 

        bdup  :: (BProd b) => b x (x :&: x)
        bfst  :: (BProd b) => b (x :&: y) x

Use of `bdup` results in the `SigUp` streams being duplicated. They are not yet independent signals. However, after applying the first and second pipes to different resources, e.g. to gather different state, they may diverge radically on update rates. Duplicating signals then using them in different ways enables developers to build complex dataflow networks. 

More utility behaviors are derived from `bdup` and `bfst`:

        (&&&) :: (BProd b) => b x y -> b x z -> b x (y :&: z)
        f &&& g = bdup >>> (f *** g)

        bsnd  :: (BProd b) => b (x :&: y) y
        bsnd = bswap >>> bfst

        bvoid :: (BProd b) => b x y -> b x x
        bvoid f = bdup >>> bfirst f >>> bsnd

        (|*|) :: (BProd b) => b x y -> b x z -> b x x
        f |*| g = bvoid f >>> bvoid g

Those last two constructors are especially useful: they allow behaviors to conveniently represent **declarative statements**, i.e. behaviors that exist only for their declarative effects. This is a close analog to the traditional imperative statements, which are executed for imperative side-effects. Declarative statements have nice properties that imperative statements do not, especially being commutative (`bvoid f >>> bvoid g = bvoid g >>> bvoid f`; `f |*| g = g |*| f`) and idempotent (`bvoid f >>> bvoid f = bvoid f`). Declarative statements are also associative and have identity, but those properties are common to imperative. 

Sirea performs its own compilation of behaviors, which will eliminate most data plumbing and perform dead-sink eliminations. (E.g. `bdup` will only duplicate the specific concrete signals that are needed on both paths.)

### Behaviors as Multi-Agent Systems

Many systems can be conveniently understood in terms of multiple agents operating in a shared environment. Each agent continuously observes its environment and may attempt to influence the environment or (through environmental resources and a [blackboard metaphor](http://en.wikipedia.org/wiki/Blackboard_system)) the other agents. Depending on the agents, behaviors may be cooperative or competitive. 

In RDP, the `|*|` operator introduces a behavior that can only be observed through its effects on the environment. This, effectively, describes a composition of agents for a multi-agent system. Consider the following behavior:

        let buildImage = load "myImage.svg" >>> renderJPG >>> save "myImage.jpg" in
        let displayImage = load "myImage.jpg" >>> displayJPG in
        buildImage |*| displayImage

Here, developers can justifiably understand `buildImage` as an agent with a very specific duty. The same developer, in a different context, might understand `buildImage` as a pipeline. This flexibility is useful; which understanding serves best will depend on context. The same can be said of `displayImage`. Multi-agent systems work best when the agents do not name directly interact or name one another: this keeps agents disentangled, and provides developers much freedom to add, remove, or replace agents. However, multi-agent systems often interact indirectly through shared resources in an open system . In this case, `buildImage` and `displayImage` interact indirectly, through the filesystem.

Sirea encourages the agent view at the higher level. The Sirea.AgentResource module simplifies breaking a complex application into one agent per task. A typical `SireaApp` might be a composite of toplevel agents.

### Behavior Switching Networks

In addition to the `(:&:)` typed signals, Sirea supports `(:|:)` typed signals. Behaviors that process these signals describe switching networks, where only one subnet is active at a time based on a decision upstream (typically via `bsplit`). Most of the basic behaviors for `(:&:)` have a dual for `(:|:)`:

        bleft    :: (BSum b) => b x x' -> b (x :|: y) (x' :|: y)
        bmirror  :: (BSum b) => b (x :|: y) (y :|: x)
        bassocls :: (BSum b) => b (x :|: (y :|: z)) ((x :|: y) :|: z)
        bmerge   :: (BSum b) => b (x :|: x) x
        binl     :: (BSum b) => b x (x :|: y)

        bright   :: (BSum b) => b y y' -> b (x :|: y) (x :|: y')
        bassocrs :: (BSum b) => b ((x :|: y) :|: z) (x :|: (y :|: z))
        (+++)    :: (BSum b) => b x x' -> b y y' -> b (x :|: y) (x' :|: y')
        (|||)    :: (BSum b) => b x z  -> b y z  -> b (x :|: y) z
        binr     :: (BSum b) => b y (x :|: y)

With product `(:&:)`, sum `(:|:)`, and unit `(S p ())`, developers can represent arbitrary data types - booleans, composition of booleans into 32-bit numbers, and so on - without relying on a second paradigm. Of course, this would be dreadfully inefficient on modern architectures (without a lot of high-level optimizations). 

Developers must also support interactions between the `(:&:)` and `(:|:)` types. The two basic interactions involve factoring a signal that occurs regardless of choice (`bconjoin`), or distributing a signal across a choice (`bdisjoin`). Conjoin is quite trivial - duplicate, filter, and merge. Disjoin, however, requires a little extra attention. 

        -- factor `x` from a decision
        bconjoin :: (BSum b, BProd b) => b ((x :&: y) :|: (x :&: z)) (x :&: (y :|: z))
        bconjoin = (getX &&& getYZ)
            where getX = (bfst +++ bfst) >>> bmerge
                  getYZ = (bsnd +++ bsnd)

        -- distribute `x` across a decision
        bdisjoin :: (BDisjoin b, SigInP p x) => b (x :&: ((S p () :&: y) :|: z)) ((x :&: y) :|: (x :&: z))

For disjoin, the `(S p ())` signal represents the decision. This is necessary because `y` and `z` may be complex signals distributed across arbitrary partitions. The `SigInP p x` indicates that `x` is fully in partition `p`. (Sirea avoids any implicit communication between partitions.) The implementation duplicates each concrete signal in `x` then masks it with the decision signal for the left, and the inverse decision signal for the right.

Between disjoin, dup, and merge, a few familiar utility functions can be defined: `bWhen`, `bUnless`, `bIfThenElse`.
 
Sadly, there is no useful dual to the multi-agent concept. The dual to `bvoid` (`bskip :: b y x -> b x x`) seems useless, except as a way to mark dead code.

### Haskell Functions as Behaviors

Sirea provides easy access for applying Haskell functions and propagating Haskell values:

        bfmap  :: (BFmap b) => (x -> y) -> b (S p x) (S p y)
        bconst :: (BFmap b) => c -> b (S p x_) (S p c)
        bzap   :: (BZip b)  => b (S p (x->y) :&: S p x) (S p y)
        bsplit :: (BSplit b)=> b (S p (Either x y)) (S p x :|: S p y)
         
        bzipWith fn = bfirst (bfmap fn) >>> bzap
        bzip = bzipWith (,)
        bsplitOn fn = bfmap fn' >>> bsplit
            where fn' x = if (fn x) then Left x else Right x

These functions can support gathering and transforming data as needed to influence resources. Though, some caution is warranted: RDP is designed for real-time systems. Developers should constrain themselves to functions that are either real-time or close enough. Long-running or batch-processing functions should be modeled as an explicit process manipulating a stateful resource.

Eventually, I would like to compile behaviors for partitions outside the Haskell process, e.g. for client-side JavaScript, CG shaders, or GPGPUs. This can still be achieved, but it will take an extra layer that only partially compiles into Sirea to constrain access to opaque Haskell functions and types. 

### Staged Programming with Dynamic Behaviors

Useful applications can be developed by composing a static set of behaviors in myriad ways. But dynamic behaviors enable so much more: discovery of resources, open extension, service brokering, runtime upgrade, live programming, dynamic reconfiguration, staged metaprogramming, capability security patterns (grant, attenuation, revocation), and flexible adaptation to a changing environment.

A behavior that allows dynamic behaviors is `beval`. Evaluation in Reactive Demand Programming is a continuous act, so what `beval` actually does is compile and install a dataflow network at runtime. The installed behavior will change over time, dynamically, based on a signal. Ideally, `beval` would look like this:

        beval :: (BDynamic b) => b (S p (b x y) :&: x) y -- IDEAL

This says that behavior type `b` can install a time-varying instance of the same type, supply some inputs, and get some outputs. Unfortunately, this ideal model has three weaknesses: it does not indicate static latency between `x` and `y`, it does not work nicely for behavior transforms, and it may require implicit communication between partitions (if `x` is not in `p`). To cover these weaknesses, here is the actual `beval` behavior:

        beval :: (BDynamic b b', SigInP p x) => DT -> b (S p (b' x y) :&: x) (y :|: S p ()) -- ACTUAL

The `DT` value provides a static latency for the behavior, and the `S p ()` output indicates an error, which happens if you try to evaluate a behavior that is larger than the given `DT` value. (I'd prefer to use dependent types for time, but those aren't trivial to model in Haskell.) The `b'` inner type supports behavior transformers or layers. The `SigInP p x` constraint ensures all the `x` signals are in the same partition as the behavior signal. 

Dynamic behaviors can serve several of the same roles as `(:|:)` signals, but `(:|:)` is more openly composable and may sometimes offer performance advantages. No best practices are yet established for when to use sums vs. dynamic behaviors for RDP, but for a small number of commonly used, statically known choices it will probably be better to favor sums.

*NOTE:* Sirea uses speculative evaluation to compile and install dynamic behaviors a few seconds before they are necessary. Thus, even though `beval` indicates only one behavior as active, in practice multiple behaviors are installed and active, computing recent past and near future in parallel. If the dynamic behaviors are relatively stable - and assuming the CPU can keep up with the higher burden - this can reduce latencies experienced by the user. 

**CAUTION:** Compared to first-class functions in Haskell, dynamic behaviors in Sirea are relatively expensive and lack effective cross-level optimizations. Dynamic behaviors for Sirea should be relatively stable, such that the expense of changing behaviors is amortized over time. Use dynamic behaviors where necessary for expressiveness, but use them sparingly. 

### Spatial Behaviors

Resources are often bound to a partitions. Broadly, the notion of an RDP "partition" might extend to remote services, tabs in a browser, or particular GPUs. Reactive Demand Programming is designed for *automatic code distribution*. An RDP application behavior is *compiled* into shards (subprogram behaviors) that are distributed across many different partitions. The shards can efficiently influence and observe resources local to that partition. RDP addresses historically legitimate fears of hosting untrusted code in three ways:

* RDP supports [object capability security](http://erights.org/elib/capability/ode/ode-capabilities.html) patterns. Capabilities enable shards to carry very fine-grained proof of authority, and they separate the concern for how the shards obtained those capabilities in the first place.
* In RDP, capabilities are dynamic behaviors, which are provided by signals, which are implicitly traceable and revocable. There is never any risk in an RDP system of authorities being "grandfathered" into the system due to lack of visibility or foresight. The cause for every grant can be traced to a particular agent or behavior. Administrators have a free license for easy takebacks. (This is augmented further by RDP's "open loop" nature: resources in RDP are external, and external resources are easy to extend system with new diagnostics, alerts, observations.)
* RDP is designed for real time systems. The space-time costs associated with any behavior or shard are statically predictable. This is important from a security perspective to control against denial-of-service or buggy code that enters infinite loops. RDP developers who need infinite loops must use external state resources, which are easy to diagnose.

Reactive Demand Programming has strong potential to abstract, install, and extend [overlay networks](http://en.wikipedia.org/wiki/Overlay_network) with very little effort. *This is a primary design goal of RDP*.

Unfortunately, Sirea does none of these things! For convenience with Haskell's module system, Sirea uses type-driven resource access, which is effectively an ambient authority (unless types are propagated in certain ways). Sirea does not enforce real time anything (though does encourage soft real time). 

Sirea's scope is limited to the Haskell process. In Sirea, the notion of a *partition* equates to a humble Haskell thread. The binding of resources to threads in Haskell is mostly for convenience (e.g. for easier state resources, organization) and occasionaly for safety (e.g. for FFI resources, OpenGL TLS, GLFW event queues, etc.). But within this limited scope, Sirea does model *automatic code distribution*. Partitions are named by type. A concrete signal `(S Foo x)` specifies a signal exists in partion `Foo`. A unique thread for that partition will be created (based on a `Partition` typeclass) when the partition is needed. 

The `bcross` behavior allows signals to move between Sirea partitions:

        bcross :: (BCross b, Partition p, Partition p') => b (S p0 x) (S pf x)

The *automatic code distribution* aspect is obvious when some behaviors are presented as occurring in another partition than the one you started in. Consider `foo >>> bcross >>> bar`. Here, the `bar` behavior may be computed in a different partition than the partition in which `foo` was computed. Information bout `bar` must be delivered to the second partition.

While Sirea is currently constrained quite severely to live within one Haskell process, I would like to eventually explore lifting Sirea to operate in open distributed systems. This can potentially be achieved by using higher level behaviors that *compile* to a mix of Sirea-local and remote behaviors (where the remote shards are more constrained). Since I'm considering something similar to support client-side RDP for sirea-webapps (pages as dynamic behaviors, compiling some code to JavaScript for a client browser) I am considering to provide a limited variation of this feature as part of sirea-webapps. 

Even without distributed behaviors, Sirea can interact with distributed resources. This is achieved by modeling a *local proxy* to a remote resource. The proxy can expose failure modes, e.g. when the network is not available. 

### Temporal Behaviors

It takes time for signals to propagate through a behavior. If you twiddle a few bits in the input signal, it might be microseconds to milliseconds before you see bits twiddling in the output. Short term, it is often convenient and harmless to pretend that the latency is zero. But, as application behaviors grow in scale, it can prove harmful: the physical latency grows faster than logical latency, and signal updates *straggle* through the network, falling behind real time. Eventually, you get signal updates telling you what you should have done many milliseconds ago, when it is too late to handle them. 

Those straggling updates can be used for retroactive corrections to state, providing RDP a degree of eventual consistency. But for many effects - e.g. sounds played, images rendered - a signal too late is no good.

Use of explicit, logical delay provides developers more control over their RDP computations.

        bdelay :: (BDelay b) => DT -> b x x

The behavior `bdelay` will delay a signal logically. It achieves this by adding the given `DT` to the update and stability times for every `SigUp`. Fundamentally, the idea is to turn *straggling* updates into *future* updates: instead of an update too late, the update might arrive a little early (based on the distance between logical and real time). Future updates are subject to speculative evaluation, anticipation, and buffering. The delay can mask variance in physical latency, hiding scheduler hiccups and similar.

Choosing too large a `DT` value has its own disadvantages: most reactive or real time systems have a tight latency budget, beyond which the quality of the system rapidly degrades. Also, large `DT` results in larger buffers and memory overheads. So developers in RDP are under some pressure to specify delays that are large enough but not too large. Fortunately, developers do not need to be especially precise or accurate; most resources are robust to an occasional straggler, and the window for "good enough" is quite large in most problem domains. In many cases, computation latencies can be entirely subsumed by communication latencies.

The `bdelay` behavior be applied to components signals in a complex signal type (e.g. `bfirst bdelay 0.01` to delay the first signal 10 milliseconds). This is why RDP has *asynchronous products and sums* for its signal types. But when signals become asynchronous, it is often desirable to synchronize them for an operation. Some operations in Sirea will implicitly synchronize: `bzip`, `bmerge`, or `bdisjoin`. But a behavior for an explicit synchronization barrier is also available:

        bsynch :: (BDelay b) => b x x

The `bsynch` behavior will take every concrete signal in `x` and delay it to match the signal with the highest logical latency. This allows developers to synchronize signals without being explicitly aware of their latencies. Synchronization barriers can be very useful for controlling side-effects, ensuring multiple resources and subsystems operate in concert.

### Anticipation

Reactive Demand Programming offers an interesting feature: since each signal update (`SigUp`) carries a potentially rich future for the signal, it is easy to peek forward a little and obtain a decent guess of what the future holds. The ability to look forward even a few milliseconds can enable more intelligent decisions. For example, given a series of commands for a robotic arm, one can use knowledge of upcoming commands to select a smoother path. By comparing a value to its future, one can detect change-events and gestures without need for a state resource. The behavior to look into the future is `bpeek`:

        bpeek :: (BPeek b) => DT -> b (S p x) (S p (Either x ()))

The `DT` value is how far ahead to look, e.g. `0.003` for three milliseconds. The `Either x ()` answer is either a future `x` or an indicator `()` that the signal will be inactive three milliseconds hence. 

This power comes with a price: the further you look, the less stable and less accurate your answers. Technically, one can understand this as looking into a buffer, where the buffer is still being updated: future updates become straggling updates. However, use of `bdelay` can directly counter the stability costs, trading latency for stability. For example:

        bdelay 0.03 >>> bpeek 0.03

Would create a 30 millisecond buffer and allow the developer to peek into without any cost to stability, albeit with a significant 30 millisecond cost to latency. The motivation for such a construct might be to avoid need for a stateful resource. An anticipation buffer is much less expressive than state, but can be used for some roles where state is traditionally necessary. Using anticipation, one can detect changes or take derivatives, but cannot accumulate values or compute integrals. By avoiding state, the system is more robust and resilient to partial failure. Avoiding state is not always the right answer, but it's worth considering in many cases.

RDP does not provide a prediction system. It only propagates updates. However, prediction systems can be built using state resources, especially animated state (which models systems with inertia). I expect Sirea will eventually have packages that cover common prediction models based on machine learning.

### Behavior Implementation and Extension

A behavior is compiled into a concrete dataflow network that uses Haskell `IO` for propagation. This network is then activated by signal updates of type `SigUp`. Signal updates are also used to deactivate the behavior, i.e. by indicating the signal is inactive. The use of `bcross` allows different parts of a behavior to compute in different partitions, where each partition is represented by a different Haskell thread. 

Between partitions, signal updates are batched into one outbox for each destination. The contents of those outboxes are delivered atomically at the end of the round. At the start of each round, ALL available batches are processed. Multiple updates for one signal will "piggyback" (be combined into one update). A bounded-buffer mechanism is applied at the batch-level between partitions, ensuring fairness and controlling space costs.

Round-based batch processing ensures an atomic "snapshot" view of other partitions within and between rounds. Snapshot consistency is weaker than "glitch freedom": by observing one resource through lenses of two different partitions, temporary inconsistency is possible. But snapshot consistency eliminates "local" glitches, which are the cause of most problems.

Within each round, updates are processed in two phases. First, every link with a pending update is *"touched"* to indicate an update will be coming on that link. Then, every update is delivered. This is a dynamic solution to the update ordering problem. The *update ordering problem* is a performance concern for push-based reactive systems with complex or ad-hoc acyclic dependencies. For example, assume a system whose dependency graph looks like:

        a--b--c--d--e--k
         \/ \/ \/ \/  /
         /\ /\ /\ /\ /
        f--g--h--i--j

In this graph, dependencies flow left to right along the lines. For example, `c` depends on `b` and `g`. Assuming there are pending updates on `a` and `f`, there are multiple orderings in which the updates could be processed:

        a,f,b,g,c,h,d,i,e,j,k -- an optimal update ordering
        a,b,h,d,j,k,c,i,e,k,g,c,d,e,k,f,b,h,d,j,k,g,h,c,d,e,i,e,j,k -- a non-optimal ordering

I do not present a worst-case ordering. The worst-case ordering is exponentially worse than the best case. If you know the dependencies, a [toplogical sort](http://en.wikipedia.org/wiki/Topological_sorting) will provide an optimal ordering. However, for RDP, the dynamic behaviors and openly shared resources (state, demand monitors) make it difficult to determine the dependencies. The *"touch"* technique used by Sirea is a simple, dynamic solution to the problem: a "touch" is a promise of a pending update. When `g` is touched by `a` and `f`, `g` knows to wait for an update from both before pushing its own update. By propagating all touches before propagating any updates, the update ordering will be optimal.

The use of "touch" is reflected directly in the data type for constructing concrete behavior networks. Each link (`LnkUp`) has two channels: one for touch, one for the signal update: 

        data LnkUp a = LnkUp
            { ln_touch  :: IO ()
            , ln_update :: SigUp a -> IO ()
            }

Within each partition, this is used in the two-phase approach described earlier: all incoming signals are touched (and updates for the same signal are combined to piggyback), then all updates are propagated, achieving an optimal update ordering with only one update to any given signal during a round. Touch is used only within each partition; `bcross` will start a new set of touches in the next partition. 

Between partitions, batches as a whole will often process in non-optimal orders. However, the tendency to accumulate and process multiple batches together will strongly resist the worst-case orderings. 

Developers will use the `LnkUp` type directly if extending Sirea for new resources. This is achieved with `unsafeLinkB` from Sirea.Link. Behaviors in Sirea are compiled in two passes, forward then back. The forward pass tracks timing and computes `bsynch`. Developers extending Sirea hook into the reverse pass:

        unsafeLinkB :: (Lnk y -> IO (Lnk x)) -> B x y
  
        data Lnk x = -- GADT
            LnkDead :: Lnk a
            LnkSig  :: LnkUp a -> Lnk (S p a)
            LnkProd :: Lnk x -> Lnk y -> Lnk (x :&: y)
            LnkSum  :: Lnk x -> Lnk y -> Lnk (x :|: y)

The `Lnk` type can carry `LnkUp` values for each concrete signal, and can also carry multiple concrete signals in case of `(x :&: y)` or `(x :|: y)`. The `LnkDead` indicates dead code on output, which can help optimize by eliminating cases where a resource is query-only. Some `IO` is allowed during construction, though it is intended for resource management and signal caching (no semantic state, no observable effects). 

Type `B` is the concrete, primitive behavior type implemented by Sirea. Type `B` is inconvenient to use directly because it is unclear where resources would be represented other than the Haskell global space, which would be problematic. Sirea provides a higher level behavior type `BCX` to mitigate this by reifying a toplevel context object `PCX`. Operations such as `bcross` depend on the `PCX` value to track partition threads and mailboxes. The Sirea application behavior is of type `BCX`, with Sirea supplying the initial `PCX`.

        newtype BCX w x y = BCX (PCX w -> B x y) 
        unsafeLinkBCX :: (PCX w -> Lnk y -> IO (Lnk x)) -> BCX w x y
        type SireaApp = forall w . BCX w (S P0 ()) (S P0 ())

These `unsafeLink` operations are unsafe from Sirea's perspective (not Haskell's). It is far too easy to violate RDP's sensitive constraints. Discipline is needed by the developer to safely extend Sirea. This is, at the moment, one of Sirea's weaknesses, though support patterns such as AgentResource and UnsafeOnUpdate can help.

### Parallelism and Performance

Sirea RDP is an excellent target for parallelism in Haskell for a few reasons:

* the `(x :&: y)` asynchronous products clarify data dependencies. This makes it easier for developers to judge where introducing parrallelism is advantageous. Very little parallelism will be wasted. 
* the concrete signal type `(S p a)` (implemented by `Sig a`) provides a clean separation between the spatial and temporal aspects of data. It is easy, in Sirea, to specify that the spatial computations occur in parallel while the temporal "spine" of the signal is left to incremental, sequential processing.
* the use of signal updates with explicit stability (`SigUp`) supports speculative evaluation and optimistic concurrency, similar to lightweight time warp protocols. (Parallelism via optimistic concurrency is really trading efficiency for latency and robustness, but is a very nice way to use that extra CPU.)
* use of `bcross` models distributed behaviors, potentially multi-processor systems, which are inherently parallel. 
* state resources are explicit and external to RDP, and persistent; there is no risk of lazy, parallel folds becoming space leaks.

Sirea provides several convenience functions for developers to control the timing of computations on data:

        bstrat :: (BFmap b) => b (S p (Eval a)) (S p a)
        bseq :: (BFmap b) => b (S p a) (S p a)
        bspark :: (BFmap b) => (x -> ()) -> b (S p x) (S p x)
        bforce :: (BFmap b) => (x -> ()) -> b (S p x) (S p x)
        bstratf :: (BFmap b, Functor f) => (forall e . f e -> e) -> b (S p (f x)) (S p x)

`bspark` is the most accessible mechanism to add parallelism to a Sirea behavior. It will spin off one spark for each value in a signal, but for only a few values at a time (incrementally, relative to stability), and downstream clients will wait on this computation to finish (as though it were strict). This eliminates risk of losing parallelism, but has the disadvantage of not being very compositional. The reduction `(x -> ())` is any sequential strategy, usually `rnf` from Control.DeepSeq. The `bforce` behavior is similar but will evaluate in the current partition's thread. A potentially useful idiom is to use `bspark` just before `bcross`, such that lazy values are computed "in transit".

The `bstrat` and `bseq` behaviors are more primitive. The `bstrat` will not directly evaluate anything, but will prepare the inner `Eval a` to evaluate when later computing the signal up to `Just a | Nothing`. The `bseq` behavior will, over time, incrementally evaluate the signals up to `Just a | Nothing`. These are usually paired. The `Eval` type is an identity monad from Haskell's [parallel](http://hackage.haskell.org/package/parallel) package. If developers desire to use another pure evaluator type, such as [Monad.Par](http://hackage.haskell.org/package/monad-par), they might simply use `bstratf runParAsync`.

The above behaviors are a Sirea user's primary tools to control the whens and wheres of lazy evaluation. But lazy evaluation is not the only performance concern. Other concerns involve caching, memoization, choking signals, and eliminating redundant updates. 

At the moment, Sirea unfortunately lacks standard idioms for memoization and caching. The traditional Haskell tricks for memoization (e.g. expressed in Luke Palmer's [memocombinators](http://hackage.haskell.org/package/data-memocombinators) package) can certainly be applied, but I fear those may accummulate too much memory over time. I've been exploring designs for windowed memoization, possibly using history with exponential decay (via a sequence or ringbuffers).

The notion of *choking signals* involves skipping high-frequency intermediate updates to a signal and showing only the occasional snapshot. This is a very different notion from filtering signals. Choking is inherently a stateful concept because it must hold onto information about the past (at the very least, when the last snapshot was selected). The motivation for choking is usually to either dampen feedback cycles (a dubious application) or to support a fast producer in feeding a slow consumer. In Sirea, choking will be modeled always with support of intermediate stateful resources, and I'm considering creating a dedicated resource just for choking. (Some resources may also perform their own ad-hoc choking, e.g. an OpenGL resource might not display faster than the framerate demanded even if there are many possible intermediate frames.)

An intelligent choke might support a *signifigance* function, i.e. choking based on time, but allowing a few extra values through if the values are somehow interesting. 

For eliminating redundant updates, Sirea does have a ready answer: 

        badjeqf :: (BFmap b, Eq x) => b (S p x) (S p x) -- adjacent equality filter

This behavior can improve performance by eliminating many redundant signal updates. There is some cost to perform the filter, of course, so it should be used judiciously. But there may be significant downstream savings for `bfmap`, `bzap`, and similar, and much rework avoided. (*NOTE:* `bconst` has a simple variation of `badjeqf` built into it; this is the main advantage of using `bconst` instead of `bfmap . const`.)

### Behaviors as Arrows

If you are familiar with the Arrows abstraction, invented by John Hughes circa 2000, then you have probably recognized several of the operators presented above: `(>>>)`, `(***)`, `(&&&)`, `(+++)`, `(|||)`, first, second, left, right. Sirea presents RDP as an arrowized model, albeit using `bfmap` instead of `arr`, and RDP offers many valuable properties not promised by arrow laws. Haskell's Control.Arrow class is not used. If you are unfamiliar with Arrows, or somewhat intimidated by them, do not fret! Like Monads, Arrows are best learned by using them, by osmosis and exposure, by understanding and manipulating toy examples. If you're feeling comfortable with the syntax and structure of RDP from the README so far, I do recommend reading [Understanding Arrows](http://en.wikibooks.org/wiki/Haskell/Understanding_arrows) in the Haskell wikibook.

Resources
---------

Resources exist outside the RDP program, but may be programmatically accessed by RDP behaviors. Resources generally include sensors, actuators, and state. Examples include keyboard, mouse, monitor, speaker, filesystem, database, network. External services and FFI can be usefully treated as resources. 

Signals to a resource represent continuous queries or commands. In practice, queries often implicitly require a system to actively gather more information, and commands often require a system to perform related queries and to respond with status. Collectively, these continuous queries and commands are called demands.

The word "demand" was chosen because its definitions and connotations in English fits in many ways. Consider a few [definitions of demand](http://en.wiktionary.org/w/index.php?title=demand&oldid=18269207). Demands may be for information (queries). Demands may be for action (orders, commands). Demand has connotations of *authority* and *force* that mere "requests" do not. Demand, as the word is used for markets and utilities, suggests continuity, concurrency, duration, variation over time. At any given moment, there may be many demands on a resource. 

*Reactive Demand* Programming is so named because it supports composition and precise control of long-lived, time-varying, concurrent demand signals. Demands in RDP are *authoritative* because RDP is designed for object capability security. RDP is a *bidirectional* reactive paradigm: one may demand information from sensors or state resources, and one may analyze and manipulate that information to form more demands. Resources in RDP are *demand-driven*. At any given instant, a resource is influenced by a *set* of active, concurrent demands. 

Processing demand signals as a *set* at any given instant - i.e. such that ordering, replication, and history of the signals is irrelevant - forms the foundation for RDP's declarative effects. Effects in RDP are commutative and idempotent ultimately because signals are treated that way by resources. 

Many Sirea packages will be focused on adapting and abstracting useful resources to the RDP model. Specific classes of resources and surrounding idioms are discussed further.

### Demand Monitors

Resources may be influenced by the set of active, concurrent demands. Thus, a simple, useful resource could respond to demands with the set of active, concurrent demands. This concept is called the **demand monitor**, and is of great practical value for RDP systems. Demand monitors and a few variants are among the few resources provided by **sirea-core**.

Demand monitors don't directly respond to demand with the set of demands. (An artificial requirement to provide a demand just to observe demands would be inconvenient, and would reduce stability.) Instead, there are two separate facets of a demand monitor: one to receive demands, one to monitor demands. Each distinct demand monitor resource is represented in RDP by a unique pair of behaviors. Demands received by a particular demand monitor are only observable to systems that hold the corresponding monitor facet.

        B (S p x) (S p ())   -- receives demands
        B (S p ()) (S p [x]) -- monitors demands

The basic demand monitor actually returns a Haskell list in some indeterminate order. Not all demands developers might wish to monitor have an `Ord` class. In that case, it is up to developers to provide the extra discipline to treat the list as a set. However, there are variations that will return a sorted, uniquified set of demands.

One variation of demand monitor is possibly the simplest useful resource: the **activity monitor**. An activity monitor is essentially a demand monitor for the unit signal: the monitor responds with a simple boolean indicating whether there is any demand at all.

        B (S p ()) (S p ())   -- receives activity
        B (S p ()) (S p Bool) -- monitors activity

Demand monitors and activity monitors are typically composed into more complex behaviors to track demands and use of resources. Demand monitors are also useful as a volatile blackboard for communication between agents, or as a registry for publishing available services or plugins.

Demand monitors have a severe weakness: a large set of demands will tend to be much less stable than any of the contributing signals. This is because an update to any constituent signal will require an update to the monitored signal. A suggested practice for demand monitors is to use many fine-grained demand monitors, one for each role or responsibility. Also, if possible, favor variations of demand monitors that narrow the set of responses (e.g. the activity monitor or a k-maximum monitor).

### Sensors

Sensors include user input devices - mouse, keyboard, joystick, accelerometers, microphones. Sensors include video, cameras, lidar, radar, sonar. There are sensors to detect touch, vibration, capacitance, chemicals, motions, magnetic fields. There are sensors to track locations, temperatures, doors. There are sensors for emotional state and brainwaves. There are many sensors, esoteric and otherwise, and many more are developed every year.

RDP is a reactive paradigm, so it is easy to integrate sensors with RDP programs - no need for awkward callbacks, polling, or event loops. Just query a behavior that provides access to the sensor. Of course, someone must first develop adapters between Sirea and the given sensor - which very well might involve callbacks, polling, or per-sensor event loops. 

The developers of sensor adapters must decide how to represent sensor outputs as time-varying values in Sirea. 

Many sensors (mouse, keyboard, joystick, video) are obvious in their adaptations. They already represent useful, time-varying world states. Others sensors, such as microphones, are somewhat less so. For example, a microphone might present sound information as a time-varying voltage or pressure, but instantaneous pressure isn't very useful information for understanding sounds. (Also, while Sirea is efficient, it isn't suitable for signals that vary at 44kHz.) An effective representation for sound might instead be a representation of estimated intensities for different frequencies, presented at a much lower frequency (perhaps 200 Hz).

Signals in RDP cannot directly represent instantenous events. However, event-sensors can be effectively adapted in at least two ways: short-term memory or framing. For short-term memory, new events are stored, and old events are removed (or archived), and the RDP behavior continuously observes the intermediate state. If events are very high frequency, that state could be choked for performance, which essentially leads to the framing technique: events accumulate into frames, and the next frame is being accumulated while the current one is displayed. Choosing a frame-rate will involve an efficiency vs. latency tradeoff. (Both of these designs involve coupling with state resources.)

Active sensors - such as radar, lidar, sonar - should generally be activated based on demand signals. Discontinuous or event-controlled sensors - such as cameras - require a more sophisticated adaptation. For cameras, one might offer some ability to statefully "schedule" an activation (which would be quite useful if orchestrating many cameras). 

Sirea will not begin its life with a large library sensors, just a few basics (mouse, keyboard, joystick). This is an area where developers will be able to easily contribute useful new packages. 

### State


### Resource Management

The *set* based processing is the foundation of RDP's declarative properties: 


 their activity and state is influenced by *sets* of active demand signals. In general, resources may be concurrently accessed and thus be influenced by more than one signal. Resource management in RDP is generally trivial: most resources can return to a rest state when there is no demand. A webcam, for example, will typically power down while there is no demand for the video signal. (Some resources can even enter rest state while there is active demand, so long as it's stable.) 

Processing demand signals as *sets* at a given instant - i.e. such that ordering, replication, and history of signals has no additional effect on the resource - is the foundation for RDP's declarative properties (spatial commutativity and idempotence). 

Most reactive paradigms focus on composing "information" signals, without providing clear understanding or control of how those signals were acquired. It's easy in FRP to observe the mouse, but very difficult in FRP to express that a webcam should be powered on only while you actively *need* its video signal. 



; it is trivial to ensure that a webcam is powered only while there is demand for its signal. And RDP is designed for object capability security, thus demands are implicitly *authoritative* requests.





In general, resources in RDP are shared. When a behavior that accesses a resource is shared (even indirectly), it is possible the resource will receive concurrent input signals representing queries or commands (collectively, 'demands') on the resource. Concurrent sharing of resources is much safer with declarative effects than with imperative effects. (*Note:* If exclusive control is necessary despite that advantage, a resource can be indirectly accessed via a controlling agent.)

Resources may be discovered or introduced to a system at runtime. Some resources may be queried 

This is modeled by querying a resource whose output is a set of dynamic behaviors and metadata. Any such resource can be called a registry. 

 e.g. by computing a unique name for a file in a filesystem. Discovery and reset idioms can be used to simulate creation and destruction of resources.



generally include sensors, actuators, and state. User input devices such as mouse, keyboard, monitor, microphone, speakers, joystick, touchscreen, etc. are all resources. Services provide indirect access to composite resources, and may usefully be treated as resources. 

 to sensors, actuators, and state. Foreign services may usefully be treated as resources. Devices for user interaction - mouse, keyboard, monitor, speakers - can generally be classified as sensors or actuators. Many devices, such as touchscreens or joysticks with force feedback, include aspects of both sensor and actuator.

Resources used in RDP are typically sensors, actuators, state, and services. Examples include keyboard, mouse, joystick, webcam, microphone, monitor, speaker, filesystem, database, network, printer, google, wolfram alpha, and so on. Resources are external to RDP. They are not created or destroyed by an RDP program (no new or delete). However, resources may be *discovered* at runtime. And by 'discovering' resources from an abstract infinite space (such as names in a filesystem) it is possible to simulate runtime creation of some resources. 

* Resources used in RDP are typically sensors, actuators, and state. Examples include keyboard, mouse, joystick, webcam, microphone, monitor, speaker, filesystem, database, network, printer, and so on. Foreign services, such as google search or wolfram alpha, may also be treated as resources. Resources are external to RDP. Resources are not created or destroyed by an RDP program - no `new` or `delete`. However, resources may be *discovered* at runtime, and it is possible to simulate dynamic creation of resources by discovering in an infinite abstract space such as names in a filesystem.


(sensors, actuators, demand monitors, state: persistent state, animated state, (formally) volatile state)

(under construction)

### Feedback



### Resources and Effectful Behaviors

RDP behaviors may be effectful. Consider the following:

    bmousepos :: B (S p ()) (S p MousePos)
    bgetKeyState :: B (S p KeyCode) (S p Bool) 
    bgetfile  :: B (S p FileName) (S p FileState)
    bcamctl   :: B (S p PanTiltZoom) (S p ())

Using effectful behaviors, we can observe the mouse position, obtain file states, and control a camera - just to name a few. (The trivial input signal for bmousepos is still relevant for specifying the duration of observation.) There can be effectful behaviors for each resource, and for collections and views of resources. A consequence is a simple type such as: 
   
    bmain     :: B (S p ()) (S p ())

is a potential signature for RDP application, much like `void main()` is a potential signature for procedural applications. Behind that trivial signature, an RDP application is a complex symphony of signals and declarative effects orchestrated in space and time.

Effects are valuable for programming open and extensible systems, but can make code unclear or more difficult to reuse. RDP avoids or mitigates many problems of effectful and concurrent programming. See discussion below on _Declarative Effects and Concurrency_.


Stateful resources are external to the RDP behavior, and access to those resources can be established by multiple dynamic behaviors. Orthogonal persistence is a natural consequence of dynamic behaviors and declarative semantics.

### Resource Management

Quite Trivial

* _Demand Driven Resource Management & Process Control._ RDP enforces a property called "duration coupling" - the output signal of a behavior has the same activity as the input signal, perhaps delayed a small amount. This is useful for process control: you can halt any RDP program or subprogram by cutting the control signal. Further, resource acquisition and activation is driven by input signals - i.e. demand driven. Resources can be released and return to a passive state when demands on that resource are halted. (*Aside:* duration coupling is the reason RDP signals cannot be created or destroyed and are not first class.)

### Agent Resource


### Demand Monitors

### Declarative State

### Discovery Idioms

### Reset Idioms

### Modeling Services  

(an agent can construct and share dynamic behaviors via a registry; that dynamic behavior becomes a service)


### First-Class Frameworks

(reactive semantics, very nice, no IoC; a framework abstracts as a simple set of RDP behaviors).



* _Ad-Hoc Workflow and Control flow._ With state, RDP can model arbitrary workflow or control flow systems. RDP's ability to express ad-hoc queries and wait for ad-hoc conditions makes RDP very expressive in this role, far more so than most native workflow and control flow systems. Further, RDP will retain its fringe benefits such as persistence, extensibility, and speculative evaluation. Of course, RDP is unlikely to be as efficient as a compiled control flow system. But for control flows, RDP systems should rarely need more than a shallow model at a border between systems. And for workflow, RDP's flexibility, anticipation, and persistence will generally prove more valuable than efficiency.

* _First Class, Composable Frameworks._ The combination of reactive temporal semantics, open system, and dynamic behaviors makes it easy in RDP to represent ad-hoc frameworks in a first-class manner. There is no need for inversion of control, just continuous observation and influence. The frameworks are composable - a single RDP behavior can easily orchestrate several frameworks, continuously translating and transferring data between them. This feature should be very valuable for staged metaprogramming (e.g. modeling DSLs, interpreters) and for development and integration of architectures.




RDP Laws and Properties
-----------------------

The trinity of composition is operators, operands, and properties. The operators are uniform: can be applied to every operand. The operands are closed: each composite is in the set of operands. The properties are inductive: they can be computed with knowledge of the operator and the properties of each operand. (*Note:* Properties that are *invariant* over the composition operators are trivially compositional.)

Composition seems essential for large scale software development. Consider:

* In large systems developers cannot afford to study the definition of every component. In open systems, they cannot look even if they wanted to. It becomes essential to reason based on the few properties that are easy to compute without knowledge of the definitions, i.e. compositional properties. 
* Developers of large scale systems cannot afford to learn new ontologies for every component they wish to introduce. Further, large ontologies become annoying: it easily becomes expensive to add the translation glue between components and subtly heterogeneous data models. It is preferable if there are just a few standard operators for introducing components - i.e. composition operators. 
* Large scale software is ultimately about systems and subsystems. Systems have no clear edges, not clear bottom, no clear top. Every subsystem is another system. I.e. systems are closed under composition. 

Reactive Demand Programming was developed while prioritizing compositional properties as a first principle. 

Of course, RDP developers still benefit from knowledge of implementation details. And whitebox utilities might still support dependent types, rich logics, contracts and the like (though Sirea doesn't provide any of that). But compositional properties will lift much burden from developers, and will enable the whitebox systems to focus more on problem domains and data models, and somewhat less on race conditions or resource management.

(todo: decide how to format and describe properties listed in System.Behavior, plus duration coupling)


Idioms and Patterns
===================

(old material. Will move most of this into Resources section, with heavy modifications.)

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
* reactive state transition:
    * state is integer
    * signals contribute possible transitions
    * transition whenever opportunity exists
* animated reactive term rewriting:
    * state is (term, time debt)
    * signals introduce term-rewrite rules
    * each rule has attributes (max debt, new debt), each > 0.
    * when a rule is applied, new debt is added to time debt.
    * rules are not applied unless max debt >= time debt
    * time debt continuously, linearly reduces towards zero

Since all state models are *external* to RDP (i.e. modeled as external services), it is not difficult to add orthogonal persistence. It is also not difficult to dream up new ones and add them to an application via library or plugin. There are interesting state models one could create for continuous signals (e.g. based on bezier curves, follow the carrot, integrals of forces). 

It's also easy to augment or create variations on existing models, though this tends to trade simplicity for other features (often a dubious tradeoff, but sometimes worth making). For example, animated reactive term rewriting might be modified with built in rules (for performance), delayed rewrites (easier scheduling), key-based ownership types (for security, control), and variation to terms rewriting (such as graph rewriting). Implicit computation of (max debt, new debt) attributes would also be nice.

Sirea shall provide a few simple state models, including the three described above, complete with *persistence by default*. But none have been implemented yet. And they won't be part of the core package.

State models may be non-deterministic, but it is highly preferable (for debugging, regression tests, anticipation, replication for fault tolerance, etc.) that they are deterministic. For state transition, when given two transitions the process could favor the lower integer. For term rewriting, the process could impose a simple arbitrary ordering on rules (assuming they aren't opaque).

An interesting possibility is to seek *stability without stateful semantics*. Stateful semantics refers to systematically preserving information from past into present time. Stability doesn't require information be preserved, only that avoid changing unnecessarily - something that can *naturally* be achieved by use of non-deterministic semantics. Stability is suitable in cases where you don't strongly care *what* the information is so long as it's consistent and stable - potential domains would include UI layout, live dependency injection, or a path planning system. Constraint logic programming and many machine learning techniques offer [effective approaches](http://awelonblue.wordpress.com/2012/03/14/stability-without-state/) to stability.

Stateless stability can help alleviate need for real state, and thus simplify reasoning about systems (especially during partial failure and recover).

*NOTE:* Reactive state transition and reactive term rewriting potentially allow for non-determinism. Fortunately these could be resolved arbitrarily. E.g. for ambiguous state transition, choose the lowest available integer. For reactive term rewriting, access to structure of rules would be necessary to impose deterministic ordering on them. Non-determini

*NOTE:* If you need *differences* between past and present values (e.g. to redraw the dirty rectangles), then RDP offers a stateless alternative: use `bpeek` to instead find differences between present and future values. This works up to a small constant time, though can work longer if preceded by `bdelay` to recover stability.

*NOTE:* Sirea and RDP will automatically remember values where necessary. No state is needed to combine signals with `bzip`, and use of `badjeqf` can help filter updates that would be identical in order to improve stability. If *memoization* is desired, you should either use [lazy Haskell techniques](http://hackage.haskell.org/package/MemoTrie-0.5) (at risk of space leak!) or use a dedicated state model.

*NOTE:* State IS necessary if you want to model choking of updates, e.g. such that you see at most one update each second. Choking uses state to record the selected value. Animated state, such as *animated reactive term rewriting*, can model choking quite elegantly. *Not that choking is elegant.* Be cautious about choking; it can mask feedback issues without solving them (often better to take the hit, *find* the problem, solve it by design).



## Modeling Control Flow and Workflow

Other than integration with control flow systems (which is a significant concern), there are not very many problems that are well aligned with control flow paradigms. However, the broad and flexible notions of [workflow](http://en.wikipedia.org/wiki/Workflow) does cover useful ground. 

Programming with control flow is implicitly, and pervasively, stateful. The state includes control pointers, stacks, continuations, event queues, and mailboxes. The more sophisticated control flow paradigms, such as join calculus, introduce more sophisticated control state, such as barriers. Workflows similarly use a great deal of state, barriers, and similar. 

Programmers working in control flow paradigms rarely think about implicit state. Out of sight is out of mind. On occasion, however, they are confronted with concerns for persistence, runtime upgrade, partial failure, recovery, unreliable communications, concurrency, job control, resource management, visualization, debugging, and extension. To address these concerns, programmers must often make explicit the implicit, e.g. with command patterns, transactions, saving workflow state to a database. External state is both easy to persist and easy to visualize with independent programs.

Reactive Demand Programming does not provide implicit state. But external state can be utilized, and RDP does enable an agent to wait on ad-hoc conditions. By using state to model propagation of control and the conditions of resources, and by modeling agents as waiting on ad-hoc conditions for state, RDP can flexibly, extensibly, persistently, and robustly model any control flow or workflow systems. 

Of course, it is unlikely that performance will be competitive with a native control flow system. 

But if the model is shallow (to integrate with a control flow system) then performance should not be a significant issue. And for workflow (or control flow at distributed scales), the flexibility, extensibility, and robustness are generally more valuable than performance under ideal conditions. RDP has potential to be competitive with control flow and workflow systems even in their own domain. 


### Feedback and Delay

A major concern with reactive programming in continuous time is *feedback* - when a signal output affects its own input. This is a natural concern, with physical cause - e.g. if we place a microphone near its associated amplifier, we often get very shrill feedback. Feedback is unstable and can consume a lot of energy very quickly.

In RDP, you cannot directly express a closed loop. (No equivalent to ArrowLoop.) So feedback is impossible for pure RDP behaviors. But feedback is still possible when interacting with external services and state. Consider:

    getState >>> bfmap (+1) >>> writeState

In this case, we might be trying to writeState at the same *instant* for which we're reading state, and thus `getState` would be modified by the results of `writeState`, creating a feedback loop through the state. 

Feedback is RDP's equivalent to divergent computation. 

Symbolic analysis or laziness could potentially eliminate feedback loops in some cases. But RDP is designed for *open systems*, where those techniques are not generally available. Sirea does not assume them. Good state models for RDP will help, both in avoidance and resolution of the problem. For example, animated reactive term rewriting eliminates most need for such loops (rewrite rules include simple functions) and helps stabilize the result (limiting updates via time debt, and not all rewrite rules will apply). But those are of limited scope.

Since you can't design feedback entirely out of complex systems, add `bdelay`. This slows the feedback down, changing instantaneous feedback into a well-defined incremental feedback process. Potentially, a natural one.

### Bridging Paradigms

Animated reactive term rewriting is incredibly expressive. Expressing a Turing machine with reactive term rewriting is no challenge at all. A few well-chosen built in types and rules could even allow highly competitive performance (i.e. via runtime compilation of terms, simple namespaces within a term, perhaps leveraging OpenCL).

If you are unfamiliar with term rewrite systems, look up Maude and broaden your horizons. The *reactive* variation on term rewriting flips when rules and terms are provided (and who provides them). The *animated* variation enforces incremental processing and makes the timing of state updates deterministic and declarative. But *animated reactive* term rewriting has the same amazing expressiveness as the traditional variation.

If it were just modifying state locally, that would be sufficient for a closed process. 

But shared state can also serve as a powerful basis for IO and side-effects in open systems. 

Shared state is often discouraged in imperative programming because of many issues with concurrency. Developers must reason about *permutations* of observation and influence. But *declarative state*, especially for RDP, is much nicer: commutative, idempotent combinations; holistic processing of updates; precise logical timing; never a missed update; ad-hoc conditions on any combination of observable states. 

Shared state can support a [blackboard metaphor](http://en.wikipedia.org/wiki/Blackboard_system). 

Agents use signals to write a task into shared state representing the board, e.g. "print this document". A printer agent could then modify the request to claim it, e.g. adding "agent Orion has accepted this task". Orion would then print the document, and update the term with status information as it runs. When finished, the original writer can remove the term. 

A blackboard allows rich and robust interaction, far more so than procedure calls or messaging:

* no blind wait; easily observe intermediate and incremental status
* modify request: pause, abort, prioritize; simple job control
* loose coupling, ambient programming; no direct reference to agent
* open extensibility, plugins; easily introduce new agents to board
* offline, disruption tolerant; agents available at different times
* collaborative: agents to split big tasks, combine results

For security and performance reasons, it would be a bad idea to have one big blackboard for the world. Instead we would use lots of small blackboards, and a few agents to selectively synchronize between them when conditions are right (observing one board, writing to another).

Between modeling machines with animated state and open interaction between agents, any ad-hoc control flow or [workflow pattern](http://en.wikipedia.org/wiki/Workflow_patterns) can be expressed. And will benefit from the robust, resilient nature of RDP.

But RDP is ultimately about open, declarative orchestration of *stateful* systems - sensors, actuators, UI, and databases. Use of external state is often essential in RDP architectures and design patterns.

*NOTE:* the notion of logically deterministic timeouts initially struck me as terribly odd, as though counter to their purpose. But timeouts still serve a valuable role when modeling incremental computations in shared state, e.g. computing a better move in a game. And deterministic timeouts makes for reproducible errors, easy maintenance, and better reasoning about timing.

### MISCELLANEOUS

* For distribution. State models can be created that are suitable for distribution and replication. DHTs, LDAP, and content-addressed networks are possibilities. (Distributed state models? Cloud Types?)
* For live programming. The "main" behavior can be modified without disturbing state. An RDP application is one big, active declaration that can be modified at any time. 

