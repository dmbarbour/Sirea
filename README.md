
Sirea
=====

**Si**mply **Rea**ctive! Declarative orchestration in Haskell using the Reactive Demand Programming (RDP) model. 

_an RDP application is a complex symphony of signals and declarative effects orchestrated in space and time_

_an RDP application is one big, active declaration that can be modified at any time_

Everybody knows *"side-effects aren't compositional"*. But that statement is only true in general. There are useful subsets of side-effects that are idempotent, commutative, monotonic. For that subset of effects - aka *declarative* effects - one can achieve equational reasoning and refactoring on par with the very best _pure, law abiding_ programming models. Any pure functional programming advocate also knows side-effects are rarely essential, i.e. that just a little more cleverness (and maybe a little help from Oleg) will find a wonderfully pure solution to almost any problem. 

How much more could be achieved with just a few, highly constrained, declarative effects?

Reactive Demand Programming was developed as an answer to this question. RDP's particular effect model was inspired from a symmetric *observer effect* in physics and psychology: one cannot generally observe or query a system without altering its behavior. Of course, by ignoring the response, one can "observe" a system with the sole purpose of influence. In RDP, resources, state, and agents may indirectly react to a set of signals that may represent active queries, requests, or commands (collectively, *demands*). Signal processing is local, modular, and composable. Treating the signals as a *set* provides spatial idempotence and commutativity. 

The answer: *Anything worth doing* can be done with declarative effects. (Or, rather: *Anything worth being* can become with declarative effects.) This includes state, video, music, sensor networks, control systems, user interface. All can be adequately observed and influenced via the RDP paradigm. It does take new idioms and abstractions (especially new state models) to effectively adapt and utilize external resources.

RDP is not the first paradigm with highly declarative effects. Synchronous reactive programming, temporal logic programming, discrete event systems, and concurrent constraint systems each precede RDP by more than a decade. Use of sub-structural types for uniqueness or linearity have also been utilized to great effect. However, RDP might be the only paradigm so far that supports declarative effects in an open system.

Reactive Demand Programming simultaneously delivers:

* the reasoning and composition benefits of pure dataflow models
* the flexibility and adaptability of ad-hoc IO
* the encapsulation and dynamic reconfigurability of OOP
* the robust open composition of object capability model
* the extensibility of publish-subscribe or blackboard systems
* the concurrency and parallelism of multi-agent systems
* the utilization and scalability of lightweight time-warp


Teaser
------

(Todo: put a small example here that does something moderately useful. Maybe display a clock, or a pendulum example, or a pocket-watch pendulum? Will need cairo or glfw working with Sirea.)

Sirea Features
--------------

Sirea is now in an alpha state. The primitive behaviors (everything in Sirea.Behavior) have been implemented, plus partitions. Sirea should be in a compiling state for each check-in, but testing is very lightweight and not yet automated. 

Sirea will eventually offer many features. Many are already supported, but some need support from separate not-yet-implemented packages such as `sirea-state` or `sirea-plugins`. All of these are on the agenda for 1.0:

* _Declarative, Compositional, Extensible._ General constructs in RDP are associative, spatially commutative, and spatially idempotent. All effects and state manipulations also have those properties. In addition to composition, RDP is designed to support open extension. There are no "closed loops" in an RDP system, not even local state: any feedback must be an open loop through an external resource. This makes it easy to add behavior to an established system. Between these properties, is easy to reason about, refactor, reuse, compose, and extend RDP code.

* _Observation & Influence._ The effects model of RDP is elegant and symmetric, push and pull. RDP behaviors can represent distributed queries, obtaining data from multiple sensors and data sources. RDP behaviors can represent distributed control, influencing many remote systems in precise logical time. An RDP behavior can combine these aspects, representing a smart reactive network. All this is orthogonal to the input and output signals for the behavior. RDP developers never need to jump through hoops to perform useful IO. 

* _Dynamic Behaviors._ Developers can easily model switching behaviors at runtime, e.g. to switch behavior based on a configuration file in real-time. Runtime resource discovery, service brokering, live programming, and staged compilation can also be modeled in this manner. Developers can model *interfaces* for arbitrary resources as collections of dynamic behaviors, supporting OO-like abstractions.

* _Declarative Resource Access._ The resources used by a Sirea application are implicit and declarative in the Sirea program, so there is almost zero setup to use Sirea - simply define the behavior and run it. `sirea-plugins` will eventually provide a distinct but much richer model, supporting soft constraints and type-driven linking between plugins.

* _Demand Driven Resource Management & Process Control._ RDP enforces a property called "duration coupling" - the output signal of a behavior has the same activity as the input signal, perhaps delayed a small amount. This is useful for process control: you can halt any RDP program or subprogram by cutting the control signal. Further, resource acquisition and activation is driven by input signals - i.e. demand driven. Resources can be released and return to a passive state when demands on that resource are halted. (*Aside:* duration coupling is the reason RDP signals cannot be created or destroyed and are not first class.)

* _Anticipation._ RDP does not implicitly predict the future, but does ensure predictions propagate compositionally - i.e. each signal update carries flexible information about the future. Anticipation is gold for planning systems, for chained prediction systems, for resource management (e.g. load a configuration file slightly in advance of needing the value), and for dynamic behaviors (masking setup latency). Anticipation also dampens variability in network or thread scheduling latencies. (*Note:* Developers can build prediction systems, e.g. Kalman filters, atop state resources.)

* _Orthogonal Persistence._ State is always modeled as an external resource to an RDP program. (To make all state external is prerequisite to effectively support dynamic behaviors.) This makes orthogonal persistence quite trivial. In Sirea, persistence will be built into all state and statelike resources (e.g. from `sirea-state` and `sirea-stable`). 

* _Live Programming._ RDP is a dynamic metacircular reactive system, and thus always models live programming. Formally, even the toplevel RDP behavior may be understood as a dynamic behavior (evaluated under `bexec`). The `sirea-plugins` package will make full live programming possible at the Haskell level. That state is an external resource greatly simplifies live programming.

* _Multi-Agent Systems._ I find it convenient, especially near the toplevel of an application where behaviors exist primarily for declarative effects, to understand RDP behaviors as expressing multiple agents, threads, or processes. Sirea provides a convenience operator, `|*|`, to declaratively compose behaviors as a multi-agent system. The `sirea-plugins` package will enable some plugins to represent toplevel agents, thus supporting multi-agent programming as the default for Sirea. 

* _First Class, Composable Frameworks._ The combination of reactive temporal semantics, open system, and dynamic behaviors makes it easy in RDP to represent ad-hoc frameworks in a first-class manner. There is no need for inversion of control, just continuous observation and influence. The frameworks are composable - a single RDP behavior can easily orchestrate several frameworks, continuously translating and transferring data between them. This feature should be very valuable for staged metaprogramming (e.g. modeling DSLs, interpreters) and for development and integration of architectures.

* _Predictable, Composable Performance._ RDP strongly favors incremental, real-time computation. Batch processes that take a long time typically must be modeled incrementally, within state resources. Sirea is suitable for soft real-time multi-threaded process-local applications. Developers in Sirea can easily reason about latency, fairness, and memory footprint. Laziness is systematically eradicated by stateful resources as they're persisted, so there are no hidden space or time leaks.

* _Efficient._ RDP does not sacrifice any performance. The `:&:` and `:|:` signal types constrain which components are updated, avoiding rework. Updates between threads are batched for efficiency, snapshot consistency, and reduced synchronization overhead. Anticipation can improve efficiency if predictions are valid for even a short while, and can reduce performance burden by enabling most resources and computations to be prepared in advance. Demand driven resources can help developers avoid bloat. Sirea performs some dead code eliminations, compiles away simple data plumbing, provides a dynamic solution to the update ordering problem, and provides several behaviors for performance annotation.

* _Declarative Parallelism._ Sirea supports simplistic data parallelism via `bspark`, ad-hoc data parallelism via `bstrat`, and multi-threaded concurrency via `bcross`. Use of `bcross` essentially models threads as resources - a feature inspired from [FRP.Sodium](http://hackage.haskell.org/package/sodium-0.5.0.2). However, `bcross` is intended to divide resource management responsibilities. For performance, developers are encouraged to favor data parallelism. The explicit temporal dimension of signals makes it easy to control sparks and avoid "fizzling" where a spark is wasted. (*Note:* [monad-par](http://hackage.haskell.org/package/monad-par) is supported indirectly, via `bstratf`.)

* _Plug-ins._ The package `sirea-plugins` will support a fine-grained, type-based plugins and linking model, avoiding some of the [evils](http://gbracha.blogspot.com/2009/06/ban-on-imports.html) and [entanglement](http://awelonblue.wordpress.com/2011/09/29/modularity-without-a-name/) issues of name-based imports. This will leverage `Data.Typeable` and `System.Plugins`. The plugins will provide dependencies to other plugins and to the main application, reducing need for Haskell's import model. Plugins will be recompiled on the fly to support live programming by default. Each plugin provides multiple modules, and each plugin module may require and export types. The plugins system uses a constraint model: only one instance for each type. Plugin modules exporting the same type will conflict, but also serve as fallbacks or adaptation possibilities based on soft constraints (preferences) or intermediate times when a plugin cannot compile due to edits. The plugins system can maintain a record of what each plugin provides, so that only useful plugins get loaded or stay in memory.

* _Blank Slate App._ Some plugin resources can be tagged (via the type system) as toplevel agents. Loading agents from plugins will obviate use of Haskell's `main` and shift Sirea applications entirely into plugins. A command line application, simply named `sirea`, will provide a blank slate whose behavior is determined by a soup of plugins and a live configuration file. Developers can start `sirea` even before creating any plugins, do live programming from the first line of code. 

* _Externalized Event Loop._ Sirea can be embedded into any application with an event loop. The integration is via a `Stepper` object that must be run on a regular basis. The `Stepper` can provide a callback to indicate when it has work to do. (If developers do not need control over their event loop, they may use the convenient `runSireaApp` which will run until interrupted.)

* _Batteries Included._ Convenience functions and behaviors are included. The default `runSireaApp` in the main thread will properly halt on `Ctrl+C`. Several packages will be developed so that Sirea can be used for real applications - at least user input, graphics, sound, network (especially HTTP client and server), and filesystem access, such that developers can immediately start on usable applications. The first of these is `sirea-glfw`, which can provide mouse, keyboard, joystick, and a window. 

Finally, while I cannot promise this as a feature you'll observe, I find RDP very intuitive. RDP supports many useful approaches to thinking about computation systems: as multi-agent systems, as composable bidirectional view-models, as a declarative resource orchestration network, as chained prediction or planning systems, and so on. The intuitions gained with RDP and Sirea have a solid foundation due to compositional properties and system-level invariants.

I realize this is an *incredible* list of features. I think that's the consequence of taking a simple idea - that one can have effects without giving up rich equational reasoning, if we're picky about the effects - and running with it as far as I have.

Weaknesses
----------

Most weakenesses of Sirea RDP belong far more to Sirea than to RDP. 

* Developers must use a point-free style for RDP behaviors in Sirea. This a very rewarding style, and anything difficult can be achieved using full Haskell functions for staged programming. But newcomers seem to find it intimidating. 

* It is possible to express open feedback loops, like placing microphone near amplifier. There are idioms to avoid such loops (e.g. for blackboard metaphor, don't put replies onto same board as requests), and there are idioms to dampen loops (e.g. `bdelay` or clever use of `badjeqf`). But Sirea provides no static warnings. Developers must learn the idioms and use discipline. (*Note:* Sirea state and demand monitors will forcibly dampen loops and report errors when they occur dynamically, trading consistency for robust performance.)

* RDP is designed for [object capability model](http://en.wikipedia.org/wiki/Object-capability_model) systems, using dynamic behaviors as runtime composable capabilities. (This is reflected, for example, in having fine-grained capabilities for many resources.) Sirea, however, is designed to be convenient in context of Haskell's module and type systems. I am not entirely comfortable with this tradeoff; it isn't necessary if one reifies the module and linking system (e.g. service registry and matchmaker). 

* `unsafeLinkB` is very minimal and can easily break RDP invariants. This behavior is used to adapt external resources to RDP. While safer and more convenient adapter functions could be built, they are not a priority for Sirea at the moment.


Haskell Packages
----------------

Present or planned.

* **sirea-core** primitive behaviors, demand monitors, discrete clock, print to console, startup and shutdown. 
* **sirea-glfw** keyboard, mouse, joystick, GL window (simple window manager?)
* **sirea-filesys** simplistic reading and writing of files with reactive updates
* **sirea-state** term rewriting, state transition system, tuple space.
* **sirea-stable** a stateless stable model based on weighted constraint logic, with machine learning
* **sirea-plugins** live loading and constraint-based linking of plugins
* **sirea-app** a blank-slate application that loads plugins

Plus something for sound, and probably something for wxWidgets. Some of these might become separate projects, too, rather than just separate packages. 

Reactive Demand Programming (in Sirea)
======================================

The Reactive Demand Programming (RDP) view divides the world into three kinds of things: **resources**, **behaviors**, and **signals**. 

* Resources used in RDP are typically sensors, actuators, state, and services. Examples include keyboard, mouse, joystick, webcam, microphone, monitor, speaker, filesystem, database, network, printer, google, wolfram alpha, and so on. Resources are external to RDP. They are not created or destroyed by an RDP program (no new or delete). However, resources may be *discovered* at runtime. And by 'discovering' resources from an abstract infinite space (such as names in a filesystem) it is possible to simulate runtime creation of some resources. 

* Behaviors describe computation-rich data plumbing between resources. Some behaviors will represent access to a resource, providing a capability to observe or influence it. But the majority of behaviors in an RDP application are often simple data plumbing and pure transforms (cf. `Sirea.Behavior`). RDP behaviors cannot accumulate state; all state is kept in external resources. A simple, linear behavior might gain a joystick signal from GLFW, transform that signal into controls for a robotic arm, `bcross` over to a partition representing the robot resource, then push the signal to the robotic arm. (`getJoyData >>> bfmap joyToRobotArm >>> bcross >>> controlArm`). RDP can express many concurrent behaviors, e.g. using `|*|`. 

* A signal describes one value as it changes over time. That value typically represents state - e.g. the position of a mouse, frames from a webcam, or desired contents for a video display. Signals must be updated over time (since the future is not entirely predictable). In addition to the value, signals have a definite start time based on the observer - e.g. you start receiving frames when you start observing the camera. They similarly have an indefinite end time, based on the observer. Signals cannot be created or destroyed by RDP behaviors (though they may be replicated and transformed). Signal updates are propagated through behaviors. Propagating those updates, and garbage collecting old signal data, is Sirea's job. RDP behaviors cannot create or destroy signals. RDP behaviors can only manipulate signals that already exist.

RDP developers are directly concerned only with behaviors. Resources and signals are useful concepts for motivating and understanding the behaviors. Within RDP, signals and resources are not programming abstractions, cannot be created or destroyed. (Signals and resources *are* concretely relevant for developing an adapter between Sirea and an ad-hoc resource.) An RDP application is a behavior - a smart network between resources - that is installed between resources and activated by a control signal from the Sirea runtime (e.g. via `runSireaApp`). Even toplevel applications can be understood as dynamic behaviors.

Here are a few examples to help clarify the points:

* A mouse is a resource. A mouse might be accessed by a behavior called `glfwMousePos`. The `glfwMousePos` behavior will respond to an input signal (representing a long-lived query) with an output signal (representing the mouse's position, updated over the lifetime of the query). The output signal can be transformed and transported by other behaviors to do something useful, such as influence state that some concurrent behavior is using to control a display.

* An SQL database is a resource. An SQL database might be accessed by a behavior called `queryDB`. The input signal would contain a `SELECT` query string. The output signal would contain the query result, or possibly some error information. It is possible for both the query string and the output signal to change over time. In general, these changes are independent - i.e. it is possible that two different query strings result in the same data, and it is possible that the database changes over time so the same query will have a different result. (For spatial idempotence, however, equivalent query signals must have equivalent result signals.)

* When RDP developers need a signal with the constant value 3, they use `bconst 3`, which transforms a concrete input signal into a concrete output signal with the value 3. Relevantly, the output signal has the same logical start and stop times as the input, and exists in the same partition. `bconst 3` is a behavior that transforms a signal; it is not a signal. There is such a concept as *"the 3 signal"* (`s_always 3`) - a signal that is, was, and forever will be 3. But this signal cannot be used in the RDP layer; it would require creating a signal, and would violate duration coupling. Most pure functions on signals are not valid RDP behaviors. (As another example, an RDP behavior may not *fold* a signal because that would require RDP to have internal state, which would cause problems with dynamic behaviors, live programming, orthogonal persistence, spatial idempotence, and robust failure modes.)

RDP toes more than one line to achieve its compositional properties and declarative effects. This results in several [software fences](http://www.johndcook.com/blog/2012/09/04/software-fences/) that might initially seem limiting. Fortunately, if you write code in Sirea and avoid the behaviors named with "unsafe", you will soon gain an implicit understanding of RDP's constraints, and of new idioms to work within them.

The best way to learn RDP is probably to use it, to implement examples. (I'll make an effort to provide many example as I make progress in implementing the RDP utilities.)

The following sections discuss signals, behaviors, and resources more deeply.

Signal Values
-------------

**Note:** RDP developers do not use first-class signals. RDP developers do not use second-class signals. RDP developers do not directly use signals. However, an understanding of signals is essential for understanding RDP. Concrete knowledge of signals is also necessary to use `unsafeLinkB` and adapt Sirea to more external resources resources. 

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

**Note:** The `Nothing` value used here does not represent failure. Let me repeat that: the `Nothing` value **does not** represent failure. Failure must be represented as an active signal. E.g. if I want to represent the possibility that the keyboard is inaccessible, then I'd need to use a type like `Either KeyState Error` for the active signal. This is necessary for RDP's *duration coupling* invariant, and is preferable because it allows RDP behaviors to observe failure and take appropriate action.

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

Signal updates have a nice properties compared to general message passing:

* updates are idempotent; useful over a shoddy network connection
* updates are composable by "piggybacking"; useful when fast source feeds slow consumer.
* updates allow takebacks (limited by stability); optimistic concurrency, eventual consistency, plan validation
* updates can target future; masks variability in latency, simplifies scheduling, avoids rework
* updates of full signal can be more efficient if they hold for a while.

These are in addition to the many benefits associated with anticipation and speculative evaluation.

In Sirea, a new behavior is compiled into a network with all signals set to inactive. Then the first signal update is applied.

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


RDP Behaviors
--------------

Behaviors describe computation-rich signal plumbing between resources. Behaviors can represent dataflows, networks, lenses, agents, and access to a resource. A behavior can leverage ad-hoc external resources for state, stability, and intelligence. Behaviors can precisely orchestrate multiple signals that model distributed, continuous queries or commands. 

Behaviors are suitable for real-time data plumbing and data transforms. By *real-time* I mean operations that can be computed in a small bounded maximum time. Sirea's `bfmap` provides all pure Haskell functions, but users are strongly encouraged to use functions that can be computed in real-time or close to it. When long-running computations are needed, they can be modeled with incremental real-time steps in state.

### Basic Behaviors

Basic behaviors are the behaviors that do not represent resources or special performance annotations. By nature, basic behaviors are pure. Basic behaviors can duplicate, delay, anticipate, split, merge, zip, swap, rearrange, and map functions over signals.  Here are a few examples:

        bfmap    :: (x -> y) -> b (S p x) (S p y)
        bconst   :: c -> b (S p x) (S p c)
        bzip     :: b (S p x :&: S p y) (S p (x,y))
        bzap     :: b (S p (x -> y) :&: S p x) (S p y))
        bfirst   :: b x x' -> b (x :&: y) (x' :&: y ) 
        bswap    :: b (x :&: y) (y :&: x)
        bdup     :: b x (x :&: x)
        bfst     :: b (x :&: y) x
        (>>>)    :: b x y -> b y z -> b x z
        bassoclp :: b (x :&: (y :&: z)) ((x :&: y) :&: z)
        bsplit   :: b (S p (Either x y)) (S p x :|: S p y)
        bmerge   :: b (x :|: x) x
        bmirror  :: b (x :|: y) (y :|: x)
        bleft    :: b x x' -> b (x :|: y) (x' :|: y)
        binl     :: b x (x :|: y)

In these examples, `b` is the abstract behavior type, and `b x y` is a behavior that takes signal `x` and responds with signal `y`. The signals `x` and `y` are complex, allowing distribution and asynchrony. Many of these behaviors are actually Haskell functions that return behaviors. For example, `bfmap` is a function that will map a Haskell function onto every element of the input signal. 

Here `b` is the behavior type, and `b x y` is a behavior that takes an `x` signal and responds with a `y` signal, which may be complex. The above two behaviors provide access to Haskell functions and values. `bfmap` will apply a fun



These type signatures might be difficult to interpret at first, a wall of text.


The basic behaviors are documented in `Sirea.Behavior`. There are about fifty that must be learned to use Sirea effectively. Fortunately, there are many symmetries and dualities that should make them easy to absorb. A few useful behaviors serve as performance annotations, to force lazy thunks, parallelize future computations, eliminate redundant updates, or simplistic memoization.

Basic beh


RDP is expressed in Sirea as an "arrowized" model. Sirea does not use Haskell's `Control.Arrow` (Sirea has `bfmap` instead of `arr`, and does not support Haskell's arrow syntax), but the typeclasses and structures are very similar. If you are unfamiliar with Arrows, do not fret: like Monads, Arrows are best learned by using them, by osmosis and exposure and hand-copying examples. Arrows lead to a point-free programming style (but with branches). Once you're comfortable with the structure and syntax, I recommend reading [Understanding Arrows](http://en.wikibooks.org/wiki/Haskell/Understanding_arrows) in the Haskell wikibook.



RDP can be understood as representing a continuous dataflow network, similar to a spreadsheet


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

### Parallelism and Concurrency


 

RDP supports a high level of potential parallelism. There is pipeline parallelism from (>>>) - computing a newer y and an older z in parallel. There is task parallelism from (***) - computing x' and y' at the same time. Dynamic behaviors Data parallelism is also readily supported (a `bspark` behavior is provided to help, there). 

Due to continuous semantics, RDP allows a great deal of parallelism from the first three compositions. Pipeline parallelism, via (>>>), involves pipeline parallelism (from >>>, computing a newer y and an older z in parallel), task parallelism (from *** or &&&; computing x' and y' in parallel), 

Since behaviors operate on continuous signals, the above actually represents a pipeline with potential for parallelism: fresh `x` values can be continuously computed in parallel with older `y` values. A vertical slice through the pipeline is effectively the signal type at that point, in this case `y`.

RDP models concurrency in terms of task-parallel pipelines, i.e. using `bdup` to create a parallel pipeline, then `bfirst` and `bswap` to add different tasks to different pipelines. A couple useful composite behavior is `(***)`:

### Modeling Control Flow with Behaviors

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

The `bdelay` behavior will add a constant delay to every concrete signal in `x`. In general, developers should add a small delay after any expensive operation based on a rough estimate. `bcross` should be combined with a small `bdelay` to represent the scheduling overheads. Sirea provides a simple _delay aggregation_ optimization so a lot of small delays can often be applied as one big delay just before time-sensitive effects. By adding delay, a straggling update might no longer be straggling, or even arrive slightly ahead of time (thus providing slack for concurrent operations or underestimates later in the pipeline). 

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

Declarative Effects, Resources, and Concurrency in RDP
-------------------------------------------------------

RDP is effectful, but is limited to *declarative effects* - by which I mean effects that are (at any given logical instant, i.e. *spatially*) commutative and idempotent, and that hold over a time. Declarative effects offer many of the reasoning and refactoring benefits associated with pure code: expressions can easily be rearranged, duplicate expressions can be eliminated, or duplicates can be introduced so overlapping subprograms can be abstracted. 

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

### Metacircular

It's dynamic RDP all the way down. That is the *ideal* view an RDP developer should have of any system. 

A "main" behavior is essentially a dynamic behavior activated by an unspecified lower level RDP behavior. It runs concurrently with other "main" behaviors, and might interact with them via shared services. Dynamic behaviors must be internally stateless because they're logically swapped out at every instant. The "main" behavior must be internally stateless because it is, in essence, a dynamic behavior.

This has many advantages:

* For metaprogramming. Much greater consistency and uniformity of code. It is easy to interpret text and a few capabilities into a dynamic behavior, and it won't be second class.
* For orthogonal persistence. External state can manage its own persistence, and nothing within the behavior needs persistence.
* For distribution. State models can be created that are suitable for distribution and replication. DHTs, LDAP, and content-addressed networks are possibilities.
* For live programming. The "main" behavior can be modified without disturbing state. An RDP application is one big, active declaration that can be modified at any time. 

Live programming, distributed upgrade problems, and dependency management with graceful failover (i.e. when a service you were depending on goes down) were my motivating arguments for the metacircular property.

Haskell [plugins](http://hackage.haskell.org/package/plugins) could make live programming a reality. But much design work is needed to make it livable: reactive dependency injection and linking; automatic management of threads; failover to fallback plugins while the preferred one is broken for editing. This will be pursued in a sirea-plugins package.


