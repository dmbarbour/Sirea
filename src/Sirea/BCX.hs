
-- | Behavior with Context.
--
-- BCX distributes a value representing an application's resource
-- context, a `PCX`, to every behavior component that needs it. The
-- resource context is for volatile resources: threads, connections, 
-- caches, demand monitors, stateless stability models. Persistent 
-- resources often need volatile proxies. (Application state should
-- be backed by persistent resources.)
--
-- Using PCX rather than Haskell global space is convenient via BCX,
-- and robust for multiple instances and plugins.
--
-- BCX and PCX enable declarative, type-driven resource acquisition
-- in Sirea. To have two behaviors observe and influence one GLUT
-- window, use the same type to identify the window resources. The
-- behaviors can access the PCX to hook elements together by type.
--
-- Types aren't essential to this design, but are convenient and
-- only weakly constrained by Haskell's module namespace. (You can
-- protect resources against casual access by hiding their types.)
-- 
-- NOTE: RDP is designed for secure programming in object capability
-- languages. A good object capability language will have convenient
-- syntax and module systems for distributing capabilities and for
-- controlling their distribution. This use of global state with BCX
-- irks me. But Haskell is not an object capability language. Sirea
-- needs to be convenient for Haskell.
-- 

module Sirea.BCX
    (
    ) where




