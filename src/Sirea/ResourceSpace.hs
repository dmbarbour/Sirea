
-- | RDP has a conservative notion of resources: nothing is created,
-- nothing is destroyed. That is, there is no equivalent to `new` or
-- `delete`, nor `malloc` and `free`, nor even `gensym`. There is no
-- operation that can return a fresh identifier or object for use by
-- a prorammer. 
--
-- Instead, RDP models resources via a discovery mechanism similar
-- to a filesystem. Resources are located and accessed by external, 
-- stable identifiers (e.g. URI or file path). These identifiers may
-- be relative to a resource space - e.g. a particular directory of
-- a filesystem. Abstractly, resource spaces may be infinite, though
-- only a finite extent can ever be explored or utilized. Perhaps a
-- more precise description is to call such spaces "unbounded". 
--
-- Sirea heavily leverages this notion of abstract resource spaces
-- via the PCX and BCX types. However, PCX is designed for a static
-- in the application toplevel, and the associated resources are, by
-- design, most useful for binding Haskell IO. 
--
-- This module exposes a more dynamic concept of a resource space,
-- intended for use both at the toplevel (via BCX) and within RDP.
-- The dynamic resource space is suitable when working with scripts
-- or configuration files.



