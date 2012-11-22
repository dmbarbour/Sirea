
-- | RDP has a conservative notion of resources: nothing is created,
-- nothing is destroyed. That is, there is no equivalent to `new` or
-- `delete`, not even `newIORef`. Instead, resource discovery idioms
-- provide a flexible set of external resources to accommodate many
-- active clients, agents, or relationships.  
--
-- Some resources are abundant and may be "discovered" in whatever
-- quantity is needed. Much like unused directories or files in the
-- filesystem, developers use naming conventions and partitioning
-- schemes to ensure a unique resources is "discovered" for each
-- unique purpose. For example, in a GUI application, a particular
-- form may be given a subdirectory based on the name of that form.
-- If multiple instances of a form are needed, forms may be named
-- based on participating elements.  - e.g. different resource for filenames for different file
-- names. 
-- combined with the name of the files to which the form applies.
--
-- Abstractly, external resources are eternal resources. Developers
-- do not need to initialize or finalize stateful resources in RDP,
-- but some resource spaces might support reset to a default state.
-- Resources in their logical default states do not consume space on
-- a disk drive. (Other potential space-level operations include
-- splicing resource spaces, or versioning spaces.)
--
-- ResourceSpace builds on the more statically structured PCX. 
-- 
-- Modularity among mutually distrustful subprograms is achieved by
-- partitioning a resource space among subprograms. There is no `..`
-- path from child space to parent. Dynamic behavior can encapsulate
-- authority for secure interactions among partitions.
--
module Sirea.ResourceSpace
    (
    ) where

