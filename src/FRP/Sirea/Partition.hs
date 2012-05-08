{-# LANGUAGE TypeOperators, EmptyDataDecls #-}

module FRP.Sirea.Partition 
    (
    ) where

import Data.Typeable
import 



{-

TWO TYPES OF PARTITION:
  bcross - between true partitions
  bscope - between thread-local partitions

limited number of comm batch-update between threads
- combine batches and updates if one thread is faster than another (efficiency)
- ensure progress of all threads is relatively close (but not lockstep)
- limits memory consumption (max # in-flight batches and updates)
- limits amount of computation in step (max # incoming batches)

Threads can have other responsibilities
- e.g. maintain GLUT window. 

NOT SUPPORTED BY SIREA (BUT DOCUMENTED ANYWAY)
  bremote - remote partitions, like bcross but extra constraints:
     serializable message
     extra response type to model disruption

Support for easy integration with imperative ops within threads.
  partition-specific variables, observable in RDP.
  may set future values, observe current values. 
  (named by type)
  update only in partition's thread
  observe only via RDP; updates not observable until next RDP step

  linear variables, may be assigned by exactly one source?
    (build-time error if used more than once).
  
  

bscope is free. It is useful if:
* you need more partition types (e.g. to model distinct resources, objects)
* you want to more precisely organize your application

CONFIGURATION:
  a typeclass that operates in Control.Make to build the threads


Sirea also provides a logical, thread-local variation of partitioning called `bscope`. Scopes can provide some extra structure to an application, and provide a decent way to organize resources (e.g. model multiple scopes). Scopes are useful because resources are often associated with specific partitions by type, and scopes allow you to model more partition types without adding threads. 


Note that you can specify `bfmap` and similar in other partitions. This corresponds to code distribution. Between Haskell threads, it is trivial. But RDP is designed for code distribution between processes.


In general, developers should seek to minimize use of bcross. Even between Haskell threads, considerable latency may be introduced due to scheduling. To help model this, each use of `bcross` may hide an implicit `bdelay` based on configuration. 

-}

