
-- | Implementation of the Dynamic behavior type for B.
module Sirea.Internal.BDynamic 
    (
    ) where


-- The basic technique:
--  Will maintain multiple dynamic behaviors at once - i.e. past, present, future.
--  Will generate a new receiver Lnk for each dynamic behavior. 
--  New receiver nodes primarily provide identity for merges.
--  Will eventually clear `old` dynamic behaviors, i.e. after fully stable.



