module Perspectives.GlobalUnsafeStrMap
( GlobalUnsafeStrMap
, createGlobalUnsafeStrMap )

where
  import Control.Monad.Eff (Eff)
  import Control.Monad.ST (ST)
  import Data.StrMap.ST (STStrMap)
  import Prelude (Unit)

  -- | A permanently thawed STStrMap that is tracked by an ST effect (put otherwise, a computation in Eff tagged by ST
  -- | that will yield a thawed STStrMap).
  -- | This is an unsafe index that can be modified everywhere!
  type GlobalUnsafeStrMap h a= Eff (st :: ST h) (STStrMap h a)

  -- | Create a computation in Eff that holds a permanently thawed STStrMap.
  foreign import createGlobalUnsafeStrMap :: forall h a. Unit -> GlobalUnsafeStrMap h a

  -- myMap :: forall h r. GlobalStrMap String h r
  -- myMap = createGlobalStrMap unit
  --
  -- r = do
  --   m <- myMap
  --   poke m "aap" "heeft een staart"
