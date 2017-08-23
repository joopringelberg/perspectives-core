module Temp.TryAffMaybe where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Maybe.Trans (MaybeT, lift)
import Data.Maybe (Maybe(..))

-- Stack Maybe on top of Aff
-- The underlying Monad is Aff.
type AffMaybe e a = MaybeT (Aff e) a

-- A function that returns an Int in this type:
returnSomething :: forall e. AffMaybe e Int
returnSomething = pure 1

-- How do we construct a function that returns Nothing in this type?
-- returnNothing :: forall e. AffMaybe e Int
-- returnNothing = do
--   pure
