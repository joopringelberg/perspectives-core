-- | Deltas are the elementary unit of change in ContextWithUpdates.
-- |
-- | **Copyright** Perspectives-IT 2017
-- |
-- | **Author** Joop Ringelberg

module Delta
  (addDelta
  , removeDelta
  , converseDelta
  , addDeltaTo
  , Delta(..)
  , Deltas
    )
where
import Prelude
import Data.Maybe (Maybe(..))
import Data.Array (elemIndex, delete, cons)

data Delta a
  = Erbij a
  | Eraf a

type Deltas a = Array (Delta a)

-- | Create an Erbij Delta
addDelta :: forall a .a -> Delta a
addDelta v = Erbij v

-- | Create an Eraf Delta
removeDelta :: forall a .a -> Delta a
removeDelta v = Eraf v

-- | Create the converse Delta, ie an Eraf for an Erbij and vv.
converseDelta :: forall a. Delta a -> Delta a
converseDelta (Erbij a) = Eraf a
converseDelta (Eraf a) = Erbij a

instance showDelta :: Show a => Show (Delta a) where
  show (Erbij a) = "(Erbij " <> show a <> ")"
  show (Eraf a) = "(Eraf " <> show a <> ")"

instance eqDelta :: Eq a => Eq (Delta a) where
  eq (Erbij a) (Erbij b) = a == b
  eq (Eraf a) (Eraf b) = a == b
  eq _ _ = false

-- | Add a Delta to an array of Deltas, removing the converse Delta if it is contained in the array instead.
addDeltaTo :: forall a. Eq a => Delta a -> Deltas a -> Deltas a
addDeltaTo d ds
  = let
      converse :: Delta a
      converse = converseDelta d
    in
      case elemIndex converse ds of
        Just i -> delete converse ds
        Nothing -> cons d ds

{-
class Updatable a where
  processDelta :: Eq a => Array a -> Delta a -> Maybe (Delta a)
-}
