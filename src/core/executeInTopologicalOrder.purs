module Perspectives.ExecuteInTopologicalOrder where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.State (StateT, evalStateT, get)
import Control.Monad.Writer (WriterT, lift, runWriterT, tell)
import Data.Array (cons, difference, elemIndex, filter, foldM, length, null)
import Data.Maybe (isJust)
import Data.Tuple (Tuple(..))
import Perspectives.Parsing.Messages (PerspectivesError(..), MultiplePerspectivesErrors)

--------------------------------------------------------------------------------------------
-- TOPOLOGICAL SORTING
-- A topological sort (see e.g. https://www.interviewcake.com/concept/java/topological-sort) of graphs.
-- There is a module Graph in Pursuit, but we cannot use it because our package set is too old.
--------------------------------------------------------------------------------------------

type ToSort = Array
type Sorted = Array
type Skipped = Array 

-- | The action must return the item, possibly transformed.
-- | The end result is the sorted list of (possibly transformed) items.
executeInTopologicalOrder :: forall m item label. MonadThrow MultiplePerspectivesErrors m => Eq label => Show label =>
  (item -> label) -> 
  (item -> Array label) ->
  ToSort item ->
  (item -> m item) ->
  m (Sorted item)
executeInTopologicalOrder getLabel getDependencies toSort action = evalStateT (executeInTopologicalOrder' toSort []) (getLabel <$> toSort)
  where
    executeInTopologicalOrder' ::
      ToSort item ->
      Sorted item ->
      (StateT (ToSort label) m) (Sorted item)
    executeInTopologicalOrder' toSort' sortedItems = do
      Tuple sortedItems' skipped <- runWriterT (foldM executeInTopologicalOrder'' sortedItems toSort')
      if null skipped
        -- If no items are left to sort, we're done.
        then pure sortedItems'
        else if length sortedItems == length sortedItems'
          -- But if our effort did not increase the number of sorted items (that is, sortedItems'), stop.
          then throwError [(Custom ("Cannot topologically sort these items (are you sure all relevant items are included?): " <> show (getLabel <$> skipped)))]
          -- Otherwise, rinse and repeat by trying to sort the skipped items!
          else executeInTopologicalOrder' skipped sortedItems'

    executeInTopologicalOrder'' ::
      Sorted item ->
      item ->
      WriterT (Skipped item) (StateT (ToSort label) m) (Sorted item)
    executeInTopologicalOrder'' sortedItems item = do
      allItems <- get
      if zeroInDegrees allItems
        -- This item has no other dependencies than those that have been handled before, so perform the action on it and tack it onto the sorted items.
        then lift $ lift $ action item >>= pure <<< flip cons sortedItems
        -- Some of the dependencies of this item have not been handled before. Push it to the skipped items and return the original sorted items.
        -- Notice that if an item has a dependency that IS NOT in the original set of items to sort, it will never be handled!
        else do
          tell [item]
          pure sortedItems
      where
        -- All of the dependencies are already sorted.
        -- INSTEAD, make sure that the item is not dependent on items that have not yet been sorted!
        zeroInDegrees :: Array label -> Boolean
        zeroInDegrees allItems = null $ (filter (isJust <<< flip elemIndex allItems) (getDependencies item)) `difference` (getLabel <$> sortedItems)

-- | Sort the items topologically, using a function that gets an items dependencies, depending on item equality.
sortTopologically  :: forall m item. MonadThrow MultiplePerspectivesErrors m => Eq item => Show item =>
  (item -> Array item) ->
  ToSort item ->
  m (Array item)
sortTopologically getDependencies toSort = executeInTopologicalOrder identity getDependencies toSort (pure <<< identity)

-- | Sort the items topologically, using a function that gets an items dependencies and a function that 
-- | returns a label from an item. Labels should be comparable using eq.
sortTopologically_  :: forall m item label. MonadThrow MultiplePerspectivesErrors m => Eq label => Show label =>
  (item -> label) ->
  (item -> Array label) ->
  ToSort item ->
  m (Array item)
sortTopologically_ getLabel getDependencies toSort = executeInTopologicalOrder getLabel getDependencies toSort (pure <<< identity)