module Perspectives.ExecuteInTopologicalOrder where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Writer (WriterT, lift, runWriterT, tell)
import Data.Array (cons, difference, foldM, length, null)
import Data.Tuple (Tuple(..), snd)
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
  m (Array item)
executeInTopologicalOrder getLabel getDependencies toSort action = snd <$> runWriterT (executeInTopologicalOrder' toSort [] 0)
  where
    executeInTopologicalOrder' ::
      ToSort item ->
      Sorted item->
      Int ->
      WriterT (Skipped item) m (Sorted item)
    executeInTopologicalOrder' toSort' sortedItems nrOfSortedItems = do
      Tuple sortedItems' skipped <- lift $ runWriterT (foldM executeInTopologicalOrder'' sortedItems toSort')
      if null skipped
        then pure sortedItems'
        else if nrOfSortedItems == length sortedItems
          then throwError [(Custom ("Cannot topologically sort these items: " <> show (getLabel <$> skipped)))]
          -- If not done, rinse and repeat!
          else executeInTopologicalOrder' skipped sortedItems' (length sortedItems)

    executeInTopologicalOrder'' ::
      Sorted item ->
      item ->
      WriterT (Skipped item) m (Sorted item)
    executeInTopologicalOrder'' sortedItems item = if zeroInDegrees
      then lift $ action item >>= pure <<< flip cons sortedItems
      else do
        tell [item]
        pure sortedItems
      where
        zeroInDegrees :: Boolean
        zeroInDegrees = null $ (getDependencies item) `difference` (getLabel <$> sortedItems)

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