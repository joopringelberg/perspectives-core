module Perspectives.ExecuteInTopologicalOrder where

import Prelude

import Control.Monad.Writer (WriterT, execWriterT, lift, runWriterT, tell)
import Data.Array (cons, difference, foldM, null)
import Data.Tuple (Tuple(..))

type ToSort = Array
type Label = String
type SortedLabels = Array Label
type Skipped = Array 

-- | Note that the result will be the empty Array by construction.
executeInTopologicalOrder :: forall m item. Monad m =>
  (item -> Label) -> 
  (item -> Array Label) ->
  ToSort item ->
  (item -> m Unit) ->
  m (Array item)
executeInTopologicalOrder getLabel getDependencies toSort action = execWriterT (executeInTopologicalOrder' toSort [])
  where
    executeInTopologicalOrder' ::
      ToSort item ->
      SortedLabels ->
      WriterT (Skipped item) m SortedLabels
    executeInTopologicalOrder' toSort' sortedLabels = do
      Tuple sortedLabels' skipped <- lift $ runWriterT (foldM executeInTopologicalOrder'' sortedLabels toSort')
      if null skipped
        then pure sortedLabels'
        -- If not done, rinse and repeat!
        else executeInTopologicalOrder' skipped sortedLabels'

    executeInTopologicalOrder'' ::
      SortedLabels ->
      item ->
      WriterT (Skipped item) m SortedLabels
    executeInTopologicalOrder'' sortedLabels item = if zeroInDegrees
      then do
        lift $ action item
        pure $ cons (getLabel item) sortedLabels
      else do
        tell [item]
        pure sortedLabels
      where
        zeroInDegrees :: Boolean
        zeroInDegrees = null $ (getDependencies item) `difference` sortedLabels
