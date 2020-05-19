-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

module Perspectives.Query.Zipped where

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Array (null, uncons)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.Inversion (compose)
import Perspectives.Query.QueryTypes (QueryFunctionDescription(..), range)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Prelude (append, bind, join, map, pure, show, ($), (<$>), (<*>), (<>))

--------------------------------------------------------------------------------------------------------------
---- ZIPPEDQUERY
--------------------------------------------------------------------------------------------------------------
-- | A ZippedQuery represents a query as seen from a specific station (context or role) that is visited by some
-- | original query. The forwards part describes a query that will run from the station to its original query's end;
-- | the backwards part is an ordered array of steps that can be composed to create a query that will run from
-- | the station to the original queries beginning.
-- | The steps are understood to be all Simple Query Function Descriptions constructed with SQD.
data ZippedQuery = ZQ (Array QueryFunctionDescription) QueryFunctionDescription

forwards :: ZippedQuery -> QueryFunctionDescription
forwards (ZQ _ qfd) = qfd

backwards :: ZippedQuery -> Array QueryFunctionDescription
backwards (ZQ steps _) = steps

--------------------------------------------------------------------------------------------------------------
---- INVERT
--------------------------------------------------------------------------------------------------------------
invert :: forall m. MonadError PerspectivesError m => QueryFunctionDescription -> m (Array ZippedQuery)
invert (MQD _ _ args _ _ _) = join <$> traverse invert args

invert (BQD _ (BinaryCombinator ComposeF) (BQD _ (BinaryCombinator ConjunctionF) conj1 conj2 _ _ _) qfd2 _ _ _) = append <$> invert (compose conj1 qfd2) <*> invert (compose conj2 qfd2)

invert (BQD _ (BinaryCombinator ComposeF) (BQD dom (BinaryCombinator FilterF) source criterium ran _ _) qfd2 _ _ _) =
  append <$> invert (compose source criterium) <*> invert (compose source qfd2)

invert (BQD dom (BinaryCombinator ComposeF) (BQD _ (BinaryCombinator ComposeF) qfd1 qfd2 _ _ _) qfd3 ran f m) =
  invert (BQD dom (BinaryCombinator ComposeF) qfd1 (BQD (range qfd1) (BinaryCombinator ComposeF) qfd2 qfd3 ran f m) ran f m)

invert (BQD _ (BinaryCombinator ComposeF) qfd1 (BQD _ (BinaryCombinator FilterF) source criterium ran _ _) _ _ _) = let
  f = compose qfd1 source in
  append <$> invert (compose f criterium) <*> invert f

-- We've now guaranteed right association. We assume a single path comes from left.
invert q@(BQD _ (BinaryCombinator ComposeF) l r _ _ _) = do
  left <- invert l
  case uncons left of
    Just {head, tail} -> if null tail
      then do
        zippedQueries <- invert r
        pure $ zippedQueries <> map (\zippedQuery -> ZQ ((backwards zippedQuery) <> (backwards head)) q)
          zippedQueries
      else throwError (Custom $ "Perspectives.Query.Zipped invert: expected single term on the left in composition: " <> show l)
    Nothing -> throwError (Custom $ "Perspectives.Query.Zipped invert: query gives no inversion: " <> show l)

invert (BQD _ (BinaryCombinator f) qfd1 qfd2 _ _ _) = append <$> invert qfd1 <*> invert qfd2

invert (UQD _ _ qfd _ _ _) = invert qfd


-- Catchall: remove when ready!
invert q = pure [(ZQ [] q)]
