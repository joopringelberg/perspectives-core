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

module Perspectives.Query.Kinked where

import Control.Monad.Error.Class (throwError)
import Data.Array (null, uncons)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, lift2, lookupVariableBinding)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.Inversion (compose, inversionIsFunctional, inversionIsMandatory, invertFunction)
import Perspectives.Query.QueryTypes (QueryFunctionDescription(..), range)
import Perspectives.Representation.Class.PersistentType (getCalculatedProperty)
import Perspectives.Representation.Class.Property (calculation)
import Perspectives.Representation.Class.Role (getCalculation, getRole)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (PropertyType(..), RoleType(..))
import Perspectives.Utilities (prettyPrint)
import Prelude (append, bind, join, map, pure, ($), (<$>), (<*>), (<>), (>=>), (>>=))

--------------------------------------------------------------------------------------------------------------
---- ZIPPEDQUERY
--------------------------------------------------------------------------------------------------------------
-- | A QueryWithAKink represents a query as seen from a specific station (context or role) that is visited by some
-- | original query. The forwards part describes a query that will run from the station to its original query's end;
-- | the backwards part is a query that will run from the station to the original queries beginning.
data QueryWithAKink = ZQ QueryFunctionDescription QueryFunctionDescription

forwards :: QueryWithAKink -> QueryFunctionDescription
forwards (ZQ _ forward) = forward

backwards :: QueryWithAKink -> QueryFunctionDescription
backwards (ZQ backward _) = backward

--------------------------------------------------------------------------------------------------------------
---- INVERT
--------------------------------------------------------------------------------------------------------------
-- | Invert
invert :: QueryFunctionDescription -> PhaseThree (Array QueryWithAKink)
invert (MQD _ _ args _ _ _) = join <$> traverse invert args

invert q@(BQD dom (BinaryCombinator ComposeF) l r _ f m) = case l of
  (BQD _ (BinaryCombinator ConjunctionF) conj1 conj2 _ _ _) -> append <$> invert (compose conj1 r) <*> invert (compose conj2 r)

  (BQD _ (BinaryCombinator FilterF) source criterium ran _ _) -> append <$> invert (compose source criterium) <*> invert (compose source r)

  (BQD _ (BinaryCombinator ComposeF) qfd1 qfd2 ran _ _) -> invert (BQD dom (BinaryCombinator ComposeF) qfd1 (BQD (range qfd1) (BinaryCombinator ComposeF) qfd2 r ran f m) ran f m)

  otherwise -> do
    left <- invert l
    case uncons left of
      Just {head, tail} -> if null tail
        then do
          zippedQueries <- invert r
          -- Now we invert the order of l and r.
          pure $ zippedQueries <> map (\zippedQuery -> ZQ (compose (backwards zippedQuery) (backwards head)) q)
            zippedQueries
        else throwError (Custom $ "Perspectives.Query.Zipped invert: expected single term on the left in composition: " <> prettyPrint l)
      Nothing -> throwError (Custom $ "Perspectives.Query.Zipped invert: query gives no inversion: " <> prettyPrint l)

invert (BQD _ (BinaryCombinator FilterF) source criterium _ _ _) = invert (compose source criterium)

invert (BQD _ (BinaryCombinator f) qfd1 qfd2 _ _ _) = append <$> invert qfd1 <*> invert qfd2

invert (UQD _ _ qfd _ _ _) = invert qfd

invert (SQD dom (Constant _ _) ran _ _) = pure []

invert q@(SQD dom (RolGetter rt) ran _ _) = case rt of
  ENR _ -> pure [ZQ (SQD ran (DataTypeGetter ContextF) dom (inversionIsFunctional (RolGetter rt)) (inversionIsMandatory (RolGetter rt))) q]
  CR r -> (lift2 $ (getRole >=> getCalculation) rt) >>= invert

invert (SQD dom (PropertyGetter (CP prop)) ran _ _) = (lift2 $ (getCalculatedProperty >=> calculation) prop) >>= invert

invert (SQD dom (DataTypeGetter CountF) ran _ _) = pure []

  -- Treat a variable by looking up its definition (a QueryFunctionDescription), inverting it and inserting it.
invert (SQD dom (VariableLookup varName) _ _ _) = do
  varExpr <- lookupVariableBinding varName
  case varExpr of
    Nothing -> pure []
    Just qfd -> invert qfd

invert q@(SQD dom f ran _ _) = do
  minvertedF <- pure $ invertFunction dom f ran
  case minvertedF of
    Nothing -> pure []
    Just invertedF -> pure [ZQ (SQD ran invertedF dom (inversionIsFunctional f) (inversionIsMandatory f)) q]

-- Catchall.
invert q = throwError (Custom $ "Missing case in invertFunctionDescription_ for: " <> prettyPrint q)
