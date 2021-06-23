-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
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
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

module Perspectives.Query.Kinked where

import Control.Monad.Error.Class (throwError)
import Data.Array (catMaybes, concat, cons, foldr, fromFoldable, intercalate, null, uncons, union, unsnoc)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map, values)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_, traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.InvertedQuery (QueryWithAKink(..))
import Perspectives.Parsing.Arc.InvertQueriesForBindings (setInvertedQueriesForUserAndRole)
import Perspectives.Parsing.Arc.PhaseThree.SetInvertedQueries (setPathForStep)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, addBinding, lift2, lookupVariableBinding, withFrame)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.Inversion (compose, inversionIsFunctional, inversionIsMandatory, invertFunction)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), domain, range, replaceDomain)
import Perspectives.Representation.Class.PersistentType (StateIdentifier, getCalculatedProperty)
import Perspectives.Representation.Class.Property (calculation)
import Perspectives.Representation.Class.Role (getCalculation, getRole)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (PropertyType(..), RoleType(..))
import Perspectives.Utilities (class PrettyPrint, prettyPrint, prettyPrint')
import Prelude (class Show, Unit, append, bind, discard, join, map, pure, unit, void, ($), (<$>), (<*>), (<<<), (<>), (==), (>=>), (>>=))

--------------------------------------------------------------------------------------------------------------
---- QUERYWITHAKINK
--------------------------------------------------------------------------------------------------------------
-- This we use in the invert_ function.
data QueryWithAKink_ = ZQ_ (Array QueryFunctionDescription) (Maybe QueryFunctionDescription)

derive instance genericQueryWithAKink_ :: Generic QueryWithAKink_ _

instance showQueryWithAKink_ :: Show QueryWithAKink_ where
  show = genericShow

instance prettyPrintQueryWithAKink_ :: PrettyPrint QueryWithAKink_ where
  prettyPrint' tab (ZQ_ qfds mqfd) = "QueryWithAKink_\n[" <> (intercalate (",\n" <> tab) (prettyPrint' (tab <> "  ") <$> qfds) <> "]\n" <> prettyPrint' (tab <> "  ") mqfd)
--------------------------------------------------------------------------------------------------------------
---- INVERT
--------------------------------------------------------------------------------------------------------------
-- | From a query, return a collection of paths that run from some station in the original direction to the query end
-- | result and in reverse direction to the query start.
-- | The original query is 'kinked' at the station.
-- | The result is a collection of all possible inverse paths, kinked at all possible stations.
-- | As the domain of the original query can be either a ContextType, or an EnumeratedRoleType, the range of the
-- | inverted parts can be a ContextType or an EnumeratedRoleType, too.
invert :: QueryFunctionDescription -> PhaseThree (Array QueryWithAKink)
invert = invert_ >=> traverse h >=> pure <<< catMaybes
  where
    h :: QueryWithAKink_ -> PhaseThree (Maybe QueryWithAKink)
    h (ZQ_ steps q) = case unsnoc steps of
      Nothing -> pure Nothing
      -- Creates a right-associative composition that preserves the order in steps.
      Just {init, last} -> pure $ Just $ ZQ (Just $ foldr compose last init) q

-- | The QueryFunctionDescriptions in the Array of each QueryWithAKink_
-- | are inversed wrt the orinal query.
invert_ :: QueryFunctionDescription -> PhaseThree (Array QueryWithAKink_)
invert_ (MQD dom (ExternalCoreRoleGetter f) args ran _ _) = pure $ [ZQ_ [SQD ran (ExternalCoreContextGetter "model:Couchdb$ContextInstances") dom Unknown Unknown] Nothing]

invert_ (MQD _ _ args _ _ _) = join <$> traverse invert_ args

invert_ q@(BQD dom (BinaryCombinator ComposeF) l r _ f m) = case l of
  (BQD _ (BinaryCombinator UnionF) conj1 conj2 _ _ _) -> append <$>
    invert_ (compose conj1 (replaceDomain r (range conj1))) <*>
    invert_ (compose conj2 (replaceDomain r (range conj2)))

  (BQD _ (BinaryCombinator IntersectionF) conj1 conj2 _ _ _) -> append <$> invert_ (compose conj1 (replaceDomain r (range conj1))) <*> invert_ (compose conj2 (replaceDomain r (range conj2)))

  (BQD _ (BinaryCombinator FilterF) source criterium ran _ _) -> append <$> invert_ (compose source criterium) <*> invert_ (compose source r)

  (BQD _ (BinaryCombinator ComposeF) qfd1 qfd2 ran _ _) -> invert_ (BQD dom (BinaryCombinator ComposeF) qfd1 (BQD (range qfd1) (BinaryCombinator ComposeF) qfd2 r ran f m) ran f m)

  qq@(SQD _ (VariableLookup varName) _ _ _) -> do
    varExpr <- lookupVariableBinding varName
    case varExpr of
      Nothing -> pure []
      Just qfd | qq == qfd -> pure []
      Just qfd -> invert_ (compose qfd r)

  (BQD _ (BinaryCombinator SequenceF) qfd1 qfd2 ran _ _) -> do
    q1 <- invert_ qfd1
    q2 <- invert_ qfd2
    pure $ join $ [q1, q2]

  otherwise -> do
    (left :: Array QueryWithAKink_) <- invert_ l
    case uncons left of
      Just {head:(ZQ_ l_ _), tail} -> if null tail
        then do
          -- The inversion of left yielded just a single QueryWithAKink_.
          zippedQueries <- invert_ r
          -- Add the kinked query consisting of the first (inverted) step as the left part
          -- and the (not inverted) rest of the steps as the right part.
          pure $ cons (ZQ_ l_ (Just r))
            -- Append the steps found in that single QueryWithAKink_ to the end
            -- of each series of steps resulting from inverting the right.
            (map (\(ZQ_ r_ q') -> ZQ_ (r_ <> l_) q') zippedQueries)
        -- TODO. In het algemene geval kan de linkerkant wel degelijk meerdere
        -- resultaten opleveren: denk aan een CalculatedRole met een join.
        else throwError (Custom $ "Perspectives.Query.Zipped invert_: expected single term on the left in composition:\n" <> prettyPrint left)
      Nothing -> pure []

invert_ (BQD _ (BinaryCombinator FilterF) source criterium _ _ _) = invert_ (compose source criterium)

invert_ (BQD _ (BinaryCombinator f) qfd1 qfd2 _ _ _) = append <$> invert_ qfd1 <*> invert_ qfd2

-- We balance VariableLookup, where we invert the expression we look up,
-- simply by storing the compiled expression under the variable name.
invert_ (UQD _ (BindVariable varName) qfd _ _ _) = do
  addBinding varName qfd
  -- As we invert the expression that the variable is bound to
  -- each time we refer it, we do not need invert it here.
  pure []

-- Push a frame. We will encounter BindVariable instances in the first
-- part of the seqence that is the qfd.
invert_ (UQD _ WithFrame qfd _ _ _) = do
  withFrame (invert_ qfd)

invert_ (UQD _ _ qfd _ _ _) = invert_ qfd

invert_ (SQD dom (Constant _ _) ran _ _) = pure []

invert_ (SQD dom (RolGetter rt) ran _ _) = case rt of
  ENR _ -> pure [ZQ_ [(SQD ran (DataTypeGetter ContextF) dom (inversionIsFunctional (RolGetter rt)) (inversionIsMandatory (RolGetter rt)))] Nothing]
  CR r -> (lift2 $ (getRole >=> getCalculation) rt) >>= invert_

invert_ (SQD dom (PropertyGetter (CP prop)) ran _ _) = (lift2 $ (getCalculatedProperty >=> calculation) prop) >>= invert_

invert_ (SQD dom (DataTypeGetter CountF) ran _ _) = pure []

  -- Treat a variable by looking up its definition (a QueryFunctionDescription), inverting it and inserting it.
invert_ q@(SQD dom (VariableLookup varName) _ _ _) = do
  varExpr <- lookupVariableBinding varName
  case varExpr of
    Nothing -> pure []
    Just qfd | qfd == q -> pure []
    Just qfd -> invert_ qfd

invert_ (SQD dom f ran _ _) = do
  minvertedF <- pure $ invertFunction dom f ran
  case minvertedF of
    Nothing -> pure []
    Just invertedF -> pure [ZQ_ [(SQD ran invertedF dom (inversionIsFunctional f) (inversionIsMandatory f))] Nothing]

-- Catchall.
invert_ q = throwError (Custom $ "Missing case in invert for: " <> prettyPrint q)

--------------------------------------------------------------------------------------------------------------
---- SET INVERTED QUERIES
--------------------------------------------------------------------------------------------------------------
setInvertedQueries ::
  Array RoleType ->
  Map PropertyType (Array StateIdentifier) ->
  Array StateIdentifier ->
  QueryFunctionDescription ->
  PhaseThree Unit
setInvertedQueries users statesPerProperty roleStates qfd = do
  -- log ("setInvertedQueries:" <> "\n users =" <> show users <> "\n states = " <> show roleStates <> "\n statesPerProperty = " <> showTree statesPerProperty <> "\n qfd = " <> show qfd)
  (zqs :: (Array QueryWithAKink)) <- invert qfd
  for_ zqs \qwk@(ZQ backward forward) -> do
    -- What is confusing about what follows is that it just seems to handle the first step of an inverted query.
    -- What about the steps that follow?
    -- Reflect that we have generated *separate inverted queries* for all these steps, each 'kinking' the original query
    -- at a different position.
    -- So if the original query was:
    --    s1 >> s2 >> s3
    -- we generate:
    --    backwards (forwards)
    --    ^s1 (s2 >> s3)          and for this we store an InvertedQuery with s1
    --    ^s2 >> ^s1 (s3)         and for this we store an InvertedQuery with s2
    --    ^s3 >> ^s2 >> ^s1 ()    and for this we store an InvertedQuery with s3

    -- Handle two cases of the backward query:
    --  * SQD >> (...), i.e. when the backward part is a composition (whose first step is not a composition, that would be an error!). Set the InvertedQuery with respect to the first step of backward.
    --  * SQD, i.e. when the backward part is just a single step. Set the InvertedQuery for that step.
    case backward of
      (Just b@(BQD _ (BinaryCombinator ComposeF) qfd1@(SQD _ _ _ _ _) qfd2 _ _ _)) -> unsafePartial $ setPathForStep qfd1 qwk users (roleStates `union` (concat $ fromFoldable $ values statesPerProperty)) statesPerProperty
      (Just b@(BQD _ (BinaryCombinator ComposeF) qfd1 qfd2 _ _ _)) -> throwError (Custom $ "impossible case in setInvertedQueries:\n" <> prettyPrint qfd1)
      -- TODO. Doubleert dit niet met de case hieronder, voor setInvertedQueriesForUserAndRole?
      (Just b@(SQD _ _ _ _ _)) -> unsafePartial $ setPathForStep b qwk users (roleStates `union` (concat $ fromFoldable $ values statesPerProperty)) statesPerProperty
      (Just x) -> throwError (Custom $ "impossible case in setInvertedQueries:\n" <> prettyPrint x)
      Nothing -> pure unit
    -- Handle the endpoint of the original query when it ends in a Role. There are two cases:
    -- a Perspective Object, where the perspective may have properties;
    -- an expression in a statement with a Property value.
    case forward, backward, domain <$> backward of
      Nothing, Just bw, Just (RDOM role) ->
        void $ setInvertedQueriesForUserAndRole users role statesPerProperty true qwk
      _, _, _ -> pure unit
