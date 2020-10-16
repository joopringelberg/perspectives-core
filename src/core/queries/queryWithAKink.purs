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
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, cons, foldr, null, uncons, unsnoc)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_, traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.InvertedQuery (QueryWithAKink(..), PropsAndVerbs)
import Perspectives.Parsing.Arc.InvertQueriesForBindings (setInvertedQueriesForUserAndRole)
import Perspectives.Parsing.Arc.PhaseThree.SetInvertedQueries (setPathForStep)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, lift2, lookupVariableBinding)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.DescriptionCompiler (makeComposition)
import Perspectives.Query.Inversion (compose, inversionIsFunctional, inversionIsMandatory, invertFunction)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), domain, range, replaceDomain)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.PersistentType (getCalculatedProperty)
import Perspectives.Representation.Class.Property (calculation)
import Perspectives.Representation.Class.Role (contextOfADT, getCalculation, getRole)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (PropertyType(..), RoleType(..))
import Perspectives.Utilities (prettyPrint)
import Prelude (class Show, Unit, append, bind, discard, join, map, pure, unit, ($), (<$>), (<*>), (<<<), (<>), (>=>), (>>=))

--------------------------------------------------------------------------------------------------------------
---- QUERYWITHAKINK
--------------------------------------------------------------------------------------------------------------
-- This we use in the invert_ function.
data QueryWithAKink_ = ZQ_ (Array QueryFunctionDescription) (Maybe QueryFunctionDescription)

derive instance genericQueryWithAKink_ :: Generic QueryWithAKink_ _

instance showQueryWithAKink_ :: Show QueryWithAKink_ where
  show = genericShow

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

invert_ :: QueryFunctionDescription -> PhaseThree (Array QueryWithAKink_)
invert_ (MQD _ _ args _ _ _) = join <$> traverse invert_ args

invert_ q@(BQD dom (BinaryCombinator ComposeF) l r _ f m) = case l of
  (BQD _ (BinaryCombinator ConjunctionF) conj1 conj2 _ _ _) -> append <$> invert_ (compose conj1 (replaceDomain r (range conj1))) <*> invert_ (compose conj2 (replaceDomain r (range conj2)))

  (BQD _ (BinaryCombinator FilterF) source criterium ran _ _) -> append <$> invert_ (compose source criterium) <*> invert_ (compose source r)

  (BQD _ (BinaryCombinator ComposeF) qfd1 qfd2 ran _ _) -> invert_ (BQD dom (BinaryCombinator ComposeF) qfd1 (BQD (range qfd1) (BinaryCombinator ComposeF) qfd2 r ran f m) ran f m)

  (SQD _ (VariableLookup varName) _ _ _) -> do
    varExpr <- lookupVariableBinding varName
    case varExpr of
      Nothing -> pure []
      Just qfd -> invert_ (compose qfd r)

  (BQD _ (BinaryCombinator SequenceF) qfd1 qfd2 ran _ _) -> do
    q1 <- invert_ qfd1
    q2 <- invert_ qfd2
    pure $ join $ [q1, q2]

  otherwise -> do
    left <- invert_ l
    case uncons left of
      Just {head:(ZQ_ l_ _), tail} -> if null tail
        then do
          zippedQueries <- invert_ r
          -- Now we invert_ the order of l and r.
          pure $ cons (ZQ_ l_ (Just q))
            (map (\(ZQ_ r_ q') -> ZQ_ (r_ <> l_) q') zippedQueries)
        else throwError (Custom $ "Perspectives.Query.Zipped invert_: expected single term on the left in composition:\n" <> prettyPrint l)
      Nothing -> pure [ZQ_ [] Nothing]

invert_ (BQD _ (BinaryCombinator FilterF) source criterium _ _ _) = invert_ (compose source criterium)

invert_ (BQD _ (BinaryCombinator f) qfd1 qfd2 _ _ _) = append <$> invert_ qfd1 <*> invert_ qfd2

invert_ (UQD _ _ qfd _ _ _) = invert_ qfd

invert_ (SQD dom (Constant _ _) ran _ _) = pure []

invert_ (SQD dom (RolGetter rt) ran _ _) = case rt of
  ENR _ -> pure [ZQ_ [(SQD ran (DataTypeGetter ContextF) dom (inversionIsFunctional (RolGetter rt)) (inversionIsMandatory (RolGetter rt)))] Nothing]
  CR r -> (lift2 $ (getRole >=> getCalculation) rt) >>= invert_

invert_ (SQD dom (PropertyGetter (CP prop)) ran _ _) = (lift2 $ (getCalculatedProperty >=> calculation) prop) >>= invert_

invert_ (SQD dom (DataTypeGetter CountF) ran _ _) = pure []

  -- Treat a variable by looking up its definition (a QueryFunctionDescription), inverting it and inserting it.
invert_ (SQD dom (VariableLookup varName) _ _ _) = do
  varExpr <- lookupVariableBinding varName
  case varExpr of
    Nothing -> pure []
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
setInvertedQueries :: Map RoleType PropsAndVerbs -> QueryFunctionDescription -> PhaseThree Unit
setInvertedQueries userTypes qfd = do
  (zqs :: (Array QueryWithAKink)) <- ensureContextDomain qfd >>= invert
  for_ zqs \qwk@(ZQ backward forward) -> do
    case backward of
      (Just b@(BQD _ (BinaryCombinator ComposeF) qfd1@(SQD _ _ _ _ _) qfd2 _ _ _)) -> unsafePartial $ setPathForStep qfd1 qwk userTypes
      (Just b@(BQD _ (BinaryCombinator ComposeF) qfd1 qfd2 _ _ _)) -> throwError (Custom $ "impossible case in setInvertedQueries:\n" <> prettyPrint qfd1)
      -- Assuming an SQD otherwise
      -- (Just b) -> unsafePartial $ setPathForStep b b userTypes
      (Just b@(SQD _ _ _ _ _)) -> unsafePartial $ setPathForStep b qwk userTypes
      (Just x) -> throwError (Custom $ "impossible case in setInvertedQueries:\n" <> prettyPrint x)
      Nothing -> pure unit
    -- TODO. Voor de rol zetten we nu voor de tweede keer de InvertedQuery.
    case forward, backward, domain <$> backward of
      Nothing, Just bw, Just (RDOM role) -> (forWithIndex_ userTypes
        \user props -> setInvertedQueriesForUserAndRole user role props true qwk)
      _, _, _ -> pure unit
  where
    -- For a query that has a Role domain, we add a step from context to role.
    -- This guarantees that the inverse query has a context domain.
    -- Note that this is equivalent to addContextToPropertyQuery in Perspectives.Query.Inversion.
    ensureContextDomain :: QueryFunctionDescription -> PhaseThree QueryFunctionDescription
    ensureContextDomain q = case domain q of
      (RDOM dom@(ST et)) -> (lift $ lift $ contextOfADT dom) >>= \c -> pure $
        makeComposition (SQD (CDOM c) (RolGetter (ENR et)) (RDOM dom) Unknown Unknown) q
      otherwise -> pure q
