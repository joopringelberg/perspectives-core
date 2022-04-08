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
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, cons, elemIndex, foldr, intercalate, null, uncons, unsnoc)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Map (Map)
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (for_, traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MP, MonadPerspectives)
import Perspectives.InvertedQuery (QueryWithAKink(..), backwards)
import Perspectives.Parsing.Arc.InvertQueriesForBindings (setInvertedQueriesForUserAndRole)
import Perspectives.Parsing.Arc.PhaseThree.SetInvertedQueries (makeComposition, storeInvertedQuery)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, PhaseTwo', addBinding, lift2, lookupVariableBinding, withFrame)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.Inversion (compose, queryFunctionIsFunctional, queryFunctionIsMandatory, invertFunction)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), RoleInContext, domain, range, replaceDomain, roleInContext2Role)
import Perspectives.Representation.ADT (ADT)
import Perspectives.Representation.Class.PersistentType (StateIdentifier, getCalculatedProperty)
import Perspectives.Representation.Class.Property (calculation)
import Perspectives.Representation.Class.Role (allLocallyRepresentedProperties, bindingOfADT, getCalculation, getRole)
import Perspectives.Representation.Perspective (ModificationSummary)
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
---- COMPLETEINVERSIONS
--------------------------------------------------------------------------------------------------------------
-- | Inverted queries without a backwards part contribute nothing to the result of this function.
completeInversions :: QueryFunctionDescription -> PhaseThree (Array QueryFunctionDescription)
completeInversions = invert >=> pure <<< catMaybes <<< map f
  where
    f :: QueryWithAKink -> Maybe QueryFunctionDescription
    f qwk = if isCompleteInverse qwk then backwards qwk else Nothing

    -- | Is the original query completely inversed? (as opposed to: is this a partial inversion).
    isCompleteInverse :: QueryWithAKink -> Boolean
    isCompleteInverse (ZQ _ Nothing) = true
    isCompleteInverse _ = false


--------------------------------------------------------------------------------------------------------------
---- INVERT
--------------------------------------------------------------------------------------------------------------
-- | From a query, return a collection of paths that run from some station in the original direction to the query end
-- | result and in reverse direction to the query start.
-- | The original query is 'kinked' at the station.
-- | The result is a collection of all possible inverse paths, kinked at all possible stations.
-- | As the domain of the original query can be either a ContextType, or an EnumeratedRoleType, the range of the
-- | inverted parts can be a ContextType or an EnumeratedRoleType, too.
-- |
-- | FILTERS and LET (WithFrame)
-- | Filters are completely ignored in the code below. On the type level, this does not matter as unfiltered queries
-- | have the same type as filtered versions. Runtime, things will have impact on performance and sometimes on
-- | semantics. See the text Filtering Inverted Queries.docx.
-- | INVARIANT: All QueryWithAKink instances will have a backwards part.
invert :: QueryFunctionDescription -> PhaseThree (Array QueryWithAKink)
invert = invert_ >=> traverse h >=> pure <<< catMaybes
  where
    h :: QueryWithAKink_ -> PhaseThree (Maybe QueryWithAKink)
    h (ZQ_ steps q) = case unsnoc steps of
      -- Remove candidates without a backwards part.
      Nothing -> pure Nothing
      -- Creates a right-associative composition that preserves the order in steps.
      Just {init, last} -> pure $ Just $ ZQ (Just $ foldr compose last init) q

-- | The QueryFunctionDescriptions in the Array of each QueryWithAKink_
-- | are inversed wrt the orinal query.
invert_ :: QueryFunctionDescription -> PhaseThree (Array QueryWithAKink_)
-- NOTE moeten we hier niet iets met de args?
invert_ (MQD dom (ExternalCoreRoleGetter f) args ran _ _) = pure $ [ZQ_ [SQD ran (ExternalCoreContextGetter "model:Couchdb$ContextInstances") dom Unknown Unknown] Nothing]

invert_ (MQD _ _ args _ _ _) = join <$> traverse invert_ args

invert_ q@(BQD dom (BinaryCombinator ComposeF) l r _ f m) = case l of
  (BQD _ (BinaryCombinator UnionF) conj1 conj2 _ _ _) -> append <$>
    invert_ (compose conj1 (replaceDomain r (range conj1))) <*>
    invert_ (compose conj2 (replaceDomain r (range conj2)))

  (BQD _ (BinaryCombinator IntersectionF) conj1 conj2 _ _ _) -> append <$>
    invert_ (compose conj1 (replaceDomain r (range conj1))) <*>
    invert_ (compose conj2 (replaceDomain r (range conj2)))

  (BQD _ (BinaryCombinator FilterF) source criterium ran _ _) -> append <$> invert_ (compose source criterium) <*> invert_ (compose source r)

  (BQD _ (BinaryCombinator ComposeF) qfd1 qfd2 ran _ _) -> invert_ (BQD dom (BinaryCombinator ComposeF) qfd1 (BQD (range qfd1) (BinaryCombinator ComposeF) qfd2 r ran f m) ran f m)

  (BQD _ (BinaryCombinator BindsF) conj1 conj2 _ _ _) -> append <$>
    invert_ (compose conj1 (replaceDomain r (range conj1))) <*>
    invert_ (compose conj2 (replaceDomain r (range conj2)))

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
          -- Now invert the right term of the composition. This will yield all
          -- possible ways to `kink` the right term.
          zippedQueries <- invert_ r
          -- Add the bottom case, consisting of the inverted left term
          -- and the original right term...
          pure $ cons (ZQ_ l_ (Just r))
            -- To the rest of the cases. These consist of the combination of the inverted left term
            -- with all results of the right term.
            -- Each right-term-inversion-result consists of a backwards- and forwards facing part.
            -- We must add the inverted left term to THE END of the backwards facing part of each.
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
  ENR _ -> pure [ZQ_ [(SQD ran (DataTypeGetter ContextF) dom True True)] Nothing]
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

invert_ qfd@(SQD dom@(RDOM roleAdt) f@(PropertyGetter prop@(ENP _)) ran fun man) = do
  (hasProp :: Boolean) <- lift $ lift $ roleHasProperty roleAdt
  if hasProp
    then do
      minvertedF <- pure $ invertFunction dom f ran
      case minvertedF of
        Nothing -> pure []
        Just invertedF -> pure [ZQ_ [(SQD ran invertedF dom True True)] Nothing]
    else ((lift $ lift (expandPropertyQuery roleAdt)) >>= invert_)

  where
    -- Creates a series of nested binding expressions until the property has been reached.
    expandPropertyQuery :: ADT RoleInContext -> MP QueryFunctionDescription
    expandPropertyQuery adt = do
      hasProp <- roleHasProperty adt
      if hasProp
        then pure (SQD (RDOM adt) (PropertyGetter prop) ran fun man)
        else makeComposition <$> makeBinding adt <*> (bindingOfADT adt >>= expandPropertyQuery)

      where
      makeBinding :: ADT RoleInContext -> MP QueryFunctionDescription
      makeBinding adt' = do
        b <- bindingOfADT adt'
        pure (SQD (RDOM adt') (DataTypeGetter BindingF) (RDOM b) True False)

    roleHasProperty :: ADT RoleInContext -> MP Boolean
    roleHasProperty adt = allLocallyRepresentedProperties (roleInContext2Role <$> adt) >>= pure <<< isJust <<< (elemIndex prop)

invert_ (SQD dom f ran _ _) = do
  (minvertedF :: Maybe QueryFunction) <- pure $ invertFunction dom f ran
  case minvertedF of
    Nothing -> pure []
    Just invertedF -> pure [ZQ_ [(SQD ran invertedF dom (queryFunctionIsFunctional invertedF) (queryFunctionIsMandatory f))] Nothing]

-- Catchall.
invert_ q = throwError (Custom $ "Missing case in invert for: " <> prettyPrint q)

--------------------------------------------------------------------------------------------------------------
---- SET INVERTED QUERIES
--------------------------------------------------------------------------------------------------------------
type WithModificationSummary = ReaderT ModificationSummary (PhaseTwo' MonadPerspectives)

-- | Modifies the DomeinFile in PhaseTwoState.
setInvertedQueries ::
  Array RoleType ->
  Map PropertyType (Array StateIdentifier) ->
  Array StateIdentifier ->
  QueryFunctionDescription ->
  Boolean ->
  WithModificationSummary Unit
setInvertedQueries users statesPerProperty roleStates qfd selfOnly = do
  -- log ("setInvertedQueries:" <> "\n users =" <> show users <> "\n states = " <> show roleStates <> "\n statesPerProperty = " <> showTree statesPerProperty <> "\n qfd = " <> show qfd)
  (zqs :: (Array QueryWithAKink)) <- lift $ invert qfd

  for_ zqs \qwk@(ZQ backward forward) -> do

      -- Store the QueryWithAKink.
      storeInvertedQuery qwk users roleStates statesPerProperty selfOnly

      -- If the original query represents the object of a perspective, we need to do more work
      -- to handle the properties in the perspective.
      -- We do this just for the full inversion of the object query, i.e. when there is no forward part.
      -- To ensure we're not dealing with a property definition, we check that the Domain of the backwards part is
      -- an RDOM (role) domain (the inversion of a property definition query would start with a VDOM (property) domain).
      case forward, backward, domain <$> backward of
        Nothing, Just bw, Just (RDOM role) ->
          void $ unsafePartial $
            setInvertedQueriesForUserAndRole
              bw
              users
              role
              statesPerProperty
              qwk
              selfOnly
        _, _, _ -> pure unit
