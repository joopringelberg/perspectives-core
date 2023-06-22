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

import Control.Alternative (guard)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, elemIndex, foldr, intercalate, unsnoc)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), isJust)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Perspectives.CoreTypes (MP)
import Perspectives.InvertedQuery (QueryWithAKink(..), backwards)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, addBinding, lift2, lookupVariableBinding, throwError, withFrame)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.Inversion (compose, composeOverMaybe, invertFunction, queryFunctionIsFunctional, queryFunctionIsMandatory)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), RoleInContext, domain, makeComposition, range, replaceDomain, roleInContext2Role)
import Perspectives.Representation.ADT (ADT)
import Perspectives.Representation.Class.PersistentType (getCalculatedProperty)
import Perspectives.Representation.Class.Property (calculation)
import Perspectives.Representation.Class.Role (allLocallyRepresentedProperties, bindingOfADT, getCalculation, getRole)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (PropertyType(..), RoleType(..))
import Perspectives.Utilities (class PrettyPrint, prettyPrint, prettyPrint')
import Prelude (class Show, append, bind, discard, join, map, pure, ($), (<$>), (<*>), (<<<), (<>), (==), (>=>), (>>=))

--------------------------------------------------------------------------------------------------------------
---- QUERYWITHAKINK
--------------------------------------------------------------------------------------------------------------
-- This we use in the invert_ function. The first part is backwards-facing (inverted). The second part is forwards-facing (not inverted).
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
    -- | By definition this is true if the forwards part is Nothing.
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
invert_ (MQD dom (ExternalCoreRoleGetter f) args ran _ _) = pure $ [ZQ_ [SQD ran (ExternalCoreContextGetter "model://perspectives.domains#Couchdb$ContextInstances") dom Unknown Unknown] Nothing]

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

  (BQD _ (BinaryCombinator FilledByF) conj1 conj2 _ _ _) -> append <$>
    invert_ (compose conj1 (replaceDomain r (range conj1))) <*>
    invert_ (compose conj2 (replaceDomain r (range conj2)))

  (BQD _ (BinaryCombinator SequenceF) qfd1 qfd2 ran _ _) -> do
    q1 <- invert_ qfd1
    q2 <- invert_ qfd2
    pure $ join $ [q1, q2]

  qq@(SQD _ (VariableLookup varName) _ _ _) -> do
    varExpr <- lookupVariableBinding varName
    case varExpr of
      Nothing -> pure []
      Just qfd | qq == qfd -> pure []
      Just qfd -> invert_ (compose qfd r)

  otherwise -> do
    (lefts :: Array QueryWithAKink_) <- invert_ l
    (rights :: Array QueryWithAKink_) <- invert_ r
    case lefts, rights of 
      [], _ -> pure rights
      _, [] -> pure lefts
      _, _ -> pure do
        (ZQ_ left_inverted_steps mLeft_forward) <- lefts
        (ZQ_ right_inverted_steps mRight_forward) <- rights
        -- The range of mLeft_forward must equal the domain of mRight_forward.
        guard $ case range <$> mLeft_forward, domain <$> mRight_forward of
          Just ran, Just domn -> ran == domn
          -- If the forward of the left part is Nothing, left has been inverted entirely and 
          -- may be combined with any inversion of right.
          Nothing, _ -> true
          _, _ -> false
        pure $ ZQ_ (right_inverted_steps <> left_inverted_steps) -- v4.value0 <> v3.value0
                    (mLeft_forward `composeOverMaybe` mRight_forward)

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
      minvertedF <- invertFunction dom f ran
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
  (minvertedF :: Maybe QueryFunction) <- invertFunction dom f ran
  case minvertedF of
    Nothing -> pure []
    Just invertedF -> pure [ZQ_ [(SQD ran invertedF dom (queryFunctionIsFunctional invertedF) (queryFunctionIsMandatory f))] Nothing]

-- Catchall.
invert_ q = throwError (Custom $ "Missing case in invert for: " <> prettyPrint q)

