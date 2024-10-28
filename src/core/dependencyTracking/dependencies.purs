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

module Perspectives.DependencyTracking.Dependency where

-- | A Dependency is a combination of a resource (ContextInstance or RoleInstance) and a type
-- | (EnumeratedRoleType, CalculatedRoleType, EnumeratedPropertyType or CalculatedPropertyType).
-- | However, in the dependency administration we omit these newMaptypes.

import Prelude

import Data.Array (concat, delete, elemIndex, filter, partition, (:))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Foreign.Object (Object, insert, lookup, singleton, values)
import Persistence.Attachment (class Attachment)
import Perspectives.ApiTypes (ApiEffect, Response(..), CorrelationIdentifier)
import Perspectives.CoreTypes (class Persistent, type (~~>), ArrayWithoutDoubles, Assumption, InformedAssumption(..), MP, assumption, runMonadPerspectivesQuery, (###=))
import Perspectives.GlobalUnsafeStrMap (GLStrMap, newMap, peek, poke, delete) as GLS
import Perspectives.ModelDependencies (indexedContextFuzzies)
import Perspectives.Persistent (entityExists)
import Perspectives.PerspectivesState (queryAssumptionRegister, queryAssumptionRegisterModify)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedPropertyType, EnumeratedRoleType(..))
import Perspectives.Types.ObjectGetters (roleAspectsClosure)

-- | Execute an EffectRunner to re-create an effect. This should be done whenever one or more assumptions underlying
-- | the computation of the query that delivers the results to the ApiEffect, has changed.
type ApiEffectRunner = Unit -> MP Unit

-- | A SupportedEffect combines an EffectRunner and an array of Assumptions on which it relies.
-- | We use the Assumptions to unregister an entry from the AssumptionRegister, when the ApiEffect is retracted.
type SupportedEffect = {runner :: ApiEffectRunner, assumptions:: Array Assumption}

-- | A SupportedEffect is in the ActiveSupportedEffects after it has been requested through the API and
-- | for as long as it is not retracted. As soon as an Assumption changes, the active SupportedEffects that
-- | rely on it, are re-executed.
-- | A SupportedEffect is stored under the CorrelationIdentifier that identifies the ApiEffect.
type ActiveSupportedEffects = GLS.GLStrMap SupportedEffect

lookupActiveSupportedEffect :: CorrelationIdentifier -> Maybe SupportedEffect
lookupActiveSupportedEffect = show >>> GLS.peek activeSupportedEffects

-- | A global store of SupportedEffect-s
-- | This index cannot be part of the PerspectivesState. The compiler loops on it.
activeSupportedEffects :: ActiveSupportedEffects
activeSupportedEffects = GLS.newMap unit

-- | Unless `onlyOnce` equals true, egister the ApiEffect and the TrackedObjectsGetter with the CorrelationIdentifier and run it once.
-- | Running means: compute the result of the TrackedObjectsGetter, add the computed Assumptions to
-- | the SupportedEffect and push the resulting values into the ApiEffect.
-- | As a result:
-- |  1. we have cached a newMap SupportedEffect in the ActiveSupportedEffects.
-- |  2. we have registered the dependency of this SupportedEffect in the AssumptionRegister in the PerspectivesState.
registerSupportedEffect :: forall a b x. Attachment x => Persistent x a => Newtype b String =>
  CorrelationIdentifier ->
  ApiEffect ->
  (a ~~> b) ->
  a ->
  Boolean ->
  MP Unit
registerSupportedEffect corrId ef q arg onlyOnce = do
  -- Add a newMap effect to activeSupportedEffects, for now with zero assumptions.
  _ <- if onlyOnce 
    then pure unit
    else void $ pure $ GLS.poke activeSupportedEffects (show corrId) {runner: apiEffectRunner, assumptions: []}
  -- then execute the SupportedEffect once
  apiEffectRunner unit
  where
    -- As an effect runner, needs an argument.
    apiEffectRunner :: Unit -> MP Unit
    apiEffectRunner _ = do
      exists <- entityExists arg
      if exists
        then do
          (Tuple result (informedAssumptions :: ArrayWithoutDoubles InformedAssumption)) <- runMonadPerspectivesQuery arg q
          assumptions <- pure (map toAssumption (filter canBeUntypedAssumption (unwrap informedAssumptions)))
          -- destructively set the assumptions in the ActiveSupportedEffects
          (moldSupports :: Maybe SupportedEffect) <- pure $ GLS.peek activeSupportedEffects (show corrId)
          -- The original request may be retracted by the client.
          case moldSupports of
            Nothing -> liftEffect $ ef (Result corrId (map unwrap result))
            Just x -> do
              (oldSupports :: Array Assumption) <- pure x.assumptions
              -- (oldSupports :: Array Assumption) <- pure <<< unsafePartial $ (fromJust moldSupports).assumptions
              _ <- pure $ GLS.poke activeSupportedEffects (show corrId) {runner: apiEffectRunner, assumptions: assumptions}
              -- destructively register the correlationIdentifier with newMap assumptions
              {no: newMap} <- pure $ partition ((maybe false (const true)) <<< flip elemIndex oldSupports) assumptions
              for_ newMap (registerDependency corrId)
              -- destructively deregister the correlationIdentifier from vanished assumptions
              {no: vanished} <- pure $ partition ((maybe false (const true)) <<< flip elemIndex assumptions) oldSupports
              for_ vanished (deregisterDependency corrId)
              -- (Re-)run the effect
              liftEffect $ ef (Result corrId (map unwrap result))
              pure unit
        else do
          -- Remove this effect and
          unregisterSupportedEffect corrId
          -- send an empty result (for the last time).
          liftEffect $ ef (Result corrId [])

unregisterSupportedEffect :: CorrelationIdentifier -> MP Unit
unregisterSupportedEffect corrId = do
  case GLS.peek activeSupportedEffects (show corrId) of
    Nothing -> pure unit
    Just {assumptions} -> do
      void <- pure $ GLS.delete activeSupportedEffects (show corrId)
      for_ assumptions (deregisterDependency corrId)

findDependencies :: Assumption -> MP (Maybe (Array CorrelationIdentifier))
findDependencies a@(Tuple resource tpe) = do
  r <- queryAssumptionRegister
  (wildCardIdentifiers :: Maybe (Array CorrelationIdentifier)) <- case lookup "def:AnyContext" r of
      Nothing -> pure Nothing
      Just (typesForResource :: Object (Array CorrelationIdentifier)) -> pure $ lookup tpe typesForResource

  case lookup resource r of
    -- The resource "def:AnyContext" serves as a wildcard.
    Nothing -> pure wildCardIdentifiers
    Just (typesForResource :: Object (Array CorrelationIdentifier)) -> case lookup tpe typesForResource of
      Nothing -> pure wildCardIdentifiers
      Just resourceIdentifiers -> case wildCardIdentifiers of
        Nothing -> pure $ Just resourceIdentifiers
        Just x -> pure $ Just (x <> resourceIdentifiers)

findResourceDependencies :: String -> MP (Array CorrelationIdentifier)
findResourceDependencies resource = do
  r <- queryAssumptionRegister
  case lookup resource r of
    Nothing -> pure []
    Just typesForResource -> pure $ join $ values typesForResource

findMeRequests :: ContextInstance -> MP (Array CorrelationIdentifier)
findMeRequests resource = findDependencies (Tuple (unwrap resource) "model://perspectives.domains#System$Context$Me") >>= \ma -> pure $ maybe [] identity ma

-- Find all correlation identifiers for requests of the form `role <TypeOfRole>`.
findRoleRequests :: ContextInstance -> EnumeratedRoleType -> MP (Array CorrelationIdentifier)
findRoleRequests resource ert = do
  allTypes <- ert ###= roleAspectsClosure
  concat <$> for allTypes \tpe -> 
    findDependencies (Tuple (unwrap resource) (unwrap tpe)) >>= \ma -> pure $ maybe [] identity ma

-- Find all correlation identifiers for requests of the form `property <TypeOfProperty`
findPropertyRequests :: RoleInstance -> EnumeratedPropertyType -> MP (Array CorrelationIdentifier)
findPropertyRequests resource tpe = findDependencies (Tuple (unwrap resource) (unwrap tpe)) >>= \ma -> pure $ maybe [] identity ma

-- | Returns CorrelationIdentifiers for requests through the API that have
-- | the `binding <roleId>` step.
findBindingRequests :: RoleInstance -> MP (Array CorrelationIdentifier)
findBindingRequests (RoleInstance roleId) = do
  r <- queryAssumptionRegister
  case lookup roleId r of
    Nothing -> pure []
    Just typesForResource -> pure $ maybe [] identity (lookup "model://perspectives.domains#System$Role$binding" typesForResource)

-- | Returns CorrelationIdentifiers for requests through the API that have
-- | the `binder <EnumeratedRoleType> in <ContextType>` step on the `roleId`.
findFilledRoleRequests :: RoleInstance -> ContextType -> EnumeratedRoleType -> MP (Array CorrelationIdentifier)
findFilledRoleRequests (RoleInstance fillerId) (ContextType filledContextType) (EnumeratedRoleType filledType) = do
  r <- queryAssumptionRegister
  case lookup fillerId r of
    Nothing -> pure []
    Just typesForResource -> pure $ maybe [] identity (lookup (filledContextType <> filledType) typesForResource)

findContextStateRequests :: ContextInstance -> MP (Array CorrelationIdentifier)
findContextStateRequests (ContextInstance contextId) = do
  r <- queryAssumptionRegister
  case lookup contextId r of
    Nothing -> pure []
    Just typesForResource -> pure $ maybe [] identity (lookup "model://perspectives.domains#System$Context$State" typesForResource)

findRoleStateRequests :: RoleInstance -> MP (Array CorrelationIdentifier)
findRoleStateRequests (RoleInstance roleId) = do
  r <- queryAssumptionRegister
  case lookup roleId r of
    Nothing -> pure []
    Just typesForResource -> pure $ maybe [] identity (lookup "model://perspectives.domains#System$Role$State" typesForResource)

findIndexedContextNamesRequests :: ContextInstance -> MP (Array CorrelationIdentifier)
findIndexedContextNamesRequests (ContextInstance cid) = do
  r <- queryAssumptionRegister
  case lookup cid r of
    Nothing -> pure []
    Just typesForResource -> pure $ maybe [] identity (lookup indexedContextFuzzies typesForResource)

isRegistered :: CorrelationIdentifier -> Assumption -> MP Boolean
isRegistered corrId assumption = findDependencies assumption >>= pure <<<
  (maybe false (maybe false (const true) <<< (elemIndex corrId)) )

registerDependency :: CorrelationIdentifier -> Assumption -> MP Unit
registerDependency corrId (Tuple resource tpe) = queryAssumptionRegisterModify \r ->
  case lookup resource r of
    Nothing -> insert resource (singleton tpe [corrId]) r
    Just (typesForResource :: Object (Array CorrelationIdentifier)) -> case lookup tpe typesForResource of
      Nothing -> insert resource (insert tpe [corrId] typesForResource) r
      Just (correlationIdentifiers :: Array CorrelationIdentifier) -> insert resource (insert tpe (corrId : correlationIdentifiers) typesForResource) r

deregisterDependency :: CorrelationIdentifier -> Assumption -> MP Unit
deregisterDependency corrId (Tuple resource tpe) = queryAssumptionRegisterModify \r ->
  case lookup resource r of
    Nothing -> r
    Just (typesForResource :: Object (Array CorrelationIdentifier)) -> case lookup tpe typesForResource of
      Nothing -> r
      Just correlationIdentifiers -> insert resource (insert tpe (delete corrId correlationIdentifiers) typesForResource) r

toAssumption :: InformedAssumption -> Assumption
toAssumption (RoleAssumption ci rt) = assumption (unwrap ci) (unwrap rt)
toAssumption (Me ci) = assumption (unwrap ci) "model://perspectives.domains#System$Context$Me"
toAssumption (Filler ri) = assumption (unwrap ri) "model://perspectives.domains#System$Role$binding"
-- NOTE that we create a single key out of the ContextType and RoleType by concatenating their String values.
toAssumption (FilledRolesAssumption ri ct rt) = assumption (unwrap ri) ((unwrap ct) <> (unwrap rt))
toAssumption (Property ri pt) = assumption (unwrap ri) (unwrap pt)
toAssumption (Context ri) = assumption (unwrap ri) "model://perspectives.domains#System$Role$context"
toAssumption (External ci) = assumption (unwrap ci) "model://perspectives.domains#System$Context$external"
toAssumption (State ci) = assumption (unwrap ci) "model://perspectives.domains#System$Context$State"
toAssumption (RoleState ri) = assumption (unwrap ri) "model://perspectives.domains#System$Role$State"

canBeUntypedAssumption :: InformedAssumption -> Boolean
canBeUntypedAssumption (RoleAssumption _ _) = true
canBeUntypedAssumption (Me c) = true
canBeUntypedAssumption (Filler _) = true
canBeUntypedAssumption (FilledRolesAssumption _ _ _) = true
canBeUntypedAssumption (Property _ _) = true
canBeUntypedAssumption (Context _) = false
canBeUntypedAssumption (External _) = false
-- TODO. Ik ben hier niet zeker van.
canBeUntypedAssumption (State _) = true
canBeUntypedAssumption (RoleState _) = true
