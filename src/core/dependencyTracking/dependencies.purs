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

module Perspectives.DependencyTracking.Dependency where

-- | A Dependency is a combination of a resource (ContextInstance or RoleInstance) and a type
-- | (EnumeratedRoleType, CalculatedRoleType, EnumeratedPropertyType or CalculatedPropertyType).
-- | However, in the dependency administration we omit these newtypes.
import Prelude

import Data.Array (delete, elemIndex, partition, (:))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Foreign.Object (Object, insert, lookup, singleton, values)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (ApiEffect, CorrelationIdentifier, Response(..))
import Perspectives.CoreTypes (Assumption, MP, type (~~>), runMonadPerspectivesQuery)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, new, peek, poke, delete) as GLS
import Perspectives.PerspectivesState (queryAssumptionRegister, queryAssumptionRegisterModify)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, EnumeratedRoleType(..))

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
activeSupportedEffects = GLS.new unit

-- | Register the ApiEffect and the TrackedObjectsGetter with the CorrelationIdentifier and run it once.
-- | Running means: compute the result of the TrackedObjectsGetter, add the computed Assumptions to
-- | the SupportedEffect and push the resulting values into the ApiEffect.
-- | As a result:
-- |  1. we have cached a new SupportedEffect in the ActiveSupportedEffects.
-- |  2. we have registered the dependency of this SupportedEffect in the AssumptionRegister in the PerspectivesState.
registerSupportedEffect :: forall a b. Newtype b String =>
  CorrelationIdentifier ->
  ApiEffect ->
  (a ~~> b) ->
  a ->
  MP Unit
registerSupportedEffect corrId ef q arg = do
  -- Add a new effect to activeSupportedEffects, for now with zero assumptions.
  _ <- pure $ GLS.poke activeSupportedEffects (show corrId) {runner: apiEffectRunner, assumptions: []}
  -- then execute the SupportedEffect once
  apiEffectRunner unit
  pure unit
  where
    apiEffectRunner :: Unit -> MP Unit
    apiEffectRunner _ = do
      (Tuple result (assumptions :: Array Assumption)) <- runMonadPerspectivesQuery arg q
      -- destructively set the assumptions in the ActiveSupportedEffects
      (moldSupports :: Maybe SupportedEffect) <- pure $ GLS.peek activeSupportedEffects (show corrId)
      -- We have ensured a registration above, hence we can use unsafePartial.
      (oldSupports :: Array Assumption) <- pure <<< unsafePartial $ (fromJust moldSupports).assumptions
      _ <- pure $ GLS.poke activeSupportedEffects (show corrId) {runner: apiEffectRunner, assumptions: assumptions}
      -- destructively register the correlationIdentifier with new assumptions
      {no: new} <- pure $ partition ((maybe false (const true)) <<< flip elemIndex oldSupports) assumptions
      for_ new (registerDependency corrId)
      -- destructively deregister the correlationIdentifier from vanished assumptions
      {no: vanished} <- pure $ partition ((maybe false (const true)) <<< flip elemIndex assumptions) oldSupports
      for_ vanished (deregisterDependency corrId)
      -- Re-run the effect
      r <- queryAssumptionRegister
      liftEffect $ ef (Result corrId (map unwrap result))
      pure unit

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
  case lookup resource r of
    -- The resource "AnyContext" serves as a wildcard.
    Nothing -> case lookup "AnyContext" r of
      Nothing -> pure Nothing
      Just (typesForResource :: Object (Array CorrelationIdentifier)) -> pure $ lookup tpe typesForResource
    Just (typesForResource :: Object (Array CorrelationIdentifier)) -> pure $ lookup tpe typesForResource

findResourceDependencies :: String -> MP (Array CorrelationIdentifier)
findResourceDependencies resource = do
  r <- queryAssumptionRegister
  case lookup resource r of
    Nothing -> pure []
    Just typesForResource -> pure $ join $ values typesForResource

-- Find all correlation identifiers for requests of the form `role <TypeOfRole>`.
findRoleRequests :: ContextInstance -> EnumeratedRoleType -> MP (Array CorrelationIdentifier)
findRoleRequests resource tpe = findDependencies (Tuple (unwrap resource) (unwrap tpe)) >>= \ma -> pure $ maybe [] identity ma

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
    Just typesForResource -> pure $ maybe [] identity (lookup "model:System$Role$binding" typesForResource)

-- | Returns CorrelationIdentifiers for requests through the API that have
-- | the `binder <typeId>` step on the `roleId`.
findBinderRequests :: RoleInstance -> EnumeratedRoleType -> MP (Array CorrelationIdentifier)
findBinderRequests (RoleInstance roleId) (EnumeratedRoleType typeId) = do
  r <- queryAssumptionRegister
  case lookup roleId r of
    Nothing -> pure []
    Just typesForResource -> pure $ maybe [] identity (lookup typeId typesForResource)

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
