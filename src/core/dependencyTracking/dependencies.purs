module Perspectives.DependencyTracking.Dependency where

-- | A Dependency is a combination of a resource (ContextInstance or RoleInstance) and a type
-- | (EnumeratedRoleType, CalculatedRoleType, EnumeratedPropertyType or CalculatedPropertyType).
-- | However, in the dependency administration we omit these newtypes.
import Prelude

import Data.Array (cons, delete, elemIndex, partition)
import Data.Foldable (for_)
import Data.Lens (_Just, firstOf, over, traversed)
import Data.Lens.At (at)
import Data.Lens.Traversal (Traversal')
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (ApiEffect, CorrelationIdentifier, ResponseRecord(..))
import Perspectives.CoreTypes (Assumption, AssumptionRegister, MP, MPQ)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, peek, poke, new)
import Perspectives.PerspectivesState (assumptionRegister, assumptionRegisterModify)
import Perspectives.RunMonadPerspectivesQuery (runMonadPerspectivesQuery)
import Unsafe.Coerce (unsafeCoerce)

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
type ActiveSupportedEffects = GLStrMap SupportedEffect

-- | A global store of SupportedEffect-s
-- | This index cannot be part of the PerspectivesState. The compiler loops on it.
activeSupportedEffects :: ActiveSupportedEffects
activeSupportedEffects = new unit

registerSupportedEffect :: forall a b.
  CorrelationIdentifier ->
  ApiEffect ->
  (a -> MPQ (Array b)) ->
  a ->
  MP Unit
registerSupportedEffect corrId ef q arg = do
  -- Add a new supported effect, for now with zero assumptions.
  liftEffect $ void $ poke activeSupportedEffects (show corrId) {runner: supportedEffect, assumptions: []}
  -- then execute the SupportedEffect once
  supportedEffect unit
  pure unit
  where
    supportedEffect :: Unit -> MP Unit
    supportedEffect _ = do
      (Tuple result (assumptions :: Array Assumption)) <- runMonadPerspectivesQuery arg q
      -- destructively set the assumptions in the ActiveSupportedEffects
      (moldSupports :: Maybe SupportedEffect) <- liftEffect $ peek activeSupportedEffects (show corrId)
      -- We have ensured a registration above, hence we can use unsafePartial.
      (oldSupports :: Array Assumption) <- pure <<< unsafePartial $ (fromJust moldSupports).assumptions
      liftEffect $ void $ poke activeSupportedEffects (show corrId) {runner: supportedEffect, assumptions: assumptions}
      -- destructively register the correlationIdentifier with new assumptions
      {no: new} <- pure $ partition ((maybe false (const true)) <<< flip elemIndex oldSupports) assumptions
      for_ new (registerDependency corrId)
      -- destructively deregister the correlationIdentifier from vanished assumptions
      {no: vanished} <- pure $ partition ((maybe false (const true)) <<< flip elemIndex assumptions) oldSupports
      for_ vanished (deregisterDependency corrId)
      -- Re-run the effect
      liftEffect $ ef (ResponseRecord {corrId: corrId, result: Just (unsafeCoerce result), error: Nothing})
      pure unit

_assumptionDependencies :: Assumption -> Traversal' AssumptionRegister (Array CorrelationIdentifier)
_assumptionDependencies (Tuple rid pid) = at rid <<< traversed <<< at pid <<< _Just

findDependencies :: Assumption -> MP (Maybe (Array CorrelationIdentifier))
findDependencies a = assumptionRegister >>= pure <<< firstOf (_assumptionDependencies a)

isRegistered :: CorrelationIdentifier -> Assumption -> MP Boolean
isRegistered corrId assumption = findDependencies assumption >>= pure <<<
  (maybe false (maybe false (const true) <<< (elemIndex corrId)) )

registerDependency :: CorrelationIdentifier -> Assumption -> MP Unit
registerDependency corrId a = assumptionRegisterModify (over (_assumptionDependencies a) (cons corrId))

deregisterDependency :: CorrelationIdentifier -> Assumption -> MP Unit
deregisterDependency corrId a = assumptionRegisterModify (over (_assumptionDependencies a) (delete corrId))
