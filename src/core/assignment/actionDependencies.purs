module Perspectives.Assignment.DependencyTracking where

-- | The ActionAssumptionRegister is indexed by the two elements of an Assumption, in order.
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Perspectives.CoreTypes (Assumption)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, peek, poke, new)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..))
import Perspectives.Representation.TypeIdentifiers (ActionType(..))
import Prelude (Unit, pure, unit)

data ActionInstance = ActionInstance ContextInstance ActionType

-- | The ActionAssumptionCache is indexed by the two elements of an Assumption, in order.
type ActionAssumptionCache =  (GLStrMap (GLStrMap (Array ActionInstance)))

-- | The ActionInstanceCache is indexed by the two elements of the ActionInstance, unwrapped
type ActionInstanceCache = GLStrMap (GLStrMap (Array Assumption))

-- TODO: write these functions!
-- | Creates reciprocal entries in the ActionAssumptionCache and the ActionInstanceCache.
-- | When the same ActionInstance is cached again but with different Assumptions,
-- | both caches are updated correspondingly.
cacheActionInstanceDependencies :: ActionInstance -> Array Assumption -> Effect Unit
cacheActionInstanceDependencies (ActionInstance c a) as = pure unit

-- | Removes the reciprocal entries from the ActionAssumptionCache and the ActionInstanceCache.
removeActionInstanceDependencies :: ActionInstance -> Effect Unit
removeActionInstanceDependencies (ActionInstance c a) = pure unit

-- | Given just a ContextInstance, find all Assumptions that reference an ActionInstance with that
-- | ContextInstance and remove those ActionInstances.
removeContextInstanceDependencies :: ContextInstance -> Effect Unit
removeContextInstanceDependencies (ContextInstance c) = pure unit

retrieveActionInstanceSupports :: ActionInstance -> Effect (Array Assumption)
retrieveActionInstanceSupports (ActionInstance c a) = pure []

retrieveAssumptionActionInstances :: Assumption -> Effect (Array ActionInstance)
retrieveAssumptionActionInstances (Tuple res pred) = pure []

actionInstancesDependingOn :: Array Assumption -> Effect (Array ActionInstance)
actionInstancesDependingOn as = pure []
