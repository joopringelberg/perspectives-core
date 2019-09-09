module Perspectives.Assignment.DependencyTracking where

-- | The ActionAssumptionRegister is indexed by the two elements of an Assumption, in order.
import Data.Array (cons, delete) as Arr
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Perspectives.CoreTypes (Assumption)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, delete, ensure, keys, modify, new, peek, poke)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..))
import Perspectives.Representation.TypeIdentifiers (ActionType(..))
import Prelude (class Eq, Unit, const, flip, join, unit, ($), (<#>), (<$>), (&&), eq, map)

-- | An ActionInstance represents the applicability of an Action to the instance of a Context.
data ActionInstance = ActionInstance ContextInstance ActionType

instance eqActionInstance :: Eq ActionInstance where
  eq (ActionInstance c1 a1) (ActionInstance c2 a2) = eq c1 c2 && eq a1 a2

-- | Actions should be re-run as the Assumptions underlying their computation change.
-- | We register the dependency of Actions for ContextInstances with a double registration that allows us to
-- | travel from Assumptions to ActionInstances and vice versa by simple lookup.

-- | The ActionAssumptionCache is indexed by the two elements of an Assumption, in order.
-- | It allows us to cache ActionInstances, signifying those that should be re-applied when an Assumption is touched.
type ActionAssumptionCache = GLStrMap (GLStrMap (Array ActionInstance))

-- | The ActionInstanceCache is indexed by the two elements of the ActionInstance, unwrapped. So given an
-- | ActionInstance, we can look up the Assumptions used in its computations.
type ActionInstanceCache = GLStrMap (GLStrMap (Array Assumption))

actionAssumptionCache :: ActionAssumptionCache
actionAssumptionCache = new unit

actionInstanceCache :: ActionInstanceCache
actionInstanceCache = new unit

-- | Creates reciprocal entries in the ActionAssumptionCache and the ActionInstanceCache.
-- | When the same ActionInstance is cached again but with different Assumptions,
-- | both caches are updated correspondingly.
cacheActionInstanceDependencies :: ActionInstance -> Array Assumption -> Unit
cacheActionInstanceDependencies act@(ActionInstance c a) as = unit where
  ignore = cacheActionInstanceSupports act as
  ignore2 = (cacheActionInstanceDependency act) <$> as

  cacheActionInstanceSupports :: ActionInstance -> Array Assumption -> GLStrMap (Array Assumption)
  cacheActionInstanceSupports (ActionInstance c' a') as' = poke (ensure actionInstanceCache (unwrap c') (new unit)) (unwrap a') as'

  cacheActionInstanceDependency :: ActionInstance -> Assumption -> GLStrMap (Array ActionInstance)
  cacheActionInstanceDependency ai (Tuple r p) = modify (ensure actionAssumptionCache r (new unit)) p (Arr.cons ai) []

-- | Retrieve the assumptions that underly the application of an Action to a ContextInstance.
retrieveActionInstanceSupports :: ActionInstance -> Maybe (Array Assumption)
retrieveActionInstanceSupports (ActionInstance c a) = join $ flip peek (unwrap a) <$> (peek actionInstanceCache (unwrap c))

-- | Retrieve the ActionInstances that depend an an Assumption.
retrieveAssumptionActionInstances :: Assumption -> Maybe (Array ActionInstance)
retrieveAssumptionActionInstances (Tuple r p) = join $ flip peek p <$> (peek actionAssumptionCache r)

-- | Given just a ContextInstance, find all Assumptions that reference an ActionInstance with that
-- | ContextInstance and remove those ActionInstances.
removeContextInstanceDependencies :: ContextInstance -> Unit
removeContextInstanceDependencies (ContextInstance c) = case delete actionInstanceCache c of
  Nothing -> unit
  (Just (asm :: GLStrMap (Array Assumption))) -> const unit $ keys asm <#> \at -> let
    actionInstance = ActionInstance (ContextInstance c) (ActionType at) in
    -- remove this ActionInstance from each of the assumptions.
    case peek asm at of
      Nothing -> unit
      (Just (as :: Array Assumption)) -> const unit $ as <#> \((Tuple r p) :: Assumption) -> case peek actionAssumptionCache r of
        Nothing -> unit
        (Just x) -> const unit (modify x p (Arr.delete actionInstance) [])

-- | Given an array of assumptions, find all ActionInstances that depend on at least one of them.
actionInstancesDependingOn :: Array Assumption -> Maybe (Array ActionInstance)
actionInstancesDependingOn as = join <$> traverse retrieveAssumptionActionInstances as

actionTypesForContextInstance :: ContextInstance -> Maybe (Array ActionType)
actionTypesForContextInstance c = (map ActionType) <$> (keys <$> peek actionInstanceCache (unwrap c))
