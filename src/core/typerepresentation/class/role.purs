module Perspectives.Representation.Class.Role where

import Control.Monad.Error.Class (throwError)
import Control.Plus (empty, (<|>))
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives, MP)
import Perspectives.Query.QueryTypes (QueryFunctionDescription(..), Domain(..))
import Perspectives.Query.QueryTypes (range) as QT
import Perspectives.Representation.ADT (ADT(..), reduce)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.Identifiable (class Identifiable, identifier)
import Perspectives.Representation.Class.PersistentType (class PersistentType, ContextType, getPerspectType, getPerspectTypes)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), EnumeratedRoleType(..), PropertyType, RoleKind, RoleType(..))
import Prelude (class Show, bind, identity, map, pure, ($), (<<<), (>=>), (>>=))

-----------------------------------------------------------
-- ROLE TYPE CLASS
-----------------------------------------------------------
class (Show r, Identifiable r i, PersistentType r i) <= RoleClass r i | r -> i, i -> r where
  kindOfRole :: r -> RoleKind
  roleAspects :: r -> MonadPerspectives (Array EnumeratedRoleType)
  context :: r -> ContextType
  binding :: r -> MonadPerspectives (Maybe (ADT EnumeratedRoleType))
  functional :: r -> MonadPerspectives Boolean
  mandatory :: r -> MonadPerspectives Boolean
  calculation :: r -> QueryFunctionDescription
  properties :: r -> MonadPerspectives (Array PropertyType)
  -- | The type of the Role, including its binding.
  fullType :: r -> MonadPerspectives (ADT EnumeratedRoleType)

instance calculatedRoleRoleClass :: RoleClass CalculatedRole CalculatedRoleType where
  kindOfRole r = (unwrap r).kindOfRole
  roleAspects = rangeOfCalculatedRole >=> roleAspects'
  context r = (unwrap r).context
  binding = rangeOfCalculatedRole >=> binding'
  functional = rangeOfCalculatedRole >=> functional'
  mandatory = rangeOfCalculatedRole >=> mandatory'
  calculation r = (unwrap r).calculation
  properties = rangeOfCalculatedRole >=> properties'
  fullType = rangeOfCalculatedRole >=> fullADTType

fullADTType :: ADT EnumeratedRoleType -> MP (ADT EnumeratedRoleType)
fullADTType (ST et) = ((getPerspectType et) :: MP EnumeratedRole) >>= fullType
fullADTType (SUM adts) = map SUM (traverse fullADTType adts)
fullADTType (PROD adts) = map PROD (traverse fullADTType adts)

roleCalculationRange :: QueryFunctionDescription -> MonadPerspectives (ADT EnumeratedRoleType)
roleCalculationRange qfd = case QT.range qfd of
  (RDOM p) -> pure p
  otherwise -> throwError (error ("range of calculation of a calculated role is not an enumerated role."))

rangeOfCalculatedRole :: CalculatedRole -> MonadPerspectives (ADT EnumeratedRoleType)
rangeOfCalculatedRole cr = roleCalculationRange (calculation cr)

rangeOfCalculation_ :: CalculatedRole -> MonadPerspectives (Array EnumeratedRole)
rangeOfCalculation_ = rangeOfCalculatedRole >=> getPerspectTypes

binding' :: ADT EnumeratedRoleType -> MP (Maybe (ADT EnumeratedRoleType))
binding' s@(ST et) = ((getPerspectType et) :: MP EnumeratedRole) >>= binding
binding' s@(SUM _) = getPerspectTypes s >>= g
  where
    g :: Array EnumeratedRole -> MP (Maybe (ADT EnumeratedRoleType))
    g rs = do
      (x :: (Array (Maybe (ADT EnumeratedRoleType)))) <- traverse binding rs
      (y :: (Maybe (Array (ADT EnumeratedRoleType)))) <- pure $ traverse identity x
      pure (map SUM y)
binding' p@(PROD _) = getPerspectTypes p >>= g
  where
    g :: Array EnumeratedRole -> MP (Maybe (ADT EnumeratedRoleType))
    g rs = do
      (x :: (Array (Maybe (ADT EnumeratedRoleType)))) <- traverse binding rs
      pure $ Just $ PROD $ catMaybes x

functional' :: ADT EnumeratedRoleType -> MP Boolean
functional' = reduce g
  where
    g :: EnumeratedRoleType -> MP Boolean
    g = (getPerspectType :: EnumeratedRoleType -> MP EnumeratedRole) >=> functional

mandatory' :: ADT EnumeratedRoleType -> MP Boolean
mandatory' = reduce g
  where
    g :: EnumeratedRoleType -> MP Boolean
    g = (getPerspectType :: EnumeratedRoleType -> MP EnumeratedRole) >=> mandatory

properties' :: ADT EnumeratedRoleType -> MP (Array PropertyType)
properties' = reduce g
  where
    g :: EnumeratedRoleType -> MP (Array PropertyType)
    g = (getPerspectType :: EnumeratedRoleType -> MP EnumeratedRole) >=> properties

roleAspects' :: ADT EnumeratedRoleType -> MP (Array EnumeratedRoleType)
roleAspects' = reduce g
  where
    g :: EnumeratedRoleType -> MP (Array EnumeratedRoleType)
    g = (getPerspectType :: EnumeratedRoleType -> MP EnumeratedRole) >=> roleAspects

instance enumeratedRoleRoleClass :: RoleClass EnumeratedRole EnumeratedRoleType where
  kindOfRole r = (unwrap r).kindOfRole
  roleAspects r = pure (unwrap r).roleAspects
  context r = (unwrap r).context
  binding r = pure (unwrap r).binding
  functional r = pure (unwrap r).functional
  mandatory r = pure (unwrap r).mandatory
  calculation r = SQD (CDOM (context r)) (RolGetter (ENR (identifier r))) (RDOM (ST (identifier r)))
  properties r = pure (unwrap r).properties
  fullType r = case (unwrap r).binding of
    Nothing -> pure $ ST $ identifier r
    (Just b) -> do
      bt <- fullADTType b
      pure $ SUM [ST (identifier r), b]

data Role = E EnumeratedRole | C CalculatedRole

id :: forall r i. RoleClass r i => r -> i
id = identifier

getRole :: RoleType -> MonadPerspectives Role
getRole (ENR e) = getPerspectType e >>= pure <<< E
getRole (CR c) = getPerspectType c >>= pure <<< C

getCalculation :: Role -> QueryFunctionDescription
getCalculation (E r) = calculation r
getCalculation (C r) = calculation r

effectiveRoleType_ :: String -> MonadPerspectives (ADT EnumeratedRoleType)
effectiveRoleType_ r = effectiveRoleType (ENR $ EnumeratedRoleType r) <|> effectiveRoleType (CR $ CalculatedRoleType r)

effectiveRoleType :: RoleType -> MonadPerspectives (ADT EnumeratedRoleType)
effectiveRoleType = getRole >=> pure <<< getCalculation >=> case _ of
    SQD _ _ (RDOM p) -> pure p
    UQD _ _ _ (RDOM p) -> pure p
    BQD _ _ _ _ (RDOM p) -> pure p
    otherwise -> empty -- NB: The Alt instance of Aff throws an error on empty!

-- Here are alternative functions, using functional dependencies in RoleClass,
-- omitting Role.
getRole' :: forall r i. RoleClass r i => i -> MonadPerspectives r
getRole' i = getPerspectType i

getCalculation' :: forall r i. RoleClass r i => r -> QueryFunctionDescription
getCalculation' r = calculation r

effectiveRoleType' :: String -> MonadPerspectives (ADT EnumeratedRoleType)
effectiveRoleType' r = effectiveRoleType'_ (EnumeratedRoleType r) <|> effectiveRoleType'_ (CalculatedRoleType r)
  where
    effectiveRoleType'_ :: forall r i. RoleClass r i => i -> MonadPerspectives (ADT EnumeratedRoleType)
    effectiveRoleType'_ i = getRole' i >>= pure <<< getCalculation' >>= case _ of
        SQD _ _ (RDOM p) -> pure p
        UQD _ _ _ (RDOM p) -> pure p
        BQD _ _ _ _ (RDOM p) -> pure p
        otherwise -> empty
