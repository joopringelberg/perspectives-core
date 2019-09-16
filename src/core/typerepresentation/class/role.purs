module Perspectives.Representation.Class.Role where

import Control.Monad.Error.Class (throwError)
import Control.Plus (empty, (<|>))
import Data.Array (elemIndex, filter)
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
import Perspectives.Representation.Class.PersistentType (class PersistentType, ContextType, getPerspectType)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), EnumeratedRoleType(..), PropertyType, RoleKind, RoleType(..))
import Prelude (class Show, bind, map, notEq, pure, ($), (<<<), (>=>), (>>=))

-----------------------------------------------------------
-- ROLE TYPE CLASS
-----------------------------------------------------------
class (Show r, Identifiable r i, PersistentType r i) <= RoleClass r i | r -> i, i -> r where
  kindOfRole :: r -> RoleKind
  roleAspects :: r -> MonadPerspectives (Array EnumeratedRoleType)
  context :: r -> ContextType
  binding :: r -> MonadPerspectives (ADT EnumeratedRoleType)
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
  binding = rangeOfCalculatedRole >=> bindingOfADT
  functional = rangeOfCalculatedRole >=> functional'
  mandatory = rangeOfCalculatedRole >=> mandatory'
  calculation r = (unwrap r).calculation
  properties = rangeOfCalculatedRole >=> properties'
  fullType = rangeOfCalculatedRole >=> fullADTType

fullADTType :: ADT EnumeratedRoleType -> MP (ADT EnumeratedRoleType)
fullADTType (ST et) = ((getPerspectType et) :: MP EnumeratedRole) >>= fullType
fullADTType (SUM adts) = map SUM (traverse fullADTType adts)
fullADTType (PROD adts) = map PROD (traverse fullADTType adts)
fullADTType NOTYPE = pure NOTYPE

roleCalculationRange :: QueryFunctionDescription -> MonadPerspectives (ADT EnumeratedRoleType)
roleCalculationRange qfd = case QT.range qfd of
  (RDOM p) -> pure p
  otherwise -> throwError (error ("range of calculation of a calculated role is not an enumerated role."))

rangeOfCalculatedRole :: CalculatedRole -> MonadPerspectives (ADT EnumeratedRoleType)
rangeOfCalculatedRole cr = roleCalculationRange (calculation cr)

-- what is the binding of a calculated role? It is the binding of the range of its calculation (an ADT).
-- what is the binding of an ADT?
bindingOfADT :: ADT EnumeratedRoleType -> MP (ADT EnumeratedRoleType)
bindingOfADT (ST et) = ((getPerspectType et) :: MP EnumeratedRole) >>= binding
bindingOfADT (SUM adts) = do
  (x :: Array (ADT EnumeratedRoleType)) <- traverse bindingOfADT adts
  -- Simplify: all members of the SUM must have a binding, otherwise the binding of the SUM is NOTYPE.
  case elemIndex NOTYPE x of
    Nothing -> pure NOTYPE
    otherwise -> pure $ SUM x
bindingOfADT (PROD adts) = do
  (x :: Array (ADT EnumeratedRoleType)) <- traverse bindingOfADT adts
  -- Simplify: remove all NOTYPE's.
  pure $ PROD $ filter (notEq NOTYPE) x
bindingOfADT NOTYPE = pure NOTYPE

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
  fullType r = pure $ PROD [(ST $ identifier r), (unwrap r).binding]

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
