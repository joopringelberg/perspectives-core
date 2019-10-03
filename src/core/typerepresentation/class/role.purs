module Perspectives.Representation.Class.Role where

import Control.Monad.Error.Class (throwError)
import Control.Plus (empty, (<|>))
import Data.Array (union)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives, MP)
import Perspectives.Query.QueryTypes (QueryFunctionDescription(..), Domain(..))
import Perspectives.Query.QueryTypes (range) as QT
import Perspectives.Representation.ADT (ADT(..), reduce)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.Identifiable (class Identifiable, identifier)
import Perspectives.Representation.Class.PersistentType (class PersistentType, ContextType, getPerspectType, getEnumeratedRole)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), EnumeratedRoleType(..), PropertyType, RoleKind, RoleType(..), ViewType)
import Prelude (class Show, map, pure, ($), (<<<), (>=>), (>>=), bind, class Eq)

-----------------------------------------------------------
-- ROLE TYPE CLASS
-----------------------------------------------------------
class (Show r, Identifiable r i, PersistentType r i) <= RoleClass r i | r -> i, i -> r where
  kindOfRole :: r -> RoleKind
  roleAspects :: r -> MonadPerspectives (Array EnumeratedRoleType)
  context :: r -> MP (ADT ContextType)
  contextOfRepresentation :: r -> ContextType
  binding :: r -> MonadPerspectives (ADT EnumeratedRoleType)
  functional :: r -> MonadPerspectives Boolean
  mandatory :: r -> MonadPerspectives Boolean
  calculation :: r -> QueryFunctionDescription
  properties :: r -> MonadPerspectives (Array PropertyType)
  -- | The type of the Role, including its binding.
  fullType :: r -> MonadPerspectives (ADT EnumeratedRoleType)
  views :: r -> MonadPerspectives (Array ViewType)

instance calculatedRoleRoleClass :: RoleClass CalculatedRole CalculatedRoleType where
  kindOfRole r = (unwrap r).kindOfRole
  roleAspects = rangeOfCalculatedRole >=> roleAspectsOfADT
  context = rangeOfCalculatedRole >=> contextOfADT
  contextOfRepresentation r = (unwrap r).context
  binding = rangeOfCalculatedRole >=> bindingOfADT
  functional = rangeOfCalculatedRole >=> functional'
  mandatory = rangeOfCalculatedRole >=> mandatory'
  calculation r = (unwrap r).calculation
  properties = rangeOfCalculatedRole >=> propertiesOfADT
  fullType = rangeOfCalculatedRole >=> fullADTType
  views =  rangeOfCalculatedRole >=> viewsOfADT

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

-- | The context of an ADT
contextOfADT :: ADT EnumeratedRoleType -> MP (ADT ContextType)
contextOfADT (ST et) = (getEnumeratedRole >=> context) et
contextOfADT (SUM adts) = map SUM (traverse contextOfADT adts)
contextOfADT (PROD adts) = map PROD (traverse contextOfADT adts)
contextOfADT NOTYPE = pure NOTYPE

-- | The binding of an ADT.
bindingOfADT :: ADT EnumeratedRoleType -> MP (ADT EnumeratedRoleType)
bindingOfADT = reduce (getEnumeratedRole >=> binding)

-- | A functional role can be filled with a relational role (but then we need to select). E.g.:
-- | one of the party-goers was the driver.
-- | Vice versa can we fill a relational role with a functional role:
-- | In the canteen, a bus driver can order a coffee.
-- | Hence, we need not take the binding of a role into account in order to determine whether it is
-- | functional.
functional' :: ADT EnumeratedRoleType -> MP Boolean
functional' = reduce g
  where
    g :: EnumeratedRoleType -> MP Boolean
    g = getEnumeratedRole >=> functional

-- | As with functional, we need not take the binding of a role into account in order to determine whether it is
-- | mandatory.
mandatory' :: ADT EnumeratedRoleType -> MP Boolean
mandatory' = reduce g
  where
    g :: EnumeratedRoleType -> MP Boolean
    g = getEnumeratedRole >=> mandatory

-- | Properties of a role's binding count as properties of the role itself.
propertiesOfADT :: ADT EnumeratedRoleType -> MP (Array PropertyType)
propertiesOfADT = reduce g
  where
    g :: EnumeratedRoleType -> MP (Array PropertyType)
    g = getEnumeratedRole >=> properties

-- Views of the binding of a role count as views of the role itself.
viewsOfADT :: ADT EnumeratedRoleType -> MP (Array ViewType)
viewsOfADT = reduce g
  where
    g :: EnumeratedRoleType -> MP (Array ViewType)
    g = getEnumeratedRole >=> views

-- | Aspects of the binding of a role count as aspects of the role itself.
roleAspectsOfADT :: ADT EnumeratedRoleType -> MP (Array EnumeratedRoleType)
roleAspectsOfADT = reduce g
  where
    g :: EnumeratedRoleType -> MP (Array EnumeratedRoleType)
    g = getEnumeratedRole >=> roleAspects

instance enumeratedRoleRoleClass :: RoleClass EnumeratedRole EnumeratedRoleType where
  kindOfRole r = (unwrap r).kindOfRole
  roleAspects r = includeBinding (\r' -> (unwrap r').roleAspects) roleAspectsOfADT r
  context r = pure $ ST $ contextOfRepresentation r
  contextOfRepresentation r = (unwrap r).context
  binding r = pure (unwrap r).binding
  functional r = pure (unwrap r).functional
  mandatory r = pure (unwrap r).mandatory
  calculation r = SQD (CDOM $ ST $ contextOfRepresentation r) (RolGetter (ENR (identifier r))) (RDOM (ST (identifier r)))
  properties r = includeBinding (\r' -> (unwrap r').properties) propertiesOfADT r
  fullType r = do
    bindingType <- fullADTType (unwrap r).binding
    pure $ PROD [(ST $ identifier r), bindingType]
  views r = includeBinding (\r' -> (unwrap r).views) viewsOfADT r

-- | A pattern of computation shared in the recursive computation of roleAspects, properties and views of a role.
-- | It computes the local value for an EnumeratedRole and then the value of the binding of the role, returning
-- | the union of the two.
includeBinding :: forall r i a. RoleClass r i => Eq a =>
  (r -> Array a) ->
  (ADT EnumeratedRoleType -> MP (Array a)) ->
  r -> MP (Array a)
includeBinding own adtF r = do
  ownAs <- pure (own r)
  binding' <- binding r
  bindingAs <- adtF binding'
  pure (union ownAs bindingAs)

data Role = E EnumeratedRole | C CalculatedRole

id :: forall r i. RoleClass r i => r -> i
id = identifier

stringId :: RoleType -> String
stringId (ENR (EnumeratedRoleType ident)) = ident
stringId (CR (CalculatedRoleType ident)) = ident

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

-- A function from i to r is possible, but not from String to r, using Alt.
-- However, a function from String to x, where x is computed as r -> x, is possible.
-- test :: forall i r. RoleClass r i => i -> MP QueryFunctionDescription
-- test = getRole' >=> pure <<< getCalculation'
--
-- test2 :: String -> MP QueryFunctionDescription
-- test2 s = (getRole' (EnumeratedRoleType s) >>= pure <<< getCalculation') <|>
--   (getRole' (CalculatedRoleType s) >>= pure <<< getCalculation')
--
-- test3 :: String -> MP QueryFunctionDescription
-- test3 s = test (EnumeratedRoleType s) <|> test (CalculatedRoleType s)

effectiveRoleType' :: String -> MonadPerspectives (ADT EnumeratedRoleType)
effectiveRoleType' r = effectiveRoleType'_ (EnumeratedRoleType r) <|> effectiveRoleType'_ (CalculatedRoleType r)
  where
    effectiveRoleType'_ :: forall r i. RoleClass r i => i -> MonadPerspectives (ADT EnumeratedRoleType)
    effectiveRoleType'_ i = getRole' i >>= pure <<< getCalculation' >>= case _ of
        SQD _ _ (RDOM p) -> pure p
        UQD _ _ _ (RDOM p) -> pure p
        BQD _ _ _ _ (RDOM p) -> pure p
        otherwise -> empty
