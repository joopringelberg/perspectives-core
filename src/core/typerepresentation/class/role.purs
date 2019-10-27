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

module Perspectives.Representation.Class.Role where

import Control.Monad.Error.Class (throwError)
import Control.Plus (empty, (<|>))
import Data.Array (cons, union)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives, MP)
import Perspectives.Query.QueryTypes (QueryFunctionDescription(..), Domain(..))
import Perspectives.Query.QueryTypes (range) as QT
import Perspectives.Representation.ADT (ADT(..), reduce)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Calculation (Calculation(..))
import Perspectives.Representation.Class.Identifiable (class Identifiable, identifier)
import Perspectives.Representation.Class.PersistentType (class PersistentType, ContextType, getCalculatedRole, getEnumeratedRole, getPerspectType)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), EnumeratedRoleType(..), PropertyType, RoleKind, RoleType(..), ViewType)
import Prelude (class Show, map, pure, ($), (<<<), (>=>), (>>=), bind, class Eq, (<>))

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
  calculation :: r -> MonadPerspectives QueryFunctionDescription
  properties :: r -> MonadPerspectives (Array PropertyType)
  -- | The type of the Role, including its binding. Is expanded just for CalculatedRole.
  typeIncludingBinding :: r -> MonadPerspectives (ADT EnumeratedRoleType)
  -- | The type of the Role, fully expanded for both CalculatedRole and EnumeratedRole.
  expandedADT :: r -> MonadPerspectives (ADT EnumeratedRoleType)
  views :: r -> MonadPerspectives (Array ViewType)

getRole' :: forall r i. RoleClass r i => i -> MonadPerspectives r
getRole' i = getPerspectType i

getCalculation' :: forall r i. RoleClass r i => r -> MonadPerspectives QueryFunctionDescription
getCalculation' r = calculation r

rangeOfRoleCalculation' :: String -> MonadPerspectives (ADT EnumeratedRoleType)
rangeOfRoleCalculation' r = rangeOfRoleCalculation'_ (EnumeratedRoleType r) <|> rangeOfRoleCalculation'_ (CalculatedRoleType r)
  where
    rangeOfRoleCalculation'_ :: forall r i. RoleClass r i => i -> MonadPerspectives (ADT EnumeratedRoleType)
    rangeOfRoleCalculation'_ i = getRole' i >>= getCalculation' >>= case _ of
        SQD _ _ (RDOM p) -> pure p
        UQD _ _ _ (RDOM p) -> pure p
        BQD _ _ _ _ (RDOM p) -> pure p
        otherwise -> empty

-----------------------------------------------------------
-- CALCULATED ROLE INSTANCE
-----------------------------------------------------------
instance calculatedRoleRoleClass :: RoleClass CalculatedRole CalculatedRoleType where
  kindOfRole r = (unwrap r).kindOfRole
  roleAspects = rangeOfCalculatedRole >=> roleAspectsOfADT
  context = rangeOfCalculatedRole >=> contextOfADT
  contextOfRepresentation r = (unwrap r).context
  binding = rangeOfCalculatedRole >=> bindingOfADT
  functional = rangeOfCalculatedRole >=> functional'
  mandatory = rangeOfCalculatedRole >=> mandatory'
  calculation r = case (unwrap r).calculation of
    Q qd -> pure qd
    otherwise -> throwError (error ("Attempt to acces QueryFunctionDescription of a CalculatedRole before the expression has been compiled. This counts as a system programming error." <> (unwrap $ (identifier r :: CalculatedRoleType))))
  properties = rangeOfCalculatedRole >=> propertiesOfADT
  typeIncludingBinding = rangeOfCalculatedRole
  expandedADT = rangeOfCalculatedRole
  views =  rangeOfCalculatedRole >=> viewsOfADT

rangeOfCalculatedRole :: CalculatedRole -> MonadPerspectives (ADT EnumeratedRoleType)
rangeOfCalculatedRole cr = calculation cr >>= roleCalculationRange
  where
    roleCalculationRange :: QueryFunctionDescription -> MonadPerspectives (ADT EnumeratedRoleType)
    roleCalculationRange qfd = case QT.range qfd of
      (RDOM p) -> pure p
      otherwise -> throwError (error ("range of calculation of a calculated role is not an enumerated role."))

-----------------------------------------------------------
-- ENUMERATED ROLE INSTANCE
-----------------------------------------------------------
instance enumeratedRoleRoleClass :: RoleClass EnumeratedRole EnumeratedRoleType where
  kindOfRole r = (unwrap r).kindOfRole
  roleAspects r = includeBinding (\r' -> (unwrap r').roleAspects) roleAspectsOfADT r
  context r = pure $ ST $ contextOfRepresentation r
  contextOfRepresentation r = (unwrap r).context
  binding r = pure (unwrap r).binding
  functional r = pure (unwrap r).functional
  mandatory r = pure (unwrap r).mandatory
  calculation r = pure $ SQD (CDOM $ ST $ contextOfRepresentation r) (RolGetter (ENR (identifier r))) (RDOM (ST (identifier r)))
  properties r = includeBinding (\r' -> (unwrap r').properties) propertiesOfADT r
  typeIncludingBinding r = do
    pure $ case (unwrap r).binding of
      PROD terms -> PROD (cons (ST $ identifier r) terms)
      NOTYPE -> (ST $ identifier r)
      st@(ST _) -> PROD [(ST $ identifier r), st]
      sum@(SUM _) -> PROD [(ST $ identifier r), sum]
  expandedADT r = expansionOfADT (ST $ identifier r)
  views r = includeBinding (\r' -> (unwrap r).views) viewsOfADT r

-- | A pattern of computation shared in the recursive computation of roleAspects, properties and views of a role.
-- | It computes the local value for an EnumeratedRole and then the value of the binding of the role, returning
-- | the union of the two.
-- TODO: kan dit niet vervangen worden door reduce?
includeBinding :: forall r i a. RoleClass r i => Eq a =>
  (r -> Array a) ->
  (ADT EnumeratedRoleType -> MP (Array a)) ->
  r -> MP (Array a)
includeBinding own adtF r = do
  ownAs <- pure (own r)
  binding' <- binding r
  bindingAs <- adtF binding'
  pure (union ownAs bindingAs)

-----------------------------------------------------------
-- FUNCTIONS OF ADT
-----------------------------------------------------------
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

-- | An ADT is fully expanded iff expanding any of the simple types (ST) results
-- | in the same type (the expansion is the fixpoint).
-- | Expand a reference to an EnumeratedType to a type that includes its binding . E.g.:
-- | ST (EnumeratedRoleType e), where the binding of e is ST (EnumeratedRoleType e1)
-- | and e1 has binding NOTYPE
-- | becomes PROD [ST (EnumeratedRoleType e), PROD [ST (EnumeratedRoleType e1), NOTYPE]]
expansionOfADT :: ADT EnumeratedRoleType -> MP (ADT EnumeratedRoleType)
expansionOfADT = reduce g
  where
    g :: EnumeratedRoleType -> MP (ADT EnumeratedRoleType)
    g = getPerspectType >=> typeIncludingBinding

-----------------------------------------------------------
-- FUNCTIONS ON ROLE
-----------------------------------------------------------
data Role = E EnumeratedRole | C CalculatedRole

id :: forall r i. RoleClass r i => r -> i
id = identifier

getCalculation :: Role -> MonadPerspectives QueryFunctionDescription
getCalculation (E r) = calculation r
getCalculation (C r) = calculation r

-----------------------------------------------------------
-- FUNCTIONS ON ROLETYPE
-----------------------------------------------------------
getRole :: RoleType -> MonadPerspectives Role
getRole (ENR e) = getPerspectType e >>= pure <<< E
getRole (CR c) = getPerspectType c >>= pure <<< C

-- | The range of the computation of the RoleType.
-- | Does not include the binding, for (ENR (EnumeratedRoleType e)).
rangeOfRoleCalculation :: RoleType -> MonadPerspectives (ADT EnumeratedRoleType)
rangeOfRoleCalculation = getRole >=> getCalculation >=> case _ of
    SQD _ _ (RDOM p) -> pure p
    UQD _ _ _ (RDOM p) -> pure p
    BQD _ _ _ _ (RDOM p) -> pure p
    otherwise -> empty -- NB: The Alt instance of Aff throws an error on empty!

expandedADT_ :: RoleType -> MonadPerspectives (ADT EnumeratedRoleType)
expandedADT_ = getRole >=> (case _ of
  E r -> expandedADT r
  C r -> expandedADT r)

-----------------------------------------------------------
-- FUNCTIONS ON STRING
-----------------------------------------------------------
getRoleType :: String -> MonadPerspectives RoleType
getRoleType s = ((getEnumeratedRole $ EnumeratedRoleType s) >>= pure <<< ENR <<< identifier)
  <|> ((getCalculatedRole $ CalculatedRoleType s) >>= pure <<< CR <<< identifier)
