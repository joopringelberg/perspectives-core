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

module Perspectives.Representation.Class.Role where

import Control.Monad.Error.Class (throwError)
import Control.Plus (empty, map, (<|>))
import Data.Array (cons, foldMap, uncons)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (unwrap)
import Data.Set (subset, fromFoldable)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception (error) 
import Foreign.Object (Object, toArrayWithKey) 
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, MP)
import Perspectives.Identifiers (buitenRol)
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), RoleInContext(..), domain2roleInContext, domain2roleType, range, roleInContext2Role)
import Perspectives.Query.QueryTypes (functional, mandatory) as QT
import Perspectives.Representation.ADT (ADT(..), commonLeavesInADT, product, reduce)
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.Context (roles)
import Perspectives.Representation.Class.Identifiable (class Identifiable, identifier, identifier_)
import Perspectives.Representation.Class.PersistentType (class PersistentType, ContextType, getCalculatedRole, getContext, getEnumeratedRole, getPerspectType)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.Perspective (Perspective, StateSpec)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.ThreeValuedLogic (bool2threeValued, pessimistic)
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), EnumeratedPropertyType, EnumeratedRoleType(..), PropertyType, RoleKind, RoleType(..), ViewType)
import Prelude (class Show, bind, flip, pure, ($), (<$>), (<<<), (<>), (>=>), (>>=), (<*>))

-----------------------------------------------------------
-- ROLE TYPE CLASS
-----------------------------------------------------------
class (Show r, Identifiable r i, PersistentType r i) <= RoleClass r i | r -> i, i -> r where
  typeOfRole :: r -> RoleType
  kindOfRole :: r -> RoleKind
  displayName :: r -> String
  roleAspects :: r -> MonadPerspectives (Array RoleInContext)
  context :: r -> MP (ADT ContextType)
  contextOfRepresentation :: r -> ContextType
  binding :: r -> MonadPerspectives (ADT RoleInContext)
  functional :: r -> MonadPerspectives Boolean
  mandatory :: r -> MonadPerspectives Boolean
  calculation :: r -> MonadPerspectives QueryFunctionDescription
  -- | The type of the Role. For an EnumeratedRole this is just `ST EnumeratedRoleType`.
  -- | For a CalculatedRole it is the range of its calculation.
  roleADT :: r -> MonadPerspectives (ADT RoleInContext)
  -- | The type of the Role, combined with its binding type.
  roleAndBinding :: r -> MonadPerspectives (ADT RoleInContext)
  -- | The type of the Role, including its direct aspects, including their transitive closure!
  -- | For a CalculatedRole it is the range of its calculation.
  roleAspectsADT :: r -> MonadPerspectives (ADT RoleInContext)
  perspectives :: r -> Array Perspective
  contextActions :: r -> Map StateSpec (Object Action)

-----------------------------------------------------------
-- CALCULATED ROLE INSTANCE
-----------------------------------------------------------
instance calculatedRoleRoleClass :: RoleClass CalculatedRole CalculatedRoleType where
  typeOfRole r = CR (unwrap r).id
  kindOfRole r = (unwrap r).kindOfRole
  displayName r = (unwrap r).displayName
  roleAspects = rangeOfCalculatedRole >=> pure <<< commonLeavesInADT
  context r = (rangeOfCalculatedRole >=> contextOfADT >=> \adt -> pure $ product [ST (unwrap r).context, adt]) r
  contextOfRepresentation r = (unwrap r).context
  binding = rangeOfCalculatedRole >=> bindingOfADT
  functional r = calculation r >>= pure <<< pessimistic <<< QT.functional
  mandatory r = calculation r >>= pure <<< pessimistic <<< QT.mandatory
  calculation r = case (unwrap r).calculation of
    Q qd -> pure qd
    otherwise -> throwError (error ("Attempt to acces QueryFunctionDescription of a CalculatedRole before the expression has been compiled. This counts as a system programming error." <> (unwrap $ (identifier r :: CalculatedRoleType))))
  roleADT r = calculation r >>= pure <<< unsafePartial domain2roleInContext <<< range
  roleAndBinding = rangeOfCalculatedRole
  -- Include the transitive closure of the aspects.
  roleAspectsADT = rangeOfCalculatedRole >=> reduce (getEnumeratedRole <<< roleInContext2Role >=> roleAspectsADT)
  perspectives r = (unwrap r).perspectives
  contextActions r = unwrap (unwrap r).actions

rangeOfCalculatedRole :: CalculatedRole -> MonadPerspectives (ADT RoleInContext)
rangeOfCalculatedRole cr = calculation cr >>= roleCalculationRange
  where
    roleCalculationRange :: QueryFunctionDescription -> MonadPerspectives (ADT RoleInContext)
    roleCalculationRange qfd = case range qfd of
      (RDOM x) -> pure x
      otherwise -> throwError (error ("range of calculation of a calculated role is not a role Domain."))

-----------------------------------------------------------
-- ENUMERATED ROLE INSTANCE
-----------------------------------------------------------
instance enumeratedRoleRoleClass :: RoleClass EnumeratedRole EnumeratedRoleType where
  typeOfRole r = ENR (unwrap r).id
  kindOfRole r = (unwrap r).kindOfRole
  displayName r = (unwrap r).displayName
  roleAspects r = pure $ _.roleAspects $ unwrap r
  context r = pure $ ST $ contextOfRepresentation r
  contextOfRepresentation r = (unwrap r).context
  binding r = pure (unwrap r).binding
  functional r = pure (unwrap r).functional
  mandatory r = pure (unwrap r).mandatory
  calculation r = if (unwrap r).unlinked
    then pure $ SQD
      (CDOM $ ST $ contextOfRepresentation r)
      (DataTypeGetterWithParameter GetRoleInstancesForContextFromDatabaseF (identifier_ r))
      (RDOM (ST $ RoleInContext {context: contextOfRepresentation r, role: (identifier r)}))
      (bool2threeValued (unwrap r).functional)
      (bool2threeValued (unwrap r).mandatory)
    else pure $ SQD
      (CDOM $ ST $ contextOfRepresentation r)
      (RolGetter (ENR (identifier r)))
      (RDOM (ST $ RoleInContext {context: contextOfRepresentation r, role: (identifier r)}))
      (bool2threeValued (unwrap r).functional)
      (bool2threeValued (unwrap r).mandatory)
  roleADT r = pure $ ST $ RoleInContext {context: (unwrap r).context, role: identifier r}
  roleAndBinding r = pure $ case (unwrap r).binding of
    PROD terms -> PROD (cons (ST $ RoleInContext {context: (unwrap r).context, role: identifier r}) terms)
    EMPTY -> (ST $ RoleInContext {context: (unwrap r).context, role: identifier r})
    st@(ST _) -> PROD [(ST $ RoleInContext {context: (unwrap r).context, role: identifier r}), st]
    sum@(SUM _) -> PROD [(ST $ RoleInContext {context: (unwrap r).context, role: identifier r}), sum]
    UNIVERSAL -> UNIVERSAL
  roleAspectsADT r@(EnumeratedRole{roleAspects}) = do
    radt <- roleADT r
    product <<< (flip cons [radt]) <$> reduce (getEnumeratedRole <<< roleInContext2Role >=> roleAspectsADT)
      (PROD (map ST roleAspects))
  perspectives r = (unwrap r).perspectives
  contextActions r = unwrap (unwrap r).actions

-- f :: RoleInContext -> MP (ADT RoleInContext)
-- f (RoleInContext{role})= getEnumeratedRole role >>= roleAspectsADT
-- f = getEnumeratedRole <<< roleInContext2Role >=> roleAspectsADT

-----------------------------------------------------------
-- FUNCTIONS OF ADT
-----------------------------------------------------------
-- | Note that this function cannot be implemented using `reduce`, as we treat SUM's and PROD's
-- | both with logical and.
adtIsFunctional :: ADT EnumeratedRoleType -> MP Boolean
adtIsFunctional (ST r) = getEnumeratedRole r >>= \(EnumeratedRole{functional}) -> pure functional
adtIsFunctional (SUM adts) = do
    (bools :: Array Boolean) <- traverse adtIsFunctional adts
    pure $ unwrap $ foldMap Conj bools
adtIsFunctional (PROD adts) = do
    (bools :: Array Boolean) <- traverse adtIsFunctional adts
    pure $ unwrap $ foldMap Conj bools
-- A role with no binding specified has binding EMPTY. roleAndBinding includes the
-- binding type and then adtIsFunctional should compute True for an otherwise functional
-- EnumeratedRole type.
adtIsFunctional EMPTY = pure true
adtIsFunctional UNIVERSAL = pure false

adtIsMandatory :: ADT EnumeratedRoleType -> MP Boolean
adtIsMandatory (ST r) = getEnumeratedRole r >>= \(EnumeratedRole{mandatory}) -> pure mandatory
adtIsMandatory (SUM adts) = do
    (bools :: Array Boolean) <- traverse adtIsMandatory adts
    pure $ unwrap $ foldMap Conj bools
adtIsMandatory (PROD adts) = do
    (bools :: Array Boolean) <- traverse adtIsMandatory adts
    pure $ unwrap $ foldMap Conj bools
-- A role with no binding specified has binding EMPTY. roleAndBinding includes the
-- binding type and then adtIsMandatory should compute True for an otherwise functional
-- EnumeratedRole type.
adtIsMandatory EMPTY = pure true
adtIsMandatory UNIVERSAL = pure false

--------------------------------------------------------------------------------------------------
---- PROPERTYSET
--------------------------------------------------------------------------------------------------
-- | All properties, computed recursively over binding and Aspects, of the Role ADT.
-- | UNIVERSAL is treated like EMPTY.
allProperties :: ADT EnumeratedRoleType -> MP (Array PropertyType)
allProperties = reduce magic
  where
    magic :: EnumeratedRoleType -> MP (Array PropertyType)
    magic role = do
      EnumeratedRole{roleAspects, binding, properties} <- getEnumeratedRole role
      x <- pure $ product (cons (roleInContext2Role <$> binding) (ST <<< roleInContext2Role <$> roleAspects))
      case x of
        EMPTY -> pure properties
        otherwise -> do
          props <- reduce magic otherwise
          pure $ props <> properties

-- | All properties, computed recursively over the role ADT and its Aspects - but excluding the binding.
-- | UNIVERSAL is treated like EMPTY.
allLocallyRepresentedProperties :: ADT EnumeratedRoleType -> MP (Array PropertyType)
allLocallyRepresentedProperties = reduce magic
  where
    magic :: EnumeratedRoleType -> MP (Array PropertyType)
    magic role = do
      EnumeratedRole{roleAspects, properties} <- getEnumeratedRole role
      x <- pure $ product (ST <$> (roleInContext2Role <$> roleAspects))
      case x of
        EMPTY -> pure properties
        otherwise -> do
          props <- reduce magic otherwise
          pure $ props <> properties

allLocalAliases :: ADT EnumeratedRoleType -> MP (Array (Tuple String EnumeratedPropertyType))
allLocalAliases = reduce magic
  where
    magic :: EnumeratedRoleType -> MP (Array (Tuple String EnumeratedPropertyType))
    magic role = do
      EnumeratedRole{propertyAliases} <- getEnumeratedRole role
      pure $ toArrayWithKey Tuple propertyAliases

-- | Similar to propertySet, except for UNIVERSAL.
-- | UNIVERSAL is treated like EMPTY.
directProperties :: ADT EnumeratedRoleType -> MP (Array PropertyType)
directProperties = reduce magic
  where
    magic :: EnumeratedRoleType -> MP (Array PropertyType)
    magic rt = do
      EnumeratedRole{properties} <- getEnumeratedRole rt
      pure properties

--------------------------------------------------------------------------------------------------
---- ALLFILLERS
--------------------------------------------------------------------------------------------------
-- | All fillers of a role (but not the role itself), computed recursively over binding, of the Role ADT.
-- | UNIVERSAL is treated like EMPTY.
allFillers :: ADT EnumeratedRoleType -> MP (ADT EnumeratedRoleType)
allFillers = reduce magic >=> case _ of 
  PROD as -> case uncons as of 
    Nothing -> pure $ PROD []
    -- Slightly shaky but based on the constructon below.
    Just {head, tail} -> pure $ PROD tail
  otherwise -> pure otherwise
  where
    magic :: EnumeratedRoleType -> MP (ADT EnumeratedRoleType)
    magic role = do
      EnumeratedRole{binding} <- getEnumeratedRole role
      case binding of 
        EMPTY -> pure $ ST role
        otherwise -> do
          expansion <- reduce magic (roleInContext2Role <$> otherwise)
          pure $ PROD [ST role, expansion]

rolaAndAllFillers :: ADT EnumeratedRoleType -> MP (ADT EnumeratedRoleType)
rolaAndAllFillers = reduce magic
  where
    magic :: EnumeratedRoleType -> MP (ADT EnumeratedRoleType)
    magic role = do
      EnumeratedRole{binding} <- getEnumeratedRole role
      case binding of 
        EMPTY -> pure $ ST role
        otherwise -> do
          expansion <- reduce magic (roleInContext2Role <$> otherwise)
          pure $ PROD [ST role, expansion]

--------------------------------------------------------------------------------------------------
---- ROLESET
--------------------------------------------------------------------------------------------------
-- | The roles of this context and its aspects, recursively.
allRoles :: ADT ContextType -> MP (Array RoleType)
allRoles = reduce magic
  where
    magic :: ContextType -> MP (Array RoleType)
    magic ct = do
      c <-  getContext ct
      pure $ roles c

-----------------------------------------------------------
-- LESSTHANOREQUALTO
-----------------------------------------------------------
-- | `p lessThanOrEqualTo q` means: p is less specific than q, or equal to q.
-- | `p lessThanOrEqualTo q` equals: `q greaterThanOrEqualTo p`
-- lessThanOrEqualTo :: ADT RoleInContext -> ADT RoleInContext -> MP Boolean
-- lessThanOrEqualTo p q = p `hasNotMorePropertiesThan` q
-- lessThanOrEqualTo p q = (&&) <$> (p `hasNotMorePropertiesThan` q) <*> (subsetPSet <$> actionSet p <*> actionSet q)

hasNotMorePropertiesThan :: ADT RoleInContext -> ADT RoleInContext -> MP Boolean
hasNotMorePropertiesThan p q = subset <$> (allProperties >=> pure <<< fromFoldable) (roleInContext2Role <$> p) <*> (allProperties >=> pure <<< fromFoldable) (roleInContext2Role <$> q)

-----------------------------------------------------------
-- CONTEXTOFADT
-----------------------------------------------------------
-- | The context of an ADT
contextOfADT :: ADT RoleInContext -> MP (ADT ContextType)
-- TODO: handle CalculatedRole.
contextOfADT = reduce (pure <<< roleInContext2Role >=> getEnumeratedRole >=> context)

-----------------------------------------------------------
-- EXTERNALROLEOFADT
-----------------------------------------------------------
-- | The external role of an ADT ContextType
externalRoleOfADT :: ADT ContextType -> MP (ADT RoleInContext)
-- TODO: handle CalculatedRole.
externalRoleOfADT = pure <<< map \ctype -> RoleInContext {context: ctype, role: EnumeratedRoleType $ buitenRol (unwrap ctype)}

-----------------------------------------------------------
-- BINDINGOFADT
-----------------------------------------------------------
-- | The binding of an ADT.
bindingOfADT :: ADT RoleInContext -> MP (ADT RoleInContext)
bindingOfADT = reduce (getEnumeratedRole <<< roleInContext2Role >=> binding)

-----------------------------------------------------------
-- VIEWSOFADT
-----------------------------------------------------------
-- | All views, computed recursively over binding and Aspects, of the Role ADT.
allViews :: ADT EnumeratedRoleType -> MP (Array ViewType)
allViews = reduce magic
  where
    magic :: EnumeratedRoleType -> MP (Array ViewType)
    magic role = do
      EnumeratedRole{roleAspects, binding, views} <- getEnumeratedRole role
      x <- pure $ product (cons (roleInContext2Role <$> binding) (ST <<< roleInContext2Role <$> roleAspects))
      case x of
        EMPTY -> pure views
        otherwise -> do
          vws <- reduce magic otherwise
          pure $ vws <> views

-----------------------------------------------------------
-- FUNCTIONS ON ROLE
-----------------------------------------------------------
data Role = E EnumeratedRole | C CalculatedRole

id :: forall r i. RoleClass r i => r -> i
id = identifier

getCalculation :: Role -> MonadPerspectives QueryFunctionDescription
getCalculation (E r) = calculation r
getCalculation (C r) = calculation r

adtOfRole :: Role -> MP (ADT RoleInContext)
adtOfRole (E e) = roleADT e
adtOfRole (C c) = roleADT c

adtOfRoleAndBinding :: Role -> MP (ADT RoleInContext)
adtOfRoleAndBinding (E e) = roleAndBinding e
adtOfRoleAndBinding (C c) = roleAndBinding c

contextOfRole :: Role -> MP (ADT ContextType)
contextOfRole (E e) = context e
contextOfRole (C c) = context c

-- Partial, because of the embedded case and because domain2roleType is Partial because it just handles
-- RDOM cases.
-- | The same result as roleAspects, but not in MonadPerspectives.
expansionOfRole :: Partial => Role -> Array EnumeratedRoleType
expansionOfRole (E (EnumeratedRole {id:i})) = [i]
expansionOfRole (C (CalculatedRole {calculation})) = roleInContext2Role <$> (commonLeavesInADT $ domain2roleType $ range $ (case calculation of Q qd -> qd))

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
    SQD _ _ (RDOM adt) _ _ -> pure $ roleInContext2Role <$> adt
    UQD _ _ _ (RDOM adt) _ _ -> pure $ roleInContext2Role <$> adt
    BQD _ _ _ _ (RDOM adt) _ _ -> pure $ roleInContext2Role <$> adt
    MQD _ _ _ (RDOM adt) _ _ -> pure $ roleInContext2Role <$> adt
    otherwise -> empty -- NB: The Alt instance of Aff throws an error on empty!

typeIncludingAspects :: RoleType -> MonadPerspectives (ADT RoleInContext)
typeIncludingAspects = getRole >=> (case _ of
  E r -> roleAspectsADT r
  C r -> roleAspectsADT r)

roleTypeIsFunctional :: RoleType -> MonadPerspectives Boolean
roleTypeIsFunctional = getRole >=> (case _ of
  E r -> functional r
  C r -> functional r)

roleTypeIsMandatory :: RoleType -> MonadPerspectives Boolean
roleTypeIsMandatory = getRole >=> (case _ of
  E r -> mandatory r
  C r -> mandatory r)

bindingOfRole :: RoleType -> MonadPerspectives (ADT RoleInContext)
bindingOfRole = getRole >=> binding'
  where
    binding' :: Role -> MonadPerspectives (ADT RoleInContext)
    binding' (E r) = binding r
    binding' (C r) = binding r

contextOfRoleType :: RoleType -> MonadPerspectives (ADT ContextType)
contextOfRoleType (ENR e) = getPerspectType e >>= context
contextOfRoleType (CR c) = getPerspectType c >>= context

contextOfRepresentationOfRole :: RoleType -> MonadPerspectives (ADT ContextType)
contextOfRepresentationOfRole (ENR e) = getPerspectType e >>= pure <<< ST <<< contextOfRepresentation
contextOfRepresentationOfRole (CR c) = getPerspectType c >>= pure <<< ST <<< contextOfRepresentation

perspectivesOfRoleType :: RoleType -> MonadPerspectives (Array Perspective)
perspectivesOfRoleType (ENR e) = getPerspectType e >>= pure <<< perspectives
perspectivesOfRoleType (CR c) = getPerspectType c >>= pure <<< perspectives

roleKindOfRoleType :: RoleType -> MonadPerspectives RoleKind
roleKindOfRoleType (ENR e) = getPerspectType e >>= pure <<< kindOfRole
roleKindOfRoleType (CR c) = getPerspectType c >>= pure <<< kindOfRole

displayNameOfRoleType :: RoleType -> MonadPerspectives String
displayNameOfRoleType (ENR e) = getEnumeratedRole e >>= pure <<< displayName
displayNameOfRoleType (CR e) = getCalculatedRole e >>= pure <<< displayName

roleTypeIsEnumerated :: RoleType -> Boolean
roleTypeIsEnumerated (ENR _) = true
roleTypeIsEnumerated _ = false

actionsOfRoleType :: RoleType -> MonadPerspectives (Map StateSpec (Object Action))
actionsOfRoleType (ENR r) = getPerspectType r >>= pure <<< contextActions
actionsOfRoleType (CR r) = getPerspectType r >>= pure <<< contextActions

roleADTOfRoleType :: RoleType -> MonadPerspectives (ADT RoleInContext)
roleADTOfRoleType (ENR r) = getPerspectType r >>= roleADT
roleADTOfRoleType (CR r) = getPerspectType r >>= roleADT

-----------------------------------------------------------
-- FUNCTIONS ON STRING
-----------------------------------------------------------
getRoleType :: String -> MonadPerspectives RoleType
getRoleType s = ((getEnumeratedRole $ EnumeratedRoleType s) >>= pure <<< ENR <<< identifier)
  <|> ((getCalculatedRole $ CalculatedRoleType s) >>= pure <<< CR <<< identifier)

getRoleADTFromString :: String -> MonadPerspectives (ADT RoleInContext)
getRoleADTFromString s =
  (getEnumeratedRole (EnumeratedRoleType s) >>= roleADT) <|>
  (getCalculatedRole (CalculatedRoleType s) >>= roleADT)
