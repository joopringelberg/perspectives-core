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
import Data.Array (cons, null, singleton, (:))
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Data.Set (subset, fromFoldable)
import Data.Traversable (traverse)
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, MP)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), range, domain2roleType)
import Perspectives.Query.QueryTypes (functional, mandatory) as QT
import Perspectives.Representation.ADT (ADT(..), product, reduce)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.Context (contextAspects, externalRole, roles)
import Perspectives.Representation.Class.Identifiable (class Identifiable, identifier, identifier_)
import Perspectives.Representation.Class.PersistentType (class PersistentType, ContextType, getCalculatedRole, getContext, getEnumeratedRole, getPerspectType)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.ExplicitSet (ExplicitSet(..), intersectionOfArrays, intersectionPset, subsetPSet, unionOfArrays, unionPset)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.ThreeValuedLogic (bool2threeValued, pessimistic)
import Perspectives.Representation.TypeIdentifiers (ActionType, CalculatedRoleType(..), EnumeratedRoleType(..), PropertyType, RoleKind, RoleType(..), ViewType)
import Prelude (class Show, class Eq, bind, flip, pure, show, ($), (<$>), (<<<), (<>), (>=>), (>>=), (<*>), (&&))

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
  -- | The type of the Role. For an EnumeratedRole this is just `ST EnumeratedRoleType`.
  -- | For a CalculatedRole it is the range of its calculation.
  roleADT :: r -> MonadPerspectives (ADT EnumeratedRoleType)
  -- | The type of the Role, combined with its binding type.
  roleAndBinding :: r -> MonadPerspectives (ADT EnumeratedRoleType)
  -- | The type of the Role, including its direct aspects: not their transitive closure!
  -- | For a CalculatedRole it is the range of its calculation.
  roleAspectsADT :: r -> MonadPerspectives (ADT EnumeratedRoleType)
  -- | Includes: the type of the Role, its own binding (not the bindings transitive closure!) and its own aspects (not their transitive closure!).
  -- | For a CalculatedRole it is the range of its calculation.
  roleAspectsBindingADT :: r -> MonadPerspectives (ADT EnumeratedRoleType)
  perspectives :: r -> Array ActionType

rangeOfRoleCalculation' :: String -> MonadPerspectives (ADT EnumeratedRoleType)
rangeOfRoleCalculation' r = rangeOfRoleCalculation'_ (EnumeratedRoleType r) <|> rangeOfRoleCalculation'_ (CalculatedRoleType r)
  where
    rangeOfRoleCalculation'_ :: forall r i. RoleClass r i => i -> MonadPerspectives (ADT EnumeratedRoleType)
    rangeOfRoleCalculation'_ i = getPerspectType i >>= calculation >>= case _ of
        SQD _ _ (RDOM p) _ _ -> pure p
        UQD _ _ _ (RDOM p) _ _ -> pure p
        BQD _ _ _ _ (RDOM p) _ _ -> pure p
        otherwise -> empty

-----------------------------------------------------------
-- CALCULATED ROLE INSTANCE
-----------------------------------------------------------
instance calculatedRoleRoleClass :: RoleClass CalculatedRole CalculatedRoleType where
  kindOfRole r = (unwrap r).kindOfRole
  roleAspects = rangeOfCalculatedRole >=> roleAspectsOfADT
  context r = (rangeOfCalculatedRole >=> contextOfADT >=> \adt -> pure $ product [ST (unwrap r).context, adt]) r
  contextOfRepresentation r = (unwrap r).context
  binding = rangeOfCalculatedRole >=> bindingOfADT
  functional r = calculation r >>= pure <<< pessimistic <<< QT.functional
  mandatory r = calculation r >>= pure <<< pessimistic <<< QT.mandatory
  calculation r = case (unwrap r).calculation of
    Q qd -> pure qd
    otherwise -> throwError (error ("Attempt to acces QueryFunctionDescription of a CalculatedRole before the expression has been compiled. This counts as a system programming error." <> (unwrap $ (identifier r :: CalculatedRoleType))))
  roleADT r = calculation r >>= pure <<< unsafePartial domain2roleType <<< range
  roleAndBinding = rangeOfCalculatedRole
  roleAspectsADT = rangeOfCalculatedRole
  roleAspectsBindingADT = rangeOfCalculatedRole
  perspectives r = (unwrap r).perspectives

rangeOfCalculatedRole :: CalculatedRole -> MonadPerspectives (ADT EnumeratedRoleType)
rangeOfCalculatedRole cr = calculation cr >>= roleCalculationRange
  where
    roleCalculationRange :: QueryFunctionDescription -> MonadPerspectives (ADT EnumeratedRoleType)
    roleCalculationRange qfd = case range qfd of
      (RDOM p) -> pure p
      otherwise -> throwError (error ("range of calculation of a calculated role is not a role Domain."))

-----------------------------------------------------------
-- ENUMERATED ROLE INSTANCE
-----------------------------------------------------------
instance enumeratedRoleRoleClass :: RoleClass EnumeratedRole EnumeratedRoleType where
  kindOfRole r = (unwrap r).kindOfRole
  roleAspects r = pure $ _.roleAspects $ unwrap r
  context r = pure $ ST $ contextOfRepresentation r
  contextOfRepresentation r = (unwrap r).context
  binding r = pure (unwrap r).binding
  functional r = pure (unwrap r).functional
  mandatory r = pure (unwrap r).mandatory
  calculation r = if (unwrap r).unlinked
    then pure $ SQD (CDOM $ ST $ contextOfRepresentation r) (DataTypeGetterWithParameter GetRoleInstancesForContextFromDatabaseF (identifier_ r)) (RDOM (ST (identifier r))) (bool2threeValued (unwrap r).functional) (bool2threeValued (unwrap r).mandatory)
    else pure $ SQD (CDOM $ ST $ contextOfRepresentation r) (RolGetter (ENR (identifier r))) (RDOM (ST (identifier r))) (bool2threeValued (unwrap r).functional) (bool2threeValued (unwrap r).mandatory)
  roleADT r = pure (ST $ identifier r)
  roleAndBinding r = pure $ case (unwrap r).binding of
    PROD terms -> PROD (cons (ST $ identifier r) terms)
    EMPTY -> (ST $ identifier r)
    st@(ST _) -> PROD [(ST $ identifier r), st]
    sum@(SUM _) -> PROD [(ST $ identifier r), sum]
    UNIVERSAL -> UNIVERSAL
  roleAspectsADT r@(EnumeratedRole{roleAspects}) = do
    aspects <- pure $ ST <$> roleAspects
    if null aspects
      then pure (ST $ identifier r)
      else pure $ PROD (cons (ST $ identifier r) aspects)
  roleAspectsBindingADT r@(EnumeratedRole{roleAspects}) = do
    aspects <- pure $ ST <$> roleAspects
    if null aspects
      then pure $ case (unwrap r).binding of
        PROD terms -> PROD (cons (ST $ identifier r) terms)
        EMPTY -> (ST $ identifier r)
        st@(ST _) -> PROD [(ST $ identifier r), st]
        sum@(SUM _) -> PROD [(ST $ identifier r), sum]
        UNIVERSAL -> UNIVERSAL
      else pure $ case (unwrap r).binding of
        PROD terms -> PROD (cons (ST $ identifier r) (terms <> aspects))
        EMPTY -> PROD (cons (ST $ identifier r) aspects)
        st@(ST _) -> PROD ([(ST $ identifier r), st] <> aspects)
        sum@(SUM _) -> PROD (sum : (ST $ identifier r) : aspects)
        UNIVERSAL -> UNIVERSAL
  perspectives r = (unwrap r).perspectives

-----------------------------------------------------------
-- FUNCTIONS OF ADT
-----------------------------------------------------------

--------------------------------------------------------------------------------------------------
---- PROPERTYSET
--------------------------------------------------------------------------------------------------
-- | All properties, computed recursively over binding and Aspects, of the Role ADT.
-- | UNIVERSAL is treated like EMPTY.
allProperties :: ADT EnumeratedRoleType -> MP (Array PropertyType)
allProperties = reduce magic
  where
    magic :: EnumeratedRoleType -> MP (Array PropertyType)
    magic rt = do
      EnumeratedRole{roleAspects, binding, properties} <- getEnumeratedRole rt
      x <- pure $ product (cons binding (ST <$> roleAspects))
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
    magic rt = do
      EnumeratedRole{roleAspects, properties} <- getEnumeratedRole rt
      x <- pure $ product (ST <$> roleAspects)
      case x of
        EMPTY -> pure properties
        otherwise -> do
          props <- reduce magic otherwise
          pure $ props <> properties

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
---- ROLESET
--------------------------------------------------------------------------------------------------
type RoleSet = ExplicitSet RoleType

-- | The ADT must be normalised in the sense that no set of terms contains EMPTY or UNIVERSAL.
-- | Computes all roles, but does not descend into Aspects in the ST case.
roleSet :: ADT ContextType -> MP RoleSet
roleSet (ST r) = getContext r >>= pure <<< PSet <<< roles
roleSet (SUM terms) = traverse roleSet terms >>= pure <<< intersectionPset
roleSet (PROD terms) = traverse roleSet terms >>= pure <<< unionPset
roleSet UNIVERSAL = pure Universal
roleSet EMPTY = pure Empty

-- | The roles of this context and its aspects, recursively.
allRoles :: ADT ContextType -> MP (Array RoleType)
allRoles = reduce magic
  where
    magic :: ContextType -> MP (Array RoleType)
    magic ct = do
      c <-  getContext ct
      if null (contextAspects c)
        then pure $ roles c
        else do
          rs <- reduce magic (PROD (ST <$> contextAspects c))
          pure (roles c <> rs)

--------------------------------------------------------------------------------------------------
---- ACTIONSET
--------------------------------------------------------------------------------------------------
type ActionSet = ExplicitSet ActionType

-- | The ADT must be normalised in the sense that no set of terms contains EMPTY or UNIVERSAL.
actionSet :: ADT EnumeratedRoleType -> MP ActionSet
actionSet (ST r) = getEnumeratedRole r >>= pure <<< PSet <<< _.perspectives <<< unwrap
actionSet (SUM terms) = traverse actionSet terms >>= pure <<< intersectionPset
actionSet (PROD terms) = traverse actionSet terms >>= pure <<< unionPset
actionSet UNIVERSAL = pure Universal
actionSet EMPTY = pure Empty

--------------------------------------------------------------------------------------------------
---- ENUMERATEDTYPESINADT
--------------------------------------------------------------------------------------------------
leavesInADT :: forall a. Eq a => ADT a -> Array a
leavesInADT = unwrap <<< reduce ((pure <<< singleton) :: a -> Identity (Array a))

-----------------------------------------------------------
-- LESSTHANOREQUALTO
-----------------------------------------------------------
-- | `p lessThanOrEqualTo q` means: p is less specific than q, or equal to q.
-- | `p lessThanOrEqualTo q` equals: `q greaterThanOrEqualTo p`
lessThanOrEqualTo :: ADT EnumeratedRoleType -> ADT EnumeratedRoleType -> MP Boolean
lessThanOrEqualTo p q = (&&) <$> (p `hasNotMorePropertiesThan` q) <*> (subsetPSet <$> actionSet p <*> actionSet q)

hasNotMorePropertiesThan :: ADT EnumeratedRoleType -> ADT EnumeratedRoleType -> MP Boolean
hasNotMorePropertiesThan p q = subset <$> (allProperties >=> pure <<< fromFoldable) p <*> (allProperties >=> pure <<< fromFoldable) q

-- | `q greaterThanOrEqualTo p` means: q is more specific than p, or equal to p
-- | If you use `less specific` instead of `more specific`, flip the arguments.
-- | If you use `more general` instead of `more specific`, flip them, too.
-- | So `less specific` instead of `more general` means flipping twice and is a no-op.
-- | Therefore `less specific` equals `more general`.
greaterThanOrEqualTo :: ADT EnumeratedRoleType -> ADT EnumeratedRoleType -> MP Boolean
greaterThanOrEqualTo = flip lessThanOrEqualTo

-----------------------------------------------------------
-- CONTEXTOFADT
-----------------------------------------------------------
-- | The context of an ADT
contextOfADT :: ADT EnumeratedRoleType -> MP (ADT ContextType)
-- TODO: handle CalculatedRole.
contextOfADT = reduce (getEnumeratedRole >=> context)

-----------------------------------------------------------
-- EXTERNALROLEOFADT
-----------------------------------------------------------
-- | The external role of an ADT ContextType
externalRoleOfADT :: ADT ContextType -> MP (ADT EnumeratedRoleType)
-- TODO: handle CalculatedRole.
externalRoleOfADT = reduce (getContext >=> pure <<< ST <<< externalRole)

-----------------------------------------------------------
-- BINDINGOFADT
-----------------------------------------------------------
-- | The binding of an ADT.
bindingOfADT :: ADT EnumeratedRoleType -> MP (ADT EnumeratedRoleType)
-- TODO: handle CalculatedRole.
bindingOfADT = reduce (getEnumeratedRole >=> binding)

-----------------------------------------------------------
-- VIEWSOFADT
-----------------------------------------------------------
-- | All views, computed recursively over binding and Aspects, of the Role ADT.
allViews :: ADT EnumeratedRoleType -> MP (Array ViewType)
allViews = reduce magic
  where
    magic :: EnumeratedRoleType -> MP (Array ViewType)
    magic rt = do
      EnumeratedRole{roleAspects, binding, views} <- getEnumeratedRole rt
      x <- pure $ product (cons binding (ST <$> roleAspects))
      case x of
        EMPTY -> pure views
        otherwise -> do
          vws <- reduce magic otherwise
          pure $ vws <> views

-----------------------------------------------------------
-- ROLEASPECTSOFADT
-----------------------------------------------------------
-- | The ADT must be normalised in the sense that no set of terms contains EMPTY or UNIVERSAL.
-- | The aspects, not including the type itself.
roleAspectsOfADT :: ADT EnumeratedRoleType -> MP (Array EnumeratedRoleType)
-- roleAspectsOfADT (ST (ENR r)) = getEnumeratedRole r >>= pure <<< _.roleAspects <<< unwrap
-- roleAspectsOfADT (ST (CR r)) = getCalculatedRole r >>= (rangeOfCalculatedRole >=> roleAspectsOfADT)
roleAspectsOfADT (ST r) = getEnumeratedRole r >>= pure <<< _.roleAspects <<< unwrap
roleAspectsOfADT (SUM terms) = traverse roleAspectsOfADT terms >>= pure <<< intersectionOfArrays
roleAspectsOfADT (PROD terms) = traverse roleAspectsOfADT terms >>= pure <<< unionOfArrays
roleAspectsOfADT EMPTY = pure []
roleAspectsOfADT UNIVERSAL = throwError (error $ show UniversalRoleHasNoParts)

-----------------------------------------------------------
-- TRANSITIVEROLEASPECTSOFADT
-----------------------------------------------------------
-- | The result will never be a SUM.
-- transitiveClosureOfRoleADT :: ADT EnumeratedRoleType -> MP (ADT EnumeratedRoleType)
-- transitiveClosureOfRoleADT (ST r) = getEnumeratedRole r >>= roleAspectsBindingADT >>= \adt -> case adt of
--   s@(ST _) -> pure s
--   otherwise -> transitiveClosureOfRoleADT otherwise
-- transitiveClosureOfRoleADT EMPTY = pure EMPTY
-- transitiveClosureOfRoleADT UNIVERSAL = pure UNIVERSAL
-- -- hoe weet je dat dit geen sum is?
-- transitiveClosureOfRoleADT (PROD terms) = traverse transitiveClosureOfRoleADT terms >>= pure <<< foldl unionOfADT EMPTY
-- transitiveClosureOfRoleADT (SUM terms) = traverse transitiveClosureOfRoleADT terms >>= pure <<< foldl intersectionOfADT UNIVERSAL

-----------------------------------------------------------
-- GREATESTCOMMONANCESTOROFROLES
-----------------------------------------------------------
-- greatestCommonAncestorOfRoles :: ADT EnumeratedRoleType -> ADT EnumeratedRoleType -> MP (ADT EnumeratedRoleType)
-- greatestCommonAncestorOfRoles a b = do
--   a' <- transitiveClosureOfRoleADT a
--   b' <- transitiveClosureOfRoleADT b
--   pure $ intersectionOfADT a' b'

-----------------------------------------------------------
-- FUNCTIONS ON ROLE
-----------------------------------------------------------
data Role = E EnumeratedRole | C CalculatedRole

id :: forall r i. RoleClass r i => r -> i
id = identifier

getCalculation :: Role -> MonadPerspectives QueryFunctionDescription
getCalculation (E r) = calculation r
getCalculation (C r) = calculation r

-- | Includes: the type of the Role, its own binding (not the bindings transitive closure!) and its own aspects (not their transitive closure!).
-- | For a CalculatedRole it is the range of its calculation.
adtOfRoleAspectsBinding :: Role -> MP (ADT EnumeratedRoleType)
adtOfRoleAspectsBinding (E e) = roleAspectsBindingADT e
adtOfRoleAspectsBinding (C c) = roleAspectsBindingADT c

adtOfRole :: Role -> MP (ADT EnumeratedRoleType)
adtOfRole (E e) = roleADT e
adtOfRole (C c) = roleADT c

adtOfRoleAndBinding :: Role -> MP (ADT EnumeratedRoleType)
adtOfRoleAndBinding (E e) = roleAndBinding e
adtOfRoleAndBinding (C c) = roleAndBinding c

contextOfRole :: Role -> MP (ADT ContextType)
contextOfRole (E e) = context e
contextOfRole (C c) = context c

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
    SQD _ _ (RDOM p) _ _ -> pure p
    UQD _ _ _ (RDOM p) _ _ -> pure p
    BQD _ _ _ _ (RDOM p) _ _ -> pure p
    otherwise -> empty -- NB: The Alt instance of Aff throws an error on empty!

typeExcludingBinding_ :: RoleType -> MonadPerspectives (ADT EnumeratedRoleType)
typeExcludingBinding_ = getRole >=> (case _ of
  E r -> roleADT r
  C r -> roleADT r)

typeIncludingAspects :: RoleType -> MonadPerspectives (ADT EnumeratedRoleType)
typeIncludingAspects = getRole >=> (case _ of
  E r -> roleAspectsADT r
  C r -> roleAspectsADT r)

typeIncludingAspectsBinding :: RoleType -> MonadPerspectives (ADT EnumeratedRoleType)
typeIncludingAspectsBinding = getRole >=> (case _ of
  E r -> roleAspectsBindingADT r
  C r -> roleAspectsBindingADT r)

roleTypeIsFunctional :: RoleType -> MonadPerspectives Boolean
roleTypeIsFunctional = getRole >=> (case _ of
  E r -> functional r
  C r -> functional r)

roleTypeIsMandatory :: RoleType -> MonadPerspectives Boolean
roleTypeIsMandatory = getRole >=> (case _ of
  E r -> mandatory r
  C r -> mandatory r)

bindingOfRole :: RoleType -> MonadPerspectives (ADT EnumeratedRoleType)
bindingOfRole = getRole >=> binding'
  where
    binding' :: Role -> MonadPerspectives (ADT EnumeratedRoleType)
    binding' (E r) = binding r
    binding' (C r) = binding r

contextOfRoleType :: RoleType -> MonadPerspectives (ADT ContextType)
contextOfRoleType (ENR e) = getPerspectType e >>= context
contextOfRoleType (CR c) = getPerspectType c >>= context

contextOfRepresentationOfRole :: RoleType -> MonadPerspectives (ADT ContextType)
contextOfRepresentationOfRole (ENR e) = getPerspectType e >>= pure <<< ST <<< contextOfRepresentation
contextOfRepresentationOfRole (CR c) = getPerspectType c >>= pure <<< ST <<< contextOfRepresentation

perspectivesOfRoleType :: RoleType -> MonadPerspectives (Array ActionType)
perspectivesOfRoleType (ENR e) = getPerspectType e >>= pure <<< perspectives
perspectivesOfRoleType (CR c) = getPerspectType c >>= pure <<< perspectives
-----------------------------------------------------------
-- FUNCTIONS ON STRING
-----------------------------------------------------------
getRoleType :: String -> MonadPerspectives RoleType
getRoleType s = ((getEnumeratedRole $ EnumeratedRoleType s) >>= pure <<< ENR <<< identifier)
  <|> ((getCalculatedRole $ CalculatedRoleType s) >>= pure <<< CR <<< identifier)

getRoleADTFromString :: String -> MonadPerspectives (ADT EnumeratedRoleType)
getRoleADTFromString s =
  (getEnumeratedRole (EnumeratedRoleType s) >>= roleADT) <|>
  (getCalculatedRole (CalculatedRoleType s) >>= roleADT)
