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
import Data.Array (cons, filterA, null, union, (:))
import Data.Newtype (unwrap)
import Data.Set (subset, fromFoldable, Set)
import Data.Traversable (traverse)
import Effect.Exception (error)
import Foreign.Object (values)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, MP)
import Perspectives.Parsing.Arc.IndentParser (upperLeft)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..))
import Perspectives.Query.QueryTypes (functional, mandatory, range) as QT
import Perspectives.Representation.ADT (ADT(..), product, sum)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.Identifiable (class Identifiable, identifier)
import Perspectives.Representation.Class.PersistentType (class PersistentType, ContextType, getCalculatedRole, getContext, getEnumeratedRole, getPerspectType)
import Perspectives.Representation.Context (roles, externalRole)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.ExplicitSet (ExplicitSet(..), elements, intersectionOfArrays, intersectionPset, subsetPSet, unionOfArrays, unionPset)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.ThreeValuedLogic (bool2threeValued, pessimistic)
import Perspectives.Representation.TypeIdentifiers (ActionType, CalculatedRoleType(..), EnumeratedRoleType(..), PropertyType, RoleKind, RoleType(..), ViewType)
import Perspectives.Representation.View (propertyReferences)
import Prelude (class Eq, class Show, bind, flip, join, map, pure, show, ($), (<$>), (<<<), (<>), (>=>), (>>=), (<*>), (&&))

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
  -- | The type of the Role. For an EnumeratedRole this is just `ST EnumeratedRoleType`.
  -- | For a CalculatedRole it is the range of its calculation.
  roleADT :: r -> MonadPerspectives (ADT EnumeratedRoleType)
  -- | The type of the Role, including its direct aspects: not their transitive closure!
  -- | For a CalculatedRole it is the range of its calculation.
  roleAspectsADT :: r -> MonadPerspectives (ADT EnumeratedRoleType)
  -- | The type of the Role, including its direct binding (not the transitive closure!) and its direct aspects: not their transitive closure!
  -- | For a CalculatedRole it is the range of its calculation.
  roleAspectsBindingADT :: r -> MonadPerspectives (ADT EnumeratedRoleType)
  views :: r -> MonadPerspectives (Array ViewType)

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
  context = rangeOfCalculatedRole >=> contextOfADT
  contextOfRepresentation r = (unwrap r).context
  binding = rangeOfCalculatedRole >=> bindingOfADT
  functional r = calculation r >>= pure <<< pessimistic <<< QT.functional
  mandatory r = calculation r >>= pure <<< pessimistic <<< QT.mandatory
  calculation r = case (unwrap r).calculation of
    Q qd -> pure qd
    otherwise -> throwError (error ("Attempt to acces QueryFunctionDescription of a CalculatedRole before the expression has been compiled. This counts as a system programming error." <> (unwrap $ (identifier r :: CalculatedRoleType))))
  properties = rangeOfCalculatedRole >=> propertiesOfADT
  roleADT = rangeOfCalculatedRole
  roleAspectsADT = rangeOfCalculatedRole
  roleAspectsBindingADT = rangeOfCalculatedRole
  views =  rangeOfCalculatedRole >=> viewsOfADT

rangeOfCalculatedRole :: CalculatedRole -> MonadPerspectives (ADT EnumeratedRoleType)
rangeOfCalculatedRole cr = calculation cr >>= roleCalculationRange
  where
    roleCalculationRange :: QueryFunctionDescription -> MonadPerspectives (ADT EnumeratedRoleType)
    roleCalculationRange qfd = case QT.range qfd of
      (RDOM p) -> pure p
      otherwise -> throwError (error ("range of calculation of a calculated role is not a role Domain."))

-----------------------------------------------------------
-- ENUMERATED ROLE INSTANCE
-----------------------------------------------------------
instance enumeratedRoleRoleClass :: RoleClass EnumeratedRole EnumeratedRoleType where
  kindOfRole r = (unwrap r).kindOfRole
  roleAspects r = roleADT r >>= roleAspectsOfADT
  context r = pure $ ST $ contextOfRepresentation r
  contextOfRepresentation r = (unwrap r).context
  binding r = pure (unwrap r).binding
  functional r = pure (unwrap r).functional
  mandatory r = pure (unwrap r).mandatory
  calculation r = pure $ SQD (CDOM $ ST $ contextOfRepresentation r) (RolGetter (ENR (identifier r))) (RDOM (ST (identifier r))) (bool2threeValued (unwrap r).functional) (bool2threeValued (unwrap r).mandatory)
  properties r = roleAspectsBindingADT r >>= propertiesOfADT
  roleADT r = pure (ST $ identifier r)
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
  views r = roleAspectsBindingADT r >>= viewsOfADT

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

--------------------------------------------------------------------------------------------------
---- PROPERTYSET
--------------------------------------------------------------------------------------------------
type PropertySet = ExplicitSet PropertyType

-- | The ADT must be normalised in the sense that no set of terms contains EMPTY or UNIVERSAL.
propertySet :: ADT EnumeratedRoleType -> MP PropertySet
-- propertySet (ST (ENR r)) = getEnumeratedRole r >>= pure <<< PSet <<< _.properties <<< unwrap
-- propertySet (ST (CR r)) = getCalculatedRole r >>= (rangeOfCalculatedRole >=> propertySet)
propertySet (ST r) = getEnumeratedRole r >>= pure <<< PSet <<< _.properties <<< unwrap
propertySet (SUM terms) = traverse propertySet terms >>= pure <<< intersectionPset
propertySet (PROD terms) = traverse propertySet terms >>= pure <<< unionPset
propertySet UNIVERSAL = pure Universal
propertySet EMPTY = pure Empty

--------------------------------------------------------------------------------------------------
---- ROLESET
--------------------------------------------------------------------------------------------------
type RoleSet = ExplicitSet RoleType

-- | The ADT must be normalised in the sense that no set of terms contains EMPTY or UNIVERSAL.
roleSet :: ADT ContextType -> MP RoleSet
roleSet (ST r) = getContext r >>= pure <<< PSet <<< roles
roleSet (SUM terms) = traverse roleSet terms >>= pure <<< intersectionPset
roleSet (PROD terms) = traverse roleSet terms >>= pure <<< unionPset
roleSet UNIVERSAL = pure Universal
roleSet EMPTY = pure Empty

--------------------------------------------------------------------------------------------------
---- ACTIONSET
--------------------------------------------------------------------------------------------------
type ActionSet = ExplicitSet ActionType

-- | The ADT must be normalised in the sense that no set of terms contains EMPTY or UNIVERSAL.
actionSet :: ADT EnumeratedRoleType -> MP ActionSet
actionSet (ST r) = getEnumeratedRole r >>= pure <<< PSet <<< join <<< values <<< _.perspectives <<< unwrap
actionSet (SUM terms) = traverse actionSet terms >>= pure <<< intersectionPset
actionSet (PROD terms) = traverse actionSet terms >>= pure <<< unionPset
actionSet UNIVERSAL = pure Universal
actionSet EMPTY = pure Empty

-----------------------------------------------------------
-- LESSTHANOREQUALTO
-----------------------------------------------------------
-- | `p lessThanOrEqualTo q` means: p is less specific than q, or equal to q.
-- | `p lessThanOrEqualTo q` equals: `q greaterThanOrEqualTo p`
lessThanOrEqualTo :: ADT EnumeratedRoleType -> ADT EnumeratedRoleType -> MP Boolean
lessThanOrEqualTo p q = (&&) <$> (subsetPSet <$> propertySet p <*> propertySet q) <*> (subsetPSet <$> actionSet p <*> actionSet q)

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
contextOfADT (ST et) = (getEnumeratedRole >=> context) et
contextOfADT (SUM adts) = map sum (traverse contextOfADT adts)
contextOfADT (PROD adts) = map product (traverse contextOfADT adts)
contextOfADT EMPTY = pure EMPTY
contextOfADT UNIVERSAL = pure UNIVERSAL

-----------------------------------------------------------
-- EXTERNALROLEOFADT
-----------------------------------------------------------
-- | The external role of an ADT ContextType
externalRoleOfADT :: ADT ContextType -> MP (ADT EnumeratedRoleType)
-- TODO: handle CalculatedRole.
externalRoleOfADT (ST ct) = getContext ct >>= pure <<< ST <<< externalRole
externalRoleOfADT (SUM adts) = map sum (traverse externalRoleOfADT adts)
externalRoleOfADT (PROD adts) = map product (traverse externalRoleOfADT adts)
externalRoleOfADT EMPTY = pure EMPTY
externalRoleOfADT UNIVERSAL = pure UNIVERSAL

-----------------------------------------------------------
-- BINDINGOFADT
-----------------------------------------------------------
-- | The binding of an ADT.
bindingOfADT :: ADT EnumeratedRoleType -> MP (ADT EnumeratedRoleType)
-- TODO: handle CalculatedRole.
bindingOfADT (ST a) = getEnumeratedRole a >>= binding
bindingOfADT (SUM adts) = map sum (traverse bindingOfADT adts)
bindingOfADT (PROD adts) = map product (traverse bindingOfADT adts)
bindingOfADT EMPTY = pure EMPTY
bindingOfADT a@UNIVERSAL = throwError (error $ show (RoleCannotHaveBinding upperLeft upperLeft (show a)))
-----------------------------------------------------------
-- PROPERTIESOFADT
-----------------------------------------------------------
-- | Properties of a role's binding count as properties of the role itself.
propertiesOfADT :: ADT EnumeratedRoleType -> MP (Array PropertyType)
propertiesOfADT adt = propertySet adt >>= case _ of
    PSet ps -> pure ps
    Empty -> pure []
    Universal -> throwError (error $ show UniversalRoleHasNoParts)

-----------------------------------------------------------
-- VIEWSOFADT
-----------------------------------------------------------
-- | The ADT must be normalised in the sense that no set of terms contains EMPTY or UNIVERSAL.
viewsOfADT :: ADT EnumeratedRoleType -> MP (Array ViewType)
-- viewsOfADT (ST (ENR r)) = getEnumeratedRole r >>= pure <<< _.roleAspects <<< unwrap
-- viewsOfADT (ST (CR r)) = getCalculatedRole r >>= (rangeOfCalculatedRole >=> roleAspectsOfADT)
viewsOfADT (ST r) = getEnumeratedRole r >>= pure <<< _.views <<< unwrap
viewsOfADT adt@(SUM terms) = do
  (allProps :: Set PropertyType) <- propertySet adt >>= pure <<< fromFoldable <<< unsafePartial elements
  (allViews :: Array ViewType) <- traverse viewsOfADT terms >>= pure <<< unionOfArrays
  filterA (propertiesOfView >=> pure <<< (flip subset allProps)) allViews
  where
    propertiesOfView :: ViewType -> MP (Set PropertyType)
    propertiesOfView = (getPerspectType >=> pure <<< fromFoldable <<< propertyReferences)
viewsOfADT (PROD terms) = traverse viewsOfADT terms >>= pure <<< unionOfArrays
viewsOfADT EMPTY = pure []
viewsOfADT UNIVERSAL = throwError (error $ show UniversalRoleHasNoParts)

-----------------------------------------------------------
-- ROLEASPECTSOFADT
-----------------------------------------------------------
-- | The ADT must be normalised in the sense that no set of terms contains EMPTY or UNIVERSAL.
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

adtOfRole :: Role -> MP (ADT EnumeratedRoleType)
adtOfRole (E e) = roleAspectsBindingADT e
adtOfRole (C c) = roleAspectsBindingADT c

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

contextOfRole :: RoleType -> MonadPerspectives (ADT ContextType)
contextOfRole (ENR e) = getPerspectType e >>= context
contextOfRole (CR c) = getPerspectType c >>= context

-----------------------------------------------------------
-- FUNCTIONS ON STRING
-----------------------------------------------------------
getRoleType :: String -> MonadPerspectives RoleType
getRoleType s = ((getEnumeratedRole $ EnumeratedRoleType s) >>= pure <<< ENR <<< identifier)
  <|> ((getCalculatedRole $ CalculatedRoleType s) >>= pure <<< CR <<< identifier)
