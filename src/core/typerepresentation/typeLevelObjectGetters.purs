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

module Perspectives.Types.ObjectGetters where

import Control.Monad.Error.Class (try)
import Control.Monad.State (StateT, execStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Control.Plus (map, (<|>), empty)
import Data.Array (concat, cons, elemIndex, filter, findIndex, foldl, foldr, fromFoldable, null, singleton, filterA)
import Data.FoldableWithIndex (foldWithIndexM)
import Data.List (foldl) as LIST
import Data.Map (Map, empty, lookup, insert, keys, unionWith, values) as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)
import Data.Set (subset, fromFoldable) as SET
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (for_, traverse)
import Foreign.Object (Object, keys, lookup, union) as OBJ
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~>), type (~~~>), MonadPerspectives, MP, (###=), (###>>))
import Perspectives.Data.EncodableMap (EncodableMap(..))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Error.Boundaries (handleDomeinFileError')
import Perspectives.Identifiers (areLastSegmentsOf, deconstructModelName, endsWithSegments, isExternalRole, startsWithSegments)
import Perspectives.Instances.Combinators (closure_, conjunction, filter', some)
import Perspectives.Instances.Combinators (filter', filter) as COMB
import Perspectives.Query.QueryTypes (QueryFunctionDescription, RoleInContext(..), domain2roleType, queryFunction, range, roleInContext2Role, roleRange, secondOperand)
import Perspectives.Representation.ADT (ADT(..), allLeavesInADT, equalsOrSpecialisesADT, reduce)
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.Class.Context (contextADT, contextRole, roleInContext, userRole) as ContextClass
import Perspectives.Representation.Class.Context (contextAspects)
import Perspectives.Representation.Class.PersistentType (getCalculatedRole, getContext, getEnumeratedRole, getPerspectType, getView, tryGetState)
import Perspectives.Representation.Class.Role (actionsOfRoleType, adtOfRole, adtOfRoleAspectsBinding, allProperties, allRoles, allViews, calculation, getRole, perspectives, perspectivesOfRoleType, roleADT, roleAspects, typeIncludingAspects)
import Perspectives.Representation.Context (Context)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.InstanceIdentifiers (Value(..))
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..), StateSpec, objectOfPerspective, perspectiveSupportsOneOfRoleVerbs, perspectiveSupportsProperty, stateSpec2StateIdentifier)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType, EnumeratedRoleType(..), PropertyType(..), RoleType(..), StateIdentifier(..), ViewType, propertytype2string, roletype2string)
import Perspectives.Representation.Verbs (PropertyVerb, RoleVerb)
import Perspectives.Representation.View (propertyReferences)
import Prelude (Unit, append, bind, flip, not, pure, show, unit, ($), (&&), (*>), (/=), (<$>), (<<<), (<>), (==), (>=>), (>>=), (>>>), (||))

----------------------------------------------------------------------------------------
------- FUNCTIONS ON ENUMERATEDROLETYPES
----------------------------------------------------------------------------------------
isUnlinked_ :: EnumeratedRoleType -> MonadPerspectives Boolean
isUnlinked_ et = getEnumeratedRole et >>= pure <<< _.unlinked <<< unwrap

isUnlinked :: EnumeratedRoleType ~~~> Boolean
isUnlinked = lift <<< isUnlinked_

-- | The state identifier of the root state of a role has the same string value as the role identifier.
roleRootState :: EnumeratedRoleType ~~~> StateIdentifier
roleRootState rtype = pure $ StateIdentifier $ unwrap rtype

-- | The transitive closure over Aspects of roleRootState.
roleRootStates :: EnumeratedRoleType ~~~> StateIdentifier
roleRootStates = roleAspectsClosure >=> roleRootState

-- | The locally defined properties (i.e. excluding Aspect properties).
enumeratedRolePropertyTypes_ :: EnumeratedRoleType -> MonadPerspectives (Array PropertyType)
enumeratedRolePropertyTypes_ = getEnumeratedRole >=> \(EnumeratedRole{properties}) -> pure properties

enumeratedRoleContextType :: EnumeratedRoleType -> MonadPerspectives ContextType
enumeratedRoleContextType = getEnumeratedRole >=> pure <<< _.context <<< unwrap

----------------------------------------------------------------------------------------
------- FUNCTIONS TO FIND A ROLETYPE WORKING FROM STRINGS OR ADT'S
----------------------------------------------------------------------------------------
string2RoleType :: String -> MonadPerspectives RoleType
string2RoleType qualifiedRoleName = getEnumeratedRole (EnumeratedRoleType qualifiedRoleName) *> pure (ENR $ EnumeratedRoleType qualifiedRoleName) <|> pure (CR $ CalculatedRoleType qualifiedRoleName)

-- | If a role with the given qualified name is available in the Context or its (in)direct aspects,
-- | return it as a RoleType. From the type we can find out its RoleKind, too.
lookForRoleType :: String -> (ContextType ~~~> RoleType)
lookForRoleType s c = (lift $ getContext c) >>= pure <<< ContextClass.contextADT >>= lookForRoleTypeOfADT s

lookForRoleTypeOfADT :: String -> (ADT ContextType ~~~> RoleType)
lookForRoleTypeOfADT s = lookForRoleOfADT (roletype2string >>> ((==) s)) s

-- | As lookForRoleType, but then with a local name that should string-match the end of the qualified name.
lookForUnqualifiedRoleType :: String -> ContextType ~~~> RoleType
lookForUnqualifiedRoleType s c = (lift $ getContext c) >>= pure <<< ContextClass.contextADT >>= lookForUnqualifiedRoleTypeOfADT s

-- | We simply require the Pattern to match the end of the string.
lookForUnqualifiedRoleTypeOfADT :: String -> ADT ContextType ~~~> RoleType
lookForUnqualifiedRoleTypeOfADT s = lookForRoleOfADT (roletype2string >>> areLastSegmentsOf s) s

-- | Constructs the set of all roles in the ADT (recursing on aspects of Enumerated roles)
-- | and then applies a comparison function to the name passed in and the names of all those roles.
lookForRoleOfADT :: (RoleType -> Boolean) -> String -> ADT ContextType ~~~> RoleType
lookForRoleOfADT criterium _ adt =  ArrayT (allRoles adt >>= pure <<< filter criterium)

----------------------------------------------------------------------------------------
------- FUNCTIONS OPERATING DIRECTLY ON CONTEXT TYPE
----------------------------------------------------------------------------------------
roleInContext :: ContextType ~~~> RoleType
roleInContext = ArrayT <<< ((getPerspectType :: ContextType -> MonadPerspectives Context) >=> pure <<< ContextClass.roleInContext)

contextRole :: ContextType ~~~> RoleType
contextRole = ArrayT <<< ((getPerspectType :: ContextType -> MonadPerspectives Context) >=> pure <<< ContextClass.contextRole)

userRole :: ContextType ~~~> RoleType
userRole = ArrayT <<< ((getPerspectType :: ContextType -> MonadPerspectives Context) >=> pure <<< ContextClass.userRole)

-- | Returns User RoleTypes that are guaranteed to be Enumerated.
enumeratedUserRole :: ContextType ~~~> RoleType
enumeratedUserRole =  ArrayT <<< ((getPerspectType :: ContextType -> MonadPerspectives Context) >=> pure <<< filter isEnumerated <<< ContextClass.userRole)
  where
    isEnumerated :: RoleType -> Boolean
    isEnumerated (ENR _) = true
    isEnumerated (CR _) = false

-- | Returns User RoleTypes that are guaranteed to be Calculated.
calculatedUserRole :: ContextType ~~~> RoleType
calculatedUserRole =  ArrayT <<< ((getPerspectType :: ContextType -> MonadPerspectives Context) >=> pure <<< filter isCalculated <<< ContextClass.userRole)
  where
    isCalculated :: RoleType -> Boolean
    isCalculated (ENR _) = false
    isCalculated (CR _) = true

-- | Returns all Enumerated role types in the context
allEnumeratedRoles :: ContextType ~~~> EnumeratedRoleType
allEnumeratedRoles ct = ArrayT do
  rs <- runArrayT $ allRoleTypesInContext ct
  pure $ foldl f [] rs
  where
    f :: Array EnumeratedRoleType -> RoleType -> Array EnumeratedRoleType
    f roles (ENR r) = cons r roles
    f roles _ = roles

allUnlinkedRoles :: ContextType ~~~> EnumeratedRoleType
allUnlinkedRoles = COMB.filter allEnumeratedRoles isUnlinked

allRoleTypesInContext :: ContextType ~~~> RoleType
allRoleTypesInContext = conjunction roleInContext $ conjunction contextRole userRole'
  where
    userRole' :: ContextType ~~~> RoleType
    userRole' = ArrayT <<< ((getPerspectType :: ContextType -> MonadPerspectives Context) >=> pure <<< ContextClass.userRole)

aspectRoles :: ContextType ~~~> RoleType
aspectRoles ct@(ContextType contextName) = filter' allRoleTypesInContext 
  (\rt -> case rt of
    (ENR (EnumeratedRoleType roleName)) -> not (roleName `startsWithSegments` contextName)
    _ -> true)
  ct

-- | Returns the name of the model that defines the role type as a String Value.
contextTypeModelName :: ContextType ~~~> Value
contextTypeModelName (ContextType rid) = maybe empty (pure <<< Value) (deconstructModelName rid)

contextTypeModelName' :: ContextType ~~> Value
contextTypeModelName' (ContextType rid) = maybe empty (pure <<< Value) (deconstructModelName rid)

-- | The state identifier of the root state of a role has the same string value as the role identifier.
contextRootState :: ContextType ~~~> StateIdentifier
contextRootState ctype = pure $ StateIdentifier $ unwrap ctype

-- | The transitive closure over Aspects of rootState.
contextRootStates :: ContextType ~~~> StateIdentifier
contextRootStates = contextAspectsClosure >=> contextRootState

----------------------------------------------------------------------------------------
------- FUNCTIONS OPERATING DIRECTLY ON STATE
----------------------------------------------------------------------------------------
subStates_ :: StateIdentifier -> MonadPerspectives (Array StateIdentifier)
subStates_ = tryGetState >=> pure <<< (maybe [] _.subStates <<< map unwrap)

----------------------------------------------------------------------------------------
------- FUNCTIONS TO FIND A PROPERTYTYPE WORKING FROM STRINGS OR ADT'S
----------------------------------------------------------------------------------------
-- | Look for a Property on a given EnumeratedRoleType (including its own aspects - not
-- | recursively - and its own binding - not recursively), using a criterium.
lookForPropertyType_ :: String -> (EnumeratedRoleType ~~~> PropertyType)
lookForPropertyType_ s i = (lift $ getRole (ENR i)) >>= lift <<< adtOfRoleAspectsBinding >>= lookForProperty (propertytype2string >>> ((==) s)) <<< map roleInContext2Role

-- | Look for an unqualified Property on a given RoleType
-- | (recursing on aspects and on the binding of Enumerated Roles), using a criterium.
lookForUnqualifiedPropertyType_ :: String -> (RoleType ~~~> PropertyType)
lookForUnqualifiedPropertyType_ s (ENR i) = lookForProperty (propertytype2string >>> areLastSegmentsOf s) (ST i)
lookForUnqualifiedPropertyType_ s (CR i) = (lift $ getCalculatedRole i) >>= lift <<< roleADT >>= lookForProperty (propertytype2string >>> areLastSegmentsOf s) <<< map roleInContext2Role

-- | Look for a Property on a given ADT, using a qualified name (recursing on aspects
-- | and on the binding of Enumerated Roles).
-- | Note: use this function to check that the property is actually defined.
lookForPropertyType :: String -> (ADT EnumeratedRoleType ~~~> PropertyType)
lookForPropertyType s = lookForProperty (propertytype2string >>> ((==) s))

-- | Look for a Property on a given ADT (recursing on aspects and on the binding of
-- | Enumerated Roles) using the postfix of a name.
lookForUnqualifiedPropertyType :: String -> (ADT EnumeratedRoleType ~~~> PropertyType)
lookForUnqualifiedPropertyType s = lookForProperty (propertytype2string >>> areLastSegmentsOf s)

-- | Look for a Property on a given ADT (recursing on aspects and on the binding of
-- | Enumerated Roles), using a criterium.
lookForProperty :: (PropertyType -> Boolean) -> ADT EnumeratedRoleType ~~~> PropertyType
lookForProperty criterium adt = ArrayT (allProperties adt >>= pure <<< filter criterium)
-- lookForProperty criterium = COMB.filter' (ArrayT <<< allProperties) criterium

-- | All properties, computed recursively over binding and Aspects, of the Role.
propertiesOfRole :: String ~~~> PropertyType
propertiesOfRole s =
  ArrayT (getPerspectType (EnumeratedRoleType s) >>= roleADT >>= allProperties <<< map roleInContext2Role)
  <|>
  ArrayT (getPerspectType (CalculatedRoleType s) >>= roleADT >>= allProperties <<< map roleInContext2Role)

----------------------------------------------------------------------------------------
------- FUNCTIONS FOR ASPECTS
----------------------------------------------------------------------------------------
aspectsOfRole :: EnumeratedRoleType ~~~> EnumeratedRoleType
aspectsOfRole = ArrayT <<< (getPerspectType >=> map (map roleInContext2Role) <<< roleAspects)

-- | All types of the role, including the root type itself.
roleAspectsClosure :: EnumeratedRoleType ~~~> EnumeratedRoleType
roleAspectsClosure = closure_ aspectsOfRole

aspectsOfContext :: ContextType ~~~> ContextType
aspectsOfContext = ArrayT <<< (getPerspectType >=> pure <<< contextAspects)

contextAspectsClosure :: ContextType ~~~> ContextType
contextAspectsClosure = closure_ aspectsOfContext

roleTypeAspectsClosure :: RoleType ~~~> RoleType
roleTypeAspectsClosure (ENR r) = ENR <$> roleAspectsClosure r
roleTypeAspectsClosure (CR r) = pure $ CR r

-- aspect `hasAspect` roleType
-- roleType ##>>> hasAspect aspect
hasAspect :: EnumeratedRoleType -> (EnumeratedRoleType ~~~> Boolean)
hasAspect aspect roleType = ArrayT do
  aspects <- roleType ###= roleAspectsClosure
  pure [isJust $ findIndex ((==) aspect) aspects]

-- aspect `hasContextAspect` contextType
-- roleType ##>>> hasContextAspect aspect
hasContextAspect :: ContextType -> (ContextType ~~~> Boolean)
hasContextAspect aspect contextType = ArrayT do
  aspects <- contextType ###= contextAspectsClosure
  pure [isJust $ findIndex ((==) aspect) aspects]

hasAspectWithLocalName :: String -> (EnumeratedRoleType ~~~> Boolean)
hasAspectWithLocalName localAspectName roleType = ArrayT do
  aspects <- roleType ###= roleAspectsClosure
  pure [isJust $ findIndex (test (unsafeRegex (localAspectName <> "$") noFlags)) (unwrap <$> aspects)]

----------------------------------------------------------------------------------------
------- GET A PERSPECTIVE
----------------------------------------------------------------------------------------
perspectivesOfRole :: EnumeratedRoleType ~~~> Perspective
perspectivesOfRole rt = ArrayT (getEnumeratedRole rt >>= unwrap >>> _.perspectives >>> pure)

-- | <UserRole> `perspectiveOnADT` <ADT> gives the perspectives of the
-- | UserRole on the role represented by the ADT.
-- | PARTIAL: can only be used after object of Perspective has been compiled in PhaseThree.
perspectiveOnADT :: Partial => RoleType -> (ADT RoleInContext) ~~~> Perspective
perspectiveOnADT userRoleType adt = ArrayT do
  (ps :: Array Perspective) <- perspectivesOfRoleType userRoleType
  filterA (flip isPerspectiveOnADT adt) ps

-- | <UserRole> `perspectiveOnRoleType` <ObjectRole> gives the perspectives (at most one)
-- | of the UserRole on the ObjectRole.
-- | PARTIAL: can only be used after object of Perspective has been compiled in PhaseThree.
perspectiveOnRoleType :: Partial => RoleType -> RoleType ~~~> Perspective
perspectiveOnRoleType userRoleType objectRoleType = (lift $ getRole objectRoleType) >>= lift <<< adtOfRole >>= (perspectiveOnADT userRoleType)

-- | For the user role (second argument), and the object role (first argument), compute
-- | for each verb in the perspective the former has on the latter, the properties.
-- | Results are computed for the user role and its Aspects.
-- | Notice that only RelevantProperties are returned. In case of All, this symbolically
-- | also represents properties of Aspects and the binding hierarchy, but does not make them explicit.
-- | PARTIAL: can only be used after object of Perspective has been compiled in PhaseThree.
-- relevantPropertiesForObjectRole :: Partial => RoleType -> RoleType -> MonadPerspectives RelevantProperties
-- relevantPropertiesForObjectRole objectRole userRole' = do
--   (perspectives :: Array Perspective) <- objectRole ###= perspectiveOnRoleType userRole'
--   pure $ fold (concat (e <$> perspectives))
--     where
--       e :: Perspective -> Array RelevantProperties
--       e (Perspective{propertyVerbs}) = let
--         (lpvs :: Array PropertyVerbs) = concat $ fromFoldable (Map.values (unwrap propertyVerbs))
--         in concat (map f lpvs)
--       f :: PropertyVerbs -> ExplicitSet RelevantProperties
--       f (PropertyVerbs pset verbs) = map
--         (\verb -> let
--           (props :: RelevantProperties) = case pset of
--             Universal -> All
--             Empty -> Properties []
--             PSet ps -> Properties ps
--           in props)
--         (verbs :: ExplicitSet PropertyVerb)

----------------------------------------------------------------------------------------
------- FUNCTIONS ON ROLETYPES
----------------------------------------------------------------------------------------
roleTypeModelName :: RoleType ~~~> Value
roleTypeModelName rt = maybe empty (pure <<< Value) (deconstructModelName (roletype2string rt))

roleTypeModelName' :: RoleType ~~> Value
roleTypeModelName' rt = maybe empty (pure <<< Value) (deconstructModelName (roletype2string rt))

-- | For a given context type, find the locally defined Enumerated role type that has an aspect role
-- | whose local name is the first parameter value.
localRoleSpecialisation :: String -> ContextType ~~~> EnumeratedRoleType
localRoleSpecialisation localAspectName = COMB.filter allEnumeratedRoles (hasAspectWithLocalName localAspectName)

-- | Tests whether we have a sequence of which the last part applies an ExternalCoreRoleGetter function.
-- | Returns `false` if the type cannot be found.
isDatabaseQueryRole :: RoleType -> MonadPerspectives Boolean
isDatabaseQueryRole (ENR _) = pure false
isDatabaseQueryRole (CR cr) = do
  calculatedRole <- getPerspectType cr
  qfd <- calculation calculatedRole
  computesDatabaseQueryRole qfd

-- | Tests whether we have a sequence of which the last part applies an ExternalCoreRoleGetter function.
-- | Returns `false` if the type cannot be found.
computesDatabaseQueryRole :: QueryFunctionDescription -> MonadPerspectives Boolean
computesDatabaseQueryRole qfd = do
  case queryFunction qfd of
    (BinaryCombinator SequenceF) -> case queryFunction <$> secondOperand qfd of
      Just (ExternalCoreRoleGetter _) -> isExternal (unsafePartial domain2roleType $ range qfd)
      _ -> pure false
    _ -> pure false
  where
    isExternal :: ADT RoleInContext -> MonadPerspectives Boolean
    isExternal = reduce \(RoleInContext{role}) -> pure $ isExternalRole $ unwrap role

--------------------------------------------------------------------------------------------------
---- COMMONTYPESINADT
--------------------------------------------------------------------------------------------------
-- | Compares with commonLeavesInADT (module Perspectives.Representation.ADT).
-- | That function treats each leaf as atomic; here we expand it to the transitive closure of its aspects.
-- | In terms of sets: transform the ADT to Disjunctive Normal Form, expand each type with its aspects
-- |  and then construct the intersection of unions.
commonTypesInRoleADT :: ADT EnumeratedRoleType ~~~> EnumeratedRoleType
-- commonTypesInADT = unwrap <<< reduce ((pure <<< singleton) :: a -> Identity (Array a))
commonTypesInRoleADT = ArrayT <$> reduce (runArrayT <<< roleAspectsClosure)

--------------------------------------------------------------------------------------------------
---- ALLTYPESINADT
--------------------------------------------------------------------------------------------------
-- | Compares with allLeavesInADT (module Perspectives.Representation.ADT).
-- | That function treats each leaf as atomic; here we expand it to the transitive closure of its aspects.
-- | In terms of sets: transform the ADT to Disjunctive Normal Form and then understand it
-- | as an union of unions.
allTypesInRoleADT :: ADT EnumeratedRoleType ~~~> EnumeratedRoleType
allTypesInRoleADT = ArrayT <<< pure <<< allLeavesInADT >=> roleAspectsClosure

--------------------------------------------------------------------------------------------------
---- EQUALSORGENERALISESROLEADT, EQUALSORSPECIALISESROLEADT, GENERALISESROLEADT, SPECIALISEDROLEADT,
---- SPECIALISESROLETYPE, SPECIALISESROLETYPE_
--------------------------------------------------------------------------------------------------
-- | a1 `equalsOrGeneralisesRoleADT` a2
-- | intuitively when a2 is built from a1 (or a2 == a1).
-- | Compares with equalsOrGeneralisesADT (module Perspectives.Representation.ADT)
-- | However, that function works for any a (Ord a, Eq a).
-- | This function works for EnumeratedRoleTypes and takes Aspects into account.
-- | See: Semantics of the Perspectives Language, chapter Another ordering of Role types for an explanation.
equalsOrGeneralisesRoleADT :: ADT EnumeratedRoleType -> ADT EnumeratedRoleType -> MP Boolean
equalsOrGeneralisesRoleADT adt1 adt2 = do
  union' <- runArrayT $ allTypesInRoleADT adt1
  intersection' <- runArrayT $ commonTypesInRoleADT adt2
  pure $ SET.subset (SET.fromFoldable union') (SET.fromFoldable intersection')

generalisesRoleADT :: ADT EnumeratedRoleType -> ADT EnumeratedRoleType -> MP Boolean
generalisesRoleADT adt1 adt2 = do
  ne <- pure (adt1 /= adt2)
  g <- adt1 `equalsOrGeneralisesRoleADT` adt2
  pure $ ne && g

specialisesRoleADT :: ADT EnumeratedRoleType -> ADT EnumeratedRoleType -> MP Boolean
specialisesRoleADT = flip generalisesRoleADT

equalsOrSpecialisesRoleADT :: ADT EnumeratedRoleType -> ADT EnumeratedRoleType -> MP Boolean
equalsOrSpecialisesRoleADT = flip equalsOrGeneralisesRoleADT

-- | R1 `specialisesRoleType` R2 is true, iff R2 is an (indirect) Aspect of R1 or if both are equal.
-- | We want to use this function as the computation behind the query step `specialisesRoleType`:
-- |  filter <expression that yields role types> with specialisesRoleType SomeRole
-- | The result must be all role types that, indeed, specialise SomeRole.
-- | hence in the QueryCompiler, we flip `specialisesRoleType`, so we can apply it to SomeRole first
-- | and it is still bound to the second parameter.
-- | This function yields a single result (it is functional in PL)
specialisesRoleType :: RoleType -> (RoleType ~~~> Value)
specialisesRoleType t1 t2 = ArrayT do 
  x <- (t1 `specialisesRoleType_` t2)
  pure [Value $ show x]

specialisesRoleType_ :: RoleType -> (RoleType -> MonadPerspectives Boolean)
specialisesRoleType_ t1 t2 = do
  t1' <- typeIncludingAspects t1
  t2' <- typeIncludingAspects t2
  (roleInContext2Role <$> t1') `specialisesRoleADT` (roleInContext2Role <$> t2')

-----------------------------------------------------------
---- LESSTHANOREQUALTO, GREATERTHENOREQUALTO
-----------------------------------------------------------
-- | `p lessThanOrEqualTo q` means: p is less specific than q, or equal to q.
-- | `p lessThanOrEqualTo q` equals: `q greaterThanOrEqualTo p`
lessThanOrEqualTo :: ADT RoleInContext -> ADT RoleInContext -> MP Boolean
lessThanOrEqualTo p q = (roleInContext2Role <$> p) `equalsOrGeneralisesRoleADT` (roleInContext2Role <$> q)

-- | `q greaterThanOrEqualTo p` means: q is more specific than p, or equal to p
-- | If you use `less specific` instead of `more specific`, flip the arguments.
-- | If you use `more general` instead of `more specific`, flip them, too.
-- | So `less specific` instead of `more general` means flipping twice and is a no-op.
-- | Therefore `less specific` equals `more general`.
greaterThanOrEqualTo :: ADT RoleInContext -> ADT RoleInContext -> MP Boolean
greaterThanOrEqualTo = flip lessThanOrEqualTo

-----------------------------------------------------------
---- ISPERSPECTIVEONADT
-----------------------------------------------------------
-- | The object of the perspective must be equal to or be a generalisation of the given ADT,
-- | for the perspective to apply to the ADT.
-- | <perspective> `isPerspectiveOnADT` <adt>
-- | PARTIAL: can only be used after object of Perspective has been compiled in PhaseThree.
isPerspectiveOnADT :: Partial => Perspective -> ADT RoleInContext -> MP Boolean
isPerspectiveOnADT p adt = (roleInContext2Role <$> objectOfPerspective p) `equalsOrGeneralisesRoleADT` (roleInContext2Role <$> adt)


----------------------------------------------------------------------------------------
------- FUNCTIONS TO FIND VIEWS AND ON VIEWS
----------------------------------------------------------------------------------------
propertiesOfView :: ViewType ~~~> PropertyType
propertiesOfView = ArrayT <<< (getPerspectType >=> pure <<< propertyReferences)

lookForUnqualifiedViewType :: String -> (RoleType ~~~> ViewType)
lookForUnqualifiedViewType s = lookForView (unwrap >>> areLastSegmentsOf s)

lookForView :: (ViewType -> Boolean) -> RoleType ~~~> ViewType
lookForView criterium = COMB.filter' viewsOfRoleType criterium

-- | If no view is specified, all properties can be accessed.
hasProperty :: PropertyType -> ViewType ~~~> Boolean
hasProperty p = lift <<< getView >=> pure <<< isJust <<< elemIndex p <<< propertyReferences

-- | All views of either a CalculatedRole or an EnumeratedRole. This includes Views defined
-- | locally on a CalculatedRole.
viewsOfRoleType :: RoleType ~~~> ViewType
viewsOfRoleType (ENR rt) = ArrayT (getEnumeratedRole rt >>= roleADT >>= allViews <<< map roleInContext2Role)
viewsOfRoleType (CR rt) = ArrayT do
  localViews <- getCalculatedRole rt >>= pure <<< _.views <<< unwrap
  otherViews <- getCalculatedRole rt >>= roleADT >>= allViews <<< map roleInContext2Role
  pure (localViews <> otherViews)

----------------------------------------------------------------------------------------
------- FUNCTIONS TO QUALIFY ROLES AND CONTEXTS
----------------------------------------------------------------------------------------
qualifyRoleInDomain :: String -> String ~~~> RoleType
qualifyRoleInDomain localName namespace = ArrayT do
  (try $ retrieveDomeinFile namespace) >>=
    handleDomeinFileError' "qualifyRoleInDomain" []
      \(DomeinFile {calculatedRoles, enumeratedRoles}) -> do
        eCandidates <- pure $ map (ENR <<< EnumeratedRoleType) (filter (\_id -> _id `endsWithSegments` localName) (OBJ.keys enumeratedRoles))
        cCandidates <- pure $ map (CR <<< CalculatedRoleType) (filter (\_id -> _id `endsWithSegments` localName) (OBJ.keys calculatedRoles))
        pure $ eCandidates <> cCandidates

qualifyEnumeratedRoleInDomain :: String -> String ~~~> EnumeratedRoleType
qualifyEnumeratedRoleInDomain localName namespace = ArrayT do
  (try $ retrieveDomeinFile namespace) >>=
    handleDomeinFileError' "qualifyEnumeratedRoleInDomain" []
      \(DomeinFile {enumeratedRoles}) -> pure $ map EnumeratedRoleType (filter (\_id -> _id `endsWithSegments` localName) (OBJ.keys enumeratedRoles))

qualifyCalculatedRoleInDomain :: String -> String ~~~> CalculatedRoleType
qualifyCalculatedRoleInDomain localName namespace = ArrayT do
  (try $ retrieveDomeinFile namespace) >>=
    handleDomeinFileError' "qualifyCalculatedRoleInDomain" []
    \(DomeinFile {calculatedRoles}) -> pure $ map CalculatedRoleType (filter (\_id -> _id `endsWithSegments` localName) (OBJ.keys calculatedRoles))

qualifyContextInDomain :: String -> String ~~~> ContextType
qualifyContextInDomain localName namespace = ArrayT do
  (try $ retrieveDomeinFile namespace) >>=
    handleDomeinFileError' "qualifyContextInDomain" []
      \(DomeinFile {contexts}) -> pure $ map ContextType (filter (\_id -> _id `endsWithSegments` localName) (OBJ.keys contexts))

----------------------------------------------------------------------------------------
------- USER ROLETYPES WITH A PERSPECTIVE ON A ROLETYPE
----------------------------------------------------------------------------------------
-- | <user RoleType> `hasPerspectiveOnRole` <RoleType>
-- | True, iff the user RoleType or one of its aspects has a perspective on the role type.
-- | PARTIAL: can only be used after object of Perspective has been compiled in PhaseThree.
hasPerspectiveOnRole :: Partial => RoleType -> EnumeratedRoleType ~~~> Boolean
-- hasPerspectiveOnRole ur rt =
hasPerspectiveOnRole ur rt = ArrayT ((hasPerspectiveWithVerb ur rt []) >>= pure <<< singleton)

-- | <RoleType> `roleIsInPerspectiveOf` <userRole>
-- | PARTIAL: can only be used after object of Perspective has been compiled in PhaseThree.
roleIsInPerspectiveOf :: Partial => EnumeratedRoleType -> RoleType ~~~> Boolean
roleIsInPerspectiveOf = flip hasPerspectiveOnRole

-- | PARTIAL: can only be used after object of Perspective has been compiled in PhaseThree.
-- | True iff the user RoleType has a perspective whose object includes the EnumeratedRoleType and one of the verbs
-- | (unless no verbs are specified).
hasPerspectiveOnRoleWithVerbs :: Partial => Array RoleVerb -> RoleType -> EnumeratedRoleType ~~~> Boolean
hasPerspectiveOnRoleWithVerbs verbs ur rt = ArrayT ((hasPerspectiveWithVerb ur rt verbs) >>= pure <<< singleton)

-- | True if the user role (subjectType (first) argument) has a perspective
-- | on the object (roleType (second) argument) that includes one of the given RoleVerbs,
-- | (unless no verbs are specified),
-- | OR if the role type has the aspect "sys:RootContext$RootUser"
-- | PARTIAL: can only be used after object of Perspective has been compiled in PhaseThree.
hasPerspectiveWithVerb :: Partial => RoleType -> EnumeratedRoleType -> Array RoleVerb -> MonadPerspectives Boolean
hasPerspectiveWithVerb subjectType roleType verbs = do
  objectIsRootUser <- roleType ###>> hasAspect (EnumeratedRoleType "model:System$RootContext$RootUser")
  if objectIsRootUser
    then pure true
    else do
      -- Find for the subject (including its aspects) a perspective
      --    * whose object adt equals the adt of `roleType` or is a generalisation of it, AND
      --    * that supports at least one of the requested RoleVerbs.
      -- (allObjects :: Array EnumeratedRoleType) <- roleType ###= roleAspectsClosure
      adtOfRoleType <- getEnumeratedRole roleType >>= roleADT
      isJust <$> findPerspective subjectType
        \perspective -> do
          p <- perspective `isPerspectiveOnADT` adtOfRoleType
          pure (p && (null verbs || perspectiveSupportsOneOfRoleVerbs perspective verbs))

----------------------------------------------------------------------------------------
------- USER ROLETYPES WITH A PERSPECTIVE ON A PROPERTYTYPE
----------------------------------------------------------------------------------------
hasPerspectiveOnProperty :: RoleType -> EnumeratedPropertyType ~~~> Boolean
hasPerspectiveOnProperty subjectType property = ArrayT do
  singleton <<< isJust <$> findPerspective
    subjectType
    \perspective -> pure $ perspectiveSupportsProperty perspective (ENP property)

-- | <RoleType> `propertyIsInPerspectiveOf` <userRole>
propertyIsInPerspectiveOf :: EnumeratedPropertyType -> RoleType ~~~> Boolean
propertyIsInPerspectiveOf = flip hasPerspectiveOnProperty

----------------------------------------------------------------------------------------
------- FIND A SPECIFIC PERSPECTIVE
----------------------------------------------------------------------------------------
-- | Returns just the first perspective for the given (user) RoleType (including its
-- | aspects in the search) that complies with the criterium, or Nothing.
findPerspective :: RoleType -> (Perspective -> MonadPerspectives Boolean) -> MonadPerspectives (Maybe Perspective)
findPerspective subjectType criterium = execStateT f Nothing
  where
    f :: StateT (Maybe Perspective) MonadPerspectives Unit
    f = do
      allSubjects <- lift (subjectType ###= roleTypeAspectsClosure)
      for_ allSubjects
        \userRole' -> hasBeenFound >>= if _
          then pure unit
          else do
            -- Search in the perspectives of userRole'
            perspectives <- lift $ perspectivesOfRoleType userRole'
            for_ perspectives
              \perspective -> (lift $ criterium perspective) >>= if _
                then put (Just perspective)
                else pure unit
            -- for_ perspectives
            --   \perspective -> if criterium perspective
            --     then put (Just perspective)
            --     else pure unit
    hasBeenFound :: StateT (Maybe Perspective) MonadPerspectives Boolean
    hasBeenFound = get >>= pure <<< isJust

-- | True iff the subject type has a perspective that includes the property,
-- | qualified with the given PropertyVerb.

-- | PARTIAL: can only be used after object of Perspective has been compiled in PhaseThree.
-- | This function tests whether a user RoleType has a perspective on an object that carries the requested PropertyType.
-- TODO. #10 hasPerspectiveOnPropertyWithVerb should check the PropertyVerbs.
hasPerspectiveOnPropertyWithVerb :: Partial => RoleType -> EnumeratedRoleType -> EnumeratedPropertyType -> PropertyVerb -> MonadPerspectives Boolean
hasPerspectiveOnPropertyWithVerb subjectType roleType property verb = do
  -- adt <- getEnumeratedRole roleType >>= roleADT
  isJust <$> findPerspective
    subjectType
    -- The test is: is `property` included in the collection of properties of the perspective object?
    (allProperties <<< map roleInContext2Role <<< objectOfPerspective >=> pure <<< isJust <<< elemIndex (ENP property))

-- perspectiveSupportsProperty
rolesWithPerspectiveOnProperty :: PropertyType -> ContextType ~~~> RoleType
rolesWithPerspectiveOnProperty pt = COMB.filter userRole (propertyIsInPerspectiveOfUser pt)
  where
    -- voor de user of één van zijn aspecten.
    propertyIsInPerspectiveOfUser :: PropertyType -> RoleType ~~~> Boolean
    propertyIsInPerspectiveOfUser property userRole' = lift $ isJust <$> findPerspective userRole'
      (pure <<< (flip perspectiveSupportsProperty property))

----------------------------------------------------------------------------------------
------- FUNCTIONS FOR PERSPECTIVES
----------------------------------------------------------------------------------------
-- | All Perspectives of an EnumeratedRoleType or one of its Aspects.
perspectivesClosure :: EnumeratedRoleType ~~~> Perspective
perspectivesClosure = roleAspectsClosure >=> perspectivesOfRole

-- | Get all Perspectives that are defined for a RoleType or its Aspects.
-- | A CalculatedRole has no aspects, hence all Perspectives returned are defined directly on it.
perspectivesClosure_ :: RoleType ~~~> Perspective
perspectivesClosure_ (ENR t) = perspectivesClosure t
perspectivesClosure_ t = perspectivesOfRole_ t

-- | For a user RoleType, get all perspectives of that RoleType
perspectivesOfRole_ :: RoleType ~~~> Perspective
perspectivesOfRole_ (ENR erole) = perspectivesOfRole erole
perspectivesOfRole_ (CR crole) = ArrayT (getCalculatedRole crole >>= pure <<< perspectives)

perspectiveObjectQfd :: Perspective -> QueryFunctionDescription
perspectiveObjectQfd (Perspective{object}) = object

statesPerProperty :: Perspective -> MonadPerspectives (Map.Map PropertyType (Array StateIdentifier))
statesPerProperty (Perspective{propertyVerbs, object}) = foldWithIndexM f Map.empty (unwrap propertyVerbs)
  where
    f :: StateSpec ->
      Map.Map PropertyType (Array StateIdentifier) ->
      Array PropertyVerbs ->
      MonadPerspectives (Map.Map PropertyType (Array StateIdentifier))
    f stateId cum pvArr = do
      (r1 :: Array (Array PropertyType)) <- traverse (propertyVerbs2PropertyArray object) pvArr
      pure $ foldr (\prop cum' ->
        case Map.lookup prop cum' of
          Nothing -> Map.insert prop [stateSpec2StateIdentifier stateId] cum'
          Just states -> Map.insert prop (cons (stateSpec2StateIdentifier stateId) states) cum')
        cum
        (concat r1)

propertiesInPerspective :: Perspective -> MonadPerspectives (Array PropertyType)
propertiesInPerspective (Perspective{propertyVerbs, object}) = foldWithIndexM f [] (unwrap propertyVerbs)
  where
    f :: StateSpec ->
      Array PropertyType ->
      Array PropertyVerbs ->
      MonadPerspectives (Array PropertyType)
    f _ cum pvArr = append cum <<< concat <$> traverse (propertyVerbs2PropertyArray object) pvArr

propertyVerbs2PropertyArray :: QueryFunctionDescription -> PropertyVerbs -> MonadPerspectives (Array PropertyType)
propertyVerbs2PropertyArray object (PropertyVerbs pset _) = case pset of
  Universal -> allProperties (roleInContext2Role <$> (unsafePartial roleRange object))
  Empty -> pure []
  PSet props -> pure props

roleStates :: Perspective -> Array StateIdentifier
roleStates (Perspective {roleVerbs}) = stateSpec2StateIdentifier <$> (fromFoldable $ Map.keys (unwrap roleVerbs))

automaticStates :: Perspective -> Array StateIdentifier
automaticStates (Perspective {automaticStates:s}) = s

actionStates :: Perspective -> Array StateIdentifier
actionStates (Perspective {actions}) = stateSpec2StateIdentifier <$> (fromFoldable $ Map.keys (unwrap actions))

-- | perspectiveAspect `isAspectOfPerspective` perspective is true when
-- | the range of the object of `perspective` equalsOrSpecialisesADT the range of the object of `perspectiveAspect`.
-- | Notice the `equal` case!
-- | PARTIAL: can only be used after object of Perspective has been compiled in PhaseThree.
isAspectOfPerspective :: Partial => Perspective -> Perspective -> Boolean
isAspectOfPerspective perspectiveAspect perspective = (objectOfPerspective perspective) `equalsOrSpecialisesADT` (objectOfPerspective perspectiveAspect)

-- | `perspectiveAspect addedToPersective perspective` integreert perspectiveAspect in perspective.
-- | roleVerbs, propertyVerbs and actions of perspectiveAspect are added to those of perspective.
-- | If perspectiveAspect.selfOnly == true, perspective.selfOnly will be made true as well.
-- | The values of isEnumerated must be equal.
addPerspectiveTo :: Perspective -> Perspective -> Perspective
addPerspectiveTo (Perspective perspectiveAspect) (Perspective perspective) = Perspective perspective
  { roleVerbs = EncodableMap $ Map.unionWith append (unwrap perspectiveAspect.roleVerbs) (unwrap perspective.roleVerbs)
  -- Note that we take a simple approach here, foregoing the opportunity to append to PropertyVerbs when their
  -- PropertySets are equal.
  , propertyVerbs = EncodableMap $ Map.unionWith append (unwrap perspectiveAspect.propertyVerbs) (unwrap perspective.propertyVerbs)
  -- Note that two Action maps with duplicate keys will lose actions when added to each other.
  , actions = EncodableMap $ Map.unionWith OBJ.union (unwrap perspectiveAspect.actions) (unwrap perspective.actions)
  , selfOnly = perspectiveAspect.selfOnly || perspective.selfOnly
  }

-- True, iff one of the types of the role instance is covered by the range of the QueryFunctionDescription.
-- The function is partial because it should only be used on a QueryFunctionDescription whose range
-- represents a role type.
isPerspectiveOnSelf :: Partial => QueryFunctionDescription -> (RoleType ~~~> Boolean)
isPerspectiveOnSelf qfd = some (
  lift <<< typeIncludingAspects >=>
  (\adt -> pure $ (roleRange qfd `equalsOrSpecialisesADT` adt)))

----------------------------------------------------------------------------------------
------- FUNCTIONS FOR ACTIONS
----------------------------------------------------------------------------------------
type ActionName = String
-- | Note that we assume action names are unique over states for the perspective a user role has on an object.
getAction :: ActionName -> Perspective -> Maybe Action
getAction actionName (Perspective{actions}) = LIST.foldl
  (\maction stateDepActions -> case maction of
    Just action -> Just action
    Nothing -> OBJ.lookup actionName stateDepActions)
  Nothing
  (Map.values $ unwrap actions)

-- | Note that we assume action names are unique over states for the perspective a user role has on an object.
getContextAction :: ActionName -> RoleType -> MonadPerspectives (Maybe Action)
getContextAction actionName userRoleType = do
  stateActionMap <- actionsOfRoleType userRoleType
  pure $ LIST.foldl
    (\maction (stateDepActions :: OBJ.Object Action) -> case maction of
      Just action -> Just action
      Nothing -> OBJ.lookup actionName stateDepActions)
    Nothing
    (Map.values stateActionMap)
