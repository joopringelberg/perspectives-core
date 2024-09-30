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

module Perspectives.Types.ObjectGetters
  
  where

import Control.Monad.Error.Class (try)
import Control.Monad.State (StateT, execStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Control.Plus (map, (<|>), empty)
import Data.Array (concat, cons, elemIndex, filter, filterA, findIndex, foldl, foldr, fromFoldable, null, singleton)
import Data.FoldableWithIndex (foldWithIndexM)
import Data.List (foldl) as LIST
import Data.Map (Map, empty, lookup, insert, keys, unionWith, values) as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (for_, traverse)
import Foreign.Object (Object, keys, lookup, union) as OBJ
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~>), type (~~~>), MP, MonadPerspectives, (###=), (###>), (###>>))
import Perspectives.Data.EncodableMap (EncodableMap(..))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Error.Boundaries (handleDomeinFileError', handlePerspectRolError')
import Perspectives.Identifiers (areLastSegmentsOf, typeUri2ModelUri, endsWithSegments, isExternalRole, startsWithSegments)
import Perspectives.Instances.Combinators (closure_, conjunction, filter', some)
import Perspectives.Instances.Combinators (filter', filter) as COMB
import Perspectives.ModelDependencies (rootUser)
import Perspectives.Persistence.API (Keys(..), getViewOnDatabase)
import Perspectives.Persistent (modelDatabaseName)
import Perspectives.Persistent.PublicStore (PublicStore)
import Perspectives.Query.QueryTypes (Calculation, QueryFunctionDescription, RoleInContext(..), domain2roleType, queryFunction, range, roleInContext2Role, roleRange, secondOperand)
import Perspectives.Representation.ADT (ADT(..), allLeavesInADT, computeExpandedBoolean, equalsOrSpecialises_, equals_, generalises_)
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.CNF (CNF)
import Perspectives.Representation.Class.Context (contextADT, contextRole, roleInContext, userRole) as ContextClass
import Perspectives.Representation.Class.Context (contextAspects)
import Perspectives.Representation.Class.Context (externalRole) as CTCLASS
import Perspectives.Representation.Class.PersistentType (DomeinFileId, getCalculatedRole, getContext, getEnumeratedRole, getPerspectType, getView, tryGetState)
import Perspectives.Representation.Class.Role (actionsOfRoleType, adtOfRole, allProperties, allRoleProperties, allRoles, allViews, bindingOfADT, calculation, expandUnexpandedLeaves, getRole, getRoleADT, perspectives, perspectivesOfRoleType, roleADT, roleADTOfRoleType, roleKindOfRoleType, toConjunctiveNormalForm_)
import Perspectives.Representation.Context (Context)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..))
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..), StateSpec, objectOfPerspective, perspectiveSupportsOneOfRoleVerbs, perspectiveSupportsProperty, perspectiveSupportsPropertyForVerb, stateSpec2StateIdentifier)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType, EnumeratedRoleType(..), PropertyType(..), RoleType(..), StateIdentifier(..), ViewType, propertytype2string, roletype2string)
import Perspectives.Representation.TypeIdentifiers (RoleKind(..)) as TI
import Perspectives.Representation.Verbs (PropertyVerb, RoleVerb)
import Perspectives.Representation.View (propertyReferences)
import Perspectives.Utilities (addUnique)
import Prelude (Unit, append, bind, eq, flip, not, pure, show, unit, ($), (&&), (*>), (<$>), (<<<), (<>), (==), (>=>), (>>=), (>>>), (||), (<*>))

----------------------------------------------------------------------------------------
------- FUNCTIONS ON ENUMERATEDROLETYPES
----------------------------------------------------------------------------------------
isUnlinked_ :: EnumeratedRoleType -> MonadPerspectives Boolean
isUnlinked_ et = getEnumeratedRole et >>= pure <<< _.unlinked <<< unwrap

isUnlinked :: EnumeratedRoleType ~~~> Boolean
isUnlinked = lift <<< isUnlinked_

isFunctional_ :: EnumeratedRoleType -> MonadPerspectives Boolean
isFunctional_ et = getEnumeratedRole et >>= pure <<< _.functional <<< unwrap

isRelational_ :: EnumeratedRoleType -> MonadPerspectives Boolean
isRelational_ et = getEnumeratedRole et >>= pure <<< not <<< _.functional <<< unwrap

isMandatory_ :: EnumeratedRoleType -> MonadPerspectives Boolean
isMandatory_ et = getEnumeratedRole et >>= pure <<< _.mandatory <<< unwrap

isOptional_ :: EnumeratedRoleType -> MonadPerspectives Boolean
isOptional_ et = getEnumeratedRole et >>= pure <<< not <<< _.mandatory <<< unwrap

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

indexedRoleName :: EnumeratedRoleType -> MonadPerspectives (Maybe RoleInstance)
indexedRoleName rtype = getEnumeratedRole rtype >>= pure <<< _.indexedRole <<< unwrap 

propertyAliases :: EnumeratedRoleType -> MP (OBJ.Object EnumeratedPropertyType)
propertyAliases rtype = getEnumeratedRole rtype >>= pure <<< _.propertyAliases <<< unwrap 

publicUrl_ :: EnumeratedRoleType -> MonadPerspectives (Maybe Calculation)
publicUrl_ et = getEnumeratedRole et >>= pure <<< _.publicUrl <<< unwrap

isPublicProxy :: RoleType -> MonadPerspectives Boolean
isPublicProxy = roleKindOfRoleType >=> pure <<< eq TI.PublicProxy

isPublic :: RoleType -> MonadPerspectives Boolean
isPublic = roleKindOfRoleType >=> \rk -> pure (rk == TI.Public || rk == TI.PublicProxy)

----------------------------------------------------------------------------------------
------- FUNCTIONS TO FIND A ROLETYPE WORKING FROM STRINGS OR ADT'S
----------------------------------------------------------------------------------------
string2RoleType :: String -> MonadPerspectives RoleType
string2RoleType qualifiedRoleName = getEnumeratedRole (EnumeratedRoleType qualifiedRoleName) *> pure (ENR $ EnumeratedRoleType qualifiedRoleName) <|> pure (CR $ CalculatedRoleType qualifiedRoleName)

string2EnumeratedRoleType :: String -> MonadPerspectives EnumeratedRoleType
string2EnumeratedRoleType qualifiedRoleName = getEnumeratedRole (EnumeratedRoleType qualifiedRoleName) *> pure (EnumeratedRoleType qualifiedRoleName)

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
------- FUNCTIONS TO FIND A ROLEINCONTEXT ADT WORKING FROM STRINGS OR ADT'S
----------------------------------------------------------------------------------------

-- | If a role with the given qualified name is available in the Context or its (in)direct aspects,
-- | return it as a RoleType. From the type we can find out its RoleKind, too.
-- lookForRoleType :: String -> (ContextType ~~~> RoleType)
-- lookForRoleType s c = (lift $ getContext c) >>= pure <<< ContextClass.contextADT >>= lookForRoleTypeOfADT s

-- lookForRoleTypeOfADT :: String -> (ADT ContextType ~~~> RoleType)
-- lookForRoleTypeOfADT s = lookForRoleOfADT (roletype2string >>> ((==) s)) s

-- -- | As lookForRoleType, but then with a local name that should string-match the end of the qualified name.
-- lookForUnqualifiedRoleType :: String -> ContextType ~~~> RoleType
-- lookForUnqualifiedRoleType s c = (lift $ getContext c) >>= pure <<< ContextClass.contextADT >>= lookForUnqualifiedRoleTypeOfADT s

-- -- | We simply require the Pattern to match the end of the string.
-- lookForUnqualifiedRoleTypeOfADT :: String -> ADT ContextType ~~~> RoleType
-- lookForUnqualifiedRoleTypeOfADT s = lookForRoleOfADT (roletype2string >>> areLastSegmentsOf s) s

-- -- | Constructs the set of all roles in the ADT (recursing on aspects of Enumerated roles)
-- -- | and then applies a comparison function to the name passed in and the names of all those roles.
-- lookForRoleOfADT :: (RoleType -> Boolean) -> String -> ADT ContextType ~~~> RoleType
-- lookForRoleOfADT criterium _ adt =  ArrayT (allRolesInContext adt >>= pure <<< filter criterium)

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

-- | Just returns the Enumerated proxies!
publicUserRole :: ContextType ~~~> RoleType
publicUserRole = ArrayT <<< 
  ((getPerspectType :: ContextType -> MonadPerspectives Context) >=> 
    filterA isPublicProxy <<< ContextClass.userRole)

-- | Returns User RoleTypes that are guaranteed to be Calculated.
calculatedUserRole :: ContextType ~~~> RoleType
calculatedUserRole =  ArrayT <<< ((getPerspectType :: ContextType -> MonadPerspectives Context) >=> pure <<< filter isCalculatedRole <<< ContextClass.userRole)

isCalculatedRole :: RoleType -> Boolean
isCalculatedRole (ENR _) = false
isCalculatedRole (CR _) = true

isCalculatedProperty :: PropertyType -> Boolean
isCalculatedProperty (ENP _) = false
isCalculatedProperty (CP _) = true

isEnumeratedProperty :: PropertyType -> Boolean
isEnumeratedProperty (ENP _) = true
isEnumeratedProperty (CP _) = false

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

aspectRoles :: ContextType ~~~> EnumeratedRoleType
aspectRoles ct@(ContextType contextName) = filter' allEnumeratedRoles
  (\(EnumeratedRoleType roleName) -> not (roleName `startsWithSegments` contextName)) ct

-- | Returns the name of the model that defines the role type as a String Value.
contextTypeModelName :: ContextType ~~~> Value
contextTypeModelName (ContextType rid) = maybe empty (pure <<< Value) (typeUri2ModelUri rid)

contextTypeModelName' :: ContextType ~~> Value
contextTypeModelName' (ContextType rid) = maybe empty (pure <<< Value) (typeUri2ModelUri rid)

-- | The state identifier of the root state of a role has the same string value as the role identifier.
contextRootState :: ContextType ~~~> StateIdentifier
contextRootState ctype = pure $ StateIdentifier $ unwrap ctype

-- | The transitive closure over Aspects of rootState.
contextRootStates :: ContextType ~~~> StateIdentifier
contextRootStates = contextAspectsClosure >=> contextRootState

getPublicStore_ :: ContextType -> MonadPerspectives (Maybe PublicStore)
getPublicStore_ ctype = getContext ctype >>= pure <<< _.public <<< unwrap 

indexedContextName :: ContextType -> MonadPerspectives (Maybe ContextInstance)
indexedContextName ctype = getContext ctype >>= pure <<< _.indexedContext <<< unwrap 

publicUrlComputation :: ContextType -> MonadPerspectives (Maybe Calculation)
publicUrlComputation ctype = 
  (ctype ###> publicUserRole) >>= case _ of 
    Just (ENR r) -> publicUrl_ r
    _ -> pure Nothing

externalRole :: ContextType -> MonadPerspectives EnumeratedRoleType
externalRole = getPerspectType >=> pure <<< CTCLASS.externalRole
----------------------------------------------------------------------------------------
------- FUNCTIONS OPERATING DIRECTLY ON STATE
----------------------------------------------------------------------------------------
-- | The substates registered in the state definition (not their transitive closure, nor Aspect states).
subStates_ :: StateIdentifier -> MonadPerspectives (Array StateIdentifier)
subStates_ = tryGetState >=> pure <<< (maybe [] _.subStates <<< map unwrap)

----------------------------------------------------------------------------------------
------- FUNCTIONS TO FIND A PROPERTYTYPE WORKING FROM STRINGS OR ADT'S
----------------------------------------------------------------------------------------

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
  ArrayT (getPerspectType (EnumeratedRoleType s) >>= allRoleProperties)
  <|>
  ArrayT (getPerspectType (CalculatedRoleType s) >>= roleADT >>= allProperties <<< map roleInContext2Role)

----------------------------------------------------------------------------------------
------- FUNCTIONS FOR ASPECTS
----------------------------------------------------------------------------------------
aspectsOfRole :: EnumeratedRoleType ~~~> EnumeratedRoleType
aspectsOfRole = ArrayT <<< (getPerspectType >=> map (map roleInContext2Role) <<< (pure <<< _.roleAspects <<< unwrap))

-- | All types of the role, including the root type itself.
-- | No doubles.
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
-- contextType ##>>> hasContextAspect aspect
-- Notice that `X hasContextAspect X` is true.
hasContextAspect :: ContextType -> (ContextType ~~~> Boolean)
hasContextAspect aspect contextType = ArrayT do
  aspects <- contextType ###= contextAspectsClosure
  pure [isJust $ findIndex ((==) aspect) aspects]

hasAspectWithLocalName :: String -> (EnumeratedRoleType ~~~> Boolean)
hasAspectWithLocalName localAspectName roleType = ArrayT do
  aspects <- roleType ###= roleAspectsClosure
  pure [isJust $ findIndex (test (unsafeRegex (localAspectName <> "$") noFlags)) (unwrap <$> aspects)]

getRoleAspectSpecialisations :: EnumeratedRoleType  ~~~> EnumeratedRoleType
getRoleAspectSpecialisations rn = ArrayT $ try (modelDatabaseName >>= \db -> getViewOnDatabase db "defaultViews/roleSpecialisationsView" (Key $ unwrap rn)) >>=
  handlePerspectRolError' "getAspectSpecialisations" []
    \(roles :: Array EnumeratedRoleType) -> pure roles

getContextAspectSpecialisations :: ContextType  ~~~> ContextType
getContextAspectSpecialisations cn = ArrayT $ try (modelDatabaseName >>= \db -> getViewOnDatabase db "defaultViews/contextSpecialisationsView" (Key $ unwrap cn)) >>=
  handlePerspectRolError' "getContextAspectSpecialisations" []
    \(contexts :: Array ContextType) -> pure contexts

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
roleTypeModelName rt = maybe empty (pure <<< Value) (typeUri2ModelUri (roletype2string rt))

roleTypeModelName' :: RoleType ~~> Value
roleTypeModelName' rt = maybe empty (pure <<< Value) (typeUri2ModelUri (roletype2string rt))

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
    isExternal = expandUnexpandedLeaves >=> pure <<< computeExpandedBoolean (\(RoleInContext{role}) -> isExternalRole $ unwrap role)

--------------------------------------------------------------------------------------------------
---- ALLTYPESINADT
--------------------------------------------------------------------------------------------------
-- | Compares with allLeavesInADT (module Perspectives.Representation.ADT).
-- | That function treats each leaf as atomic; here we expand it to the transitive closure of its aspects.
-- | In terms of sets: transform the ADT to Disjunctive Normal Form and then understand it
-- | as an union of unions.
-- | An alternative to this function is allLeavesInADT <<< roleInContext2role <$> roleAspectsADT 
allTypesInRoleADT :: ADT EnumeratedRoleType ~~~> EnumeratedRoleType
allTypesInRoleADT = ArrayT <<< pure <<< allLeavesInADT >=> roleAspectsClosure

allTypesInContextADT :: ADT ContextType ~~~> ContextType
allTypesInContextADT = ArrayT <<< pure <<< allLeavesInADT >=> contextAspectsClosure

--------------------------------------------------------------------------------------------------
---- EQUALSORGENERALISES, EQUALSORSPECIALISES FOR ROLETYPE
--------------------------------------------------------------------------------------------------
-- | t1 <- t2 (NOTICE REVERSED ARROW)
-- | t1 `generalisesRoleType` t2 is true, if, for example:
-- |    * t2 is an (indirect) Aspect of t1 or if both are equal.
-- |    * t2 fills t1.
-- | The term 'specialisation' should be applied to the level of terms, not to the extension of instances!
-- | In that sense, a V b is more specialised (extends) than a.
-- | We want to use this function as the computation behind the query step `generalisesRoleType`:
-- |  filter <expression that yields role types> with generalisesRoleType SomeRole
-- | The result must be all role types that, indeed, specialise SomeRole.
-- | hence in the QueryCompiler, we flip `generalisesRoleType`, so we can apply it to SomeRole first
-- | and it is still bound to the second parameter.
-- | This function yields a single result (it is functional in PL)
generalisesRoleType :: RoleType -> (RoleType ~~~> Value)
generalisesRoleType t1 t2 = ArrayT do 
  x <- (t1 `generalisesRoleType_` t2)
  pure [Value $ show x]

-- | t1 <- t2 (NOTICE REVERSED ARROW)
-- | See `generalisesRoleType.`
generalisesRoleType_ :: RoleType -> (RoleType -> MonadPerspectives Boolean)
generalisesRoleType_ t1 t2 = do 
  -- expand
  (et1 :: CNF RoleInContext) <- (roleADTOfRoleType >=> toConjunctiveNormalForm_) t1
  (et2 :: CNF RoleInContext) <- (roleADTOfRoleType >=> toConjunctiveNormalForm_) t2
  pure (et1 `generalises_` et2)

-----------------------------------------------------------
---- EQUALS, EQUALSORGENERALISES, EQUALSORSPECIALISES FOR ROLE IN CONTEXT
-----------------------------------------------------------
-- | Compares with `equalsOrGeneralises`.
-- | right -> left (logical implication)
equalsOrGeneralisesRoleInContext :: ADT RoleInContext -> ADT RoleInContext -> MP Boolean
equalsOrGeneralisesRoleInContext = flip equalsOrSpecialisesRoleInContext

-- | left -> right
-- | Compares with `equalsOrSpecialises`.
equalsOrSpecialisesRoleInContext :: ADT RoleInContext -> ADT RoleInContext -> MP Boolean
equalsOrSpecialisesRoleInContext left right = do 
  (left' :: CNF RoleInContext) <- toConjunctiveNormalForm_ left
  (right' :: CNF RoleInContext) <- toConjunctiveNormalForm_ right
  pure (left' `equalsOrSpecialises_` right')

equals :: ADT RoleInContext -> ADT RoleInContext -> MP Boolean
equals left right = do 
  (left' :: CNF RoleInContext) <- toConjunctiveNormalForm_ left
  (right' :: CNF RoleInContext) <- toConjunctiveNormalForm_ right
  pure (left' `equals_` right')

-----------------------------------------------------------
---- ISPERSPECTIVEONADT
-----------------------------------------------------------
-- | The object of the perspective must be equal to or be a generalisation of the given ADT,
-- | for the perspective to apply to the ADT.
-- | <perspective> `isPerspectiveOnADT` <adt>
-- | objectOfPerspective -> adt
-- | PARTIAL: can only be used after object of Perspective has been compiled in PhaseThree.
isPerspectiveOnADT :: Partial => Perspective -> ADT RoleInContext -> MP Boolean
isPerspectiveOnADT p adt = (objectOfPerspective p) `equalsOrGeneralisesRoleInContext` adt

isPerspectiveOnRoleType :: Partial => Perspective -> RoleType -> MP Boolean
isPerspectiveOnRoleType p roleType = getRoleADT roleType >>= isPerspectiveOnADT p

-- | From the object of the perspective, we derive an abstract type that characterises the restriction on the fillers of that object.
-- | If that restriction specialises the roleType, it must be because it has it (the roleType) as (indirect) filler.
-- | Such a perspective may provide access to some properties of the roleType.
isPerspectiveOnRoleFilledWithRoleType :: Partial => Perspective -> RoleType -> MP Boolean
isPerspectiveOnRoleFilledWithRoleType p roleType = do
  mfillRestriction <- bindingOfADT (objectOfPerspective p)
  case mfillRestriction of
    Nothing -> pure false
    Just fillRestriction -> getRoleADT roleType >>= equalsOrSpecialisesRoleInContext fillRestriction
  
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
qualifyRoleInDomain :: String -> DomeinFileId ~~~> RoleType
qualifyRoleInDomain localName namespace = ArrayT do
  (try $ retrieveDomeinFile namespace) >>=
    handleDomeinFileError' "qualifyRoleInDomain" []
      \(DomeinFile {calculatedRoles, enumeratedRoles}) -> do
        eCandidates <- pure $ map (ENR <<< EnumeratedRoleType) (filter (\id -> id `endsWithSegments` localName) (OBJ.keys enumeratedRoles))
        cCandidates <- pure $ map (CR <<< CalculatedRoleType) (filter (\id -> id `endsWithSegments` localName) (OBJ.keys calculatedRoles))
        pure $ eCandidates <> cCandidates

qualifyEnumeratedRoleInDomain :: String -> DomeinFileId ~~~> EnumeratedRoleType
qualifyEnumeratedRoleInDomain localName namespace = ArrayT do
  (try $ retrieveDomeinFile namespace) >>=
    handleDomeinFileError' "qualifyEnumeratedRoleInDomain" []
      \(DomeinFile {enumeratedRoles}) -> pure $ map EnumeratedRoleType (filter (\id -> id `endsWithSegments` localName) (OBJ.keys enumeratedRoles))

qualifyCalculatedRoleInDomain :: String -> DomeinFileId ~~~> CalculatedRoleType
qualifyCalculatedRoleInDomain localName namespace = ArrayT do
  (try $ retrieveDomeinFile namespace) >>=
    handleDomeinFileError' "qualifyCalculatedRoleInDomain" []
    \(DomeinFile {calculatedRoles}) -> pure $ map CalculatedRoleType (filter (\id -> id `endsWithSegments` localName) (OBJ.keys calculatedRoles))

qualifyContextInDomain :: String -> DomeinFileId ~~~> ContextType
qualifyContextInDomain localName namespace = ArrayT do
  (try $ retrieveDomeinFile namespace) >>=
    handleDomeinFileError' "qualifyContextInDomain" []
      \(DomeinFile {contexts}) -> pure $ map ContextType (filter (\id -> id `endsWithSegments` localName) (OBJ.keys contexts))

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
  objectIsRootUser <- roleType ###>> hasAspect (EnumeratedRoleType rootUser)
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
hasPerspectiveOnPropertyWithVerb :: Partial => RoleType -> EnumeratedRoleType -> EnumeratedPropertyType -> PropertyVerb -> MonadPerspectives Boolean
hasPerspectiveOnPropertyWithVerb subjectType roleType property verb =
    isJust <$> findPerspective
      subjectType
      (\perspective -> do 
        a <- pure $ perspectiveSupportsPropertyForVerb perspective (ENP property) verb
        b <- perspective `isPerspectiveOnRoleType` (ENR roleType)
        c <- perspective `isPerspectiveOnRoleFilledWithRoleType` (ENR roleType)
        pure (a && (b || c)))

-- | All role types in a context type that have a perspective on a given object role, with a perspective on the given property.
rolesWithPerspectiveOnRoleAndProperty :: Partial => RoleType -> PropertyType -> ContextType ~~~> RoleType
rolesWithPerspectiveOnRoleAndProperty object pt = COMB.filter userRole (propertyIsInPerspectiveOfUser pt)
  where
    -- voor de user of één van zijn aspecten.
    propertyIsInPerspectiveOfUser :: PropertyType -> RoleType ~~~> Boolean
    propertyIsInPerspectiveOfUser property userRole' = lift $ isJust <$> findPerspective userRole'
      (\perspective -> (&&) <$> (perspective `isPerspectiveOnRoleType` object) <*> (pure $ perspectiveSupportsProperty perspective pt))

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
          Just states -> Map.insert prop (addUnique (stateSpec2StateIdentifier stateId) states) cum')
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
-- | the range of the object of `perspective` equalsOrSpecialises the range of the object of `perspectiveAspect`.
-- | Notice the `equal` case!
-- | PARTIAL: can only be used after object of Perspective has been compiled in PhaseThree.
isAspectOfPerspective :: Partial => Perspective -> Perspective -> MP Boolean
isAspectOfPerspective perspectiveAspect perspective = (objectOfPerspective perspective) `equalsOrSpecialisesRoleInContext` (objectOfPerspective perspectiveAspect)

-- | `perspectiveAspect `addPerspectiveTo` perspective` integreert perspectiveAspect in perspective.
-- | roleVerbs, propertyVerbs and actions of perspectiveAspect are added to those of perspective.
-- | If perspectiveAspect.selfOnly == true, perspective.selfOnly will be made true as well.
-- | The same holds for authorOnly.
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
  , authorOnly = perspectiveAspect.authorOnly || perspective.authorOnly
  }

-- True, iff one of the types of the role instance is a specialisation of the range of the QueryFunctionDescription.
-- The function is partial because it should only be used on a QueryFunctionDescription whose range
-- represents a role type.
isPerspectiveOnSelf :: Partial => QueryFunctionDescription -> (RoleType ~~~> Boolean)
isPerspectiveOnSelf qfd = 
  some (\userRole' -> do 
    dnf <- lift $ toConjunctiveNormalForm_ (roleRange qfd)
    (lift $ roleADTOfRoleType userRole') >>= lift <<< toConjunctiveNormalForm_ >>=
      (\userRoleDNF -> pure (userRoleDNF `equals_` dnf)))

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

----------------------------------------------------------------------------------------
------- FUNCTIONS FOR STATES
----------------------------------------------------------------------------------------
roleGroundState :: EnumeratedRoleType -> StateIdentifier
roleGroundState (EnumeratedRoleType id) = StateIdentifier id

contextGroundState :: ContextType -> StateIdentifier
contextGroundState (ContextType id) = StateIdentifier id