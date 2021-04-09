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

import Control.Monad.Error.Class (throwError, try)
import Control.Monad.State (StateT, execStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Control.Plus (empty, map, (<|>))
import Data.Array (concat, cons, elemIndex, filter, filterA, findIndex, fold, foldl, head, intersect, null, singleton, fromFoldable)
import Data.List (List)
import Data.Map (values)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (for, for_, traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception (error)
import Foreign.Object (fromFoldable, keys) as OBJ
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~~>), MonadPerspectives, (###=), type (~~>), (###>>), (###>))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Error.Boundaries (handleDomeinFileError')
import Perspectives.Identifiers (areLastSegmentsOf, deconstructModelName, endsWithSegments)
import Perspectives.Instances.Combinators (closure_, conjunction)
import Perspectives.Instances.Combinators (filter', filter) as COMB
import Perspectives.InvertedQuery (PropsAndVerbs, RelevantProperties(..))
import Perspectives.Query.QueryTypes (Calculation(..), QueryFunctionDescription)
import Perspectives.Representation.ADT (ADT(..), leavesInADT)
import Perspectives.Representation.Action (Action(..), Verb)
import Perspectives.Representation.Class.Action (object, providesPerspectiveOnProperty, providesPerspectiveOnRole, requiredObjectProperties, verb, objectType)
import Perspectives.Representation.Class.Context (allContextTypes)
import Perspectives.Representation.Class.Context (contextRole, roleInContext, userRole, contextAspectsADT) as ContextClass
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.PersistentType (getCalculatedRole, getContext, getEnumeratedRole, getPerspectType, getView)
import Perspectives.Representation.Class.Role (adtOfRole, adtOfRoleAspectsBinding, allProperties, allRoles, allViews, getRole, greaterThanOrEqualTo, perspectives, perspectivesOfRoleType, roleADT, roleAspects, typeExcludingBinding, typeIncludingAspects, typeIncludingAspectsBinding)
import Perspectives.Representation.Context (Context)
import Perspectives.Representation.ExplicitSet (ExplicitSet(..), hasElementM)
import Perspectives.Representation.InstanceIdentifiers (Value(..))
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..), isPerspectiveOnADT, objectOfPerspective, perspectiveSupportsOneOfRoleVerbs, perspectiveSupportsProperty, perspectiveSupportsPropertyForVerb, perspectiveSupportsRoleVerbs)
import Perspectives.Representation.TypeIdentifiers (ActionType, CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType, EnumeratedRoleType(..), PropertyType(..), RoleType(..), ViewType, propertytype2string, roletype2string)
import Perspectives.Representation.Verbs (PropertyVerb, RoleVerb)
import Perspectives.Representation.View (propertyReferences)
import Prelude (Unit, bind, eq, flip, not, pure, show, unit, ($), (&&), (<$>), (<<<), (<>), (==), (>=>), (>>=), (>>>), (||), (<*>), (<#>))

----------------------------------------------------------------------------------------
------- FUNCTIONS ON ENUMERATEDROLETYPES
----------------------------------------------------------------------------------------
isUnlinked_ :: EnumeratedRoleType -> MonadPerspectives Boolean
isUnlinked_ et = getEnumeratedRole et >>= pure <<< _.unlinked <<< unwrap

----------------------------------------------------------------------------------------
------- FUNCTIONS TO FIND A ROLETYPE WORKING FROM STRINGS OR ADT'S
----------------------------------------------------------------------------------------
-- | If a role with the given qualified name is available in the Context or its (in)direct aspects,
-- | return it as a RoleType. From the type we can find out its RoleKind, too.
lookForRoleType :: String -> (ContextType ~~~> RoleType)
lookForRoleType s c = (lift $ getContext c) >>= pure <<< ContextClass.contextAspectsADT >>= lookForRoleTypeOfADT s

lookForRoleTypeOfADT :: String -> (ADT ContextType ~~~> RoleType)
lookForRoleTypeOfADT s = lookForRoleOfADT (roletype2string >>> ((==) s)) s

-- | As lookForRoleType, but then with a local name that should string-match the end of the qualified name.
lookForUnqualifiedRoleType :: String -> ContextType ~~~> RoleType
lookForUnqualifiedRoleType s c = (lift $ getContext c) >>= pure <<< ContextClass.contextAspectsADT >>= lookForUnqualifiedRoleTypeOfADT s

-- | We simply require the Pattern to match the end of the string.
lookForUnqualifiedRoleTypeOfADT :: String -> ADT ContextType ~~~> RoleType
lookForUnqualifiedRoleTypeOfADT s = lookForRoleOfADT (roletype2string >>> areLastSegmentsOf s) s

-- | Constructs the set of all roles in the ADT (recursing on aspects of Enumerated roles)
-- | and then applies a comparison function to the name passed in and the names of all those roles.
lookForRoleOfADT :: (RoleType -> Boolean) -> String -> ADT ContextType ~~~> RoleType
lookForRoleOfADT criterium rname adt =  ArrayT (allRoles adt >>= pure <<< filter criterium)

----------------------------------------------------------------------------------------
------- FUNCTIONS TO FIND A CONTEXTTYPE WORKING FROM STRINGS OR ADT'S
----------------------------------------------------------------------------------------
-- | Returns all qualified context types whose names end with the given input.
-- | Candidates are retrieved from the nested contexts of the given ADT ContextType, closed under Aspect.
lookForUnqualifiedContextType :: String -> ADT ContextType ~~~> ContextType
lookForUnqualifiedContextType s c = ArrayT (allContextTypes c >>= pure <<< filter ((areLastSegmentsOf s) <<< unwrap))

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

-- | Returns all Enumerated role types in the context
allEnumeratedRoles :: ContextType ~~~> EnumeratedRoleType
allEnumeratedRoles ct = ArrayT do
  rs <- runArrayT $ allRoleTypesInContext ct
  pure $ foldl f [] rs
  where
    f :: Array EnumeratedRoleType -> RoleType -> Array EnumeratedRoleType
    f roles (ENR r) = cons r roles
    f roles _ = roles

allRoleTypesInContext :: ContextType ~~~> RoleType
allRoleTypesInContext = conjunction roleInContext $ conjunction contextRole userRole'
  where
    userRole' :: ContextType ~~~> RoleType
    userRole' = ArrayT <<< ((getPerspectType :: ContextType -> MonadPerspectives Context) >=> pure <<< ContextClass.userRole)

-- | Returns the name of the model that defines the role type as a String Value.
contextTypeModelName :: ContextType ~~~> Value
contextTypeModelName (ContextType rid) = maybe empty (pure <<< Value) (deconstructModelName rid)

contextTypeModelName' :: ContextType ~~> Value
contextTypeModelName' (ContextType rid) = maybe empty (pure <<< Value) (deconstructModelName rid)

----------------------------------------------------------------------------------------
------- FUNCTIONS TO FIND AN ENUMERATEDPROPERTY WORKING FROM STRINGS OR ADT'S
----------------------------------------------------------------------------------------
-- | Look for a Property on a given EnumeratedRoleType (including its own aspects - not
-- | recursively - and its own binding - not recursively), using a criterium.
lookForPropertyType_ :: String -> (EnumeratedRoleType ~~~> PropertyType)
lookForPropertyType_ s i = (lift $ getRole (ENR i)) >>= lift <<< adtOfRoleAspectsBinding >>= lookForProperty (propertytype2string >>> ((==) s))

-- | Look for an unqualified Property on a given EnumeratedRoleType
-- | (recursing on aspects and on the binding of Enumerated Roles), using a criterium.
lookForUnqualifiedPropertyType_ :: String -> (EnumeratedRoleType ~~~> PropertyType)
lookForUnqualifiedPropertyType_ s i = lookForProperty (propertytype2string >>> areLastSegmentsOf s) (ST i)
-- lookForUnqualifiedPropertyType_ s i = (lift $ getRole (ENR i)) >>= lift <<< adtOfRoleAspectsBinding >>= lookForProperty (propertytype2string >>> areLastSegmentsOf s)

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
  ArrayT (allProperties (ST $ EnumeratedRoleType s))
  <|>
  ArrayT (getPerspectType (CalculatedRoleType s) >>= roleADT >>= allProperties)

----------------------------------------------------------------------------------------
------- FUNCTIONS FOR ASPECTS
----------------------------------------------------------------------------------------
aspectsOfRole :: EnumeratedRoleType ~~~> EnumeratedRoleType
aspectsOfRole = ArrayT <<< (getPerspectType >=> roleAspects)

aspectsClosure :: EnumeratedRoleType ~~~> EnumeratedRoleType
aspectsClosure = closure_ aspectsOfRole

roleTypeAspectsClosure :: RoleType ~~~> RoleType
roleTypeAspectsClosure (ENR r) = ENR <$> aspectsClosure r
roleTypeAspectsClosure (CR r) = pure $ CR r

-- aspect `hasAspect` roleType
-- roleType ##>>> hasAspect aspect
hasAspect :: EnumeratedRoleType -> (EnumeratedRoleType ~~~> Boolean)
hasAspect aspect roleType = ArrayT do
  aspects <- roleType ###= aspectsClosure
  pure [isJust $ findIndex ((==) aspect) aspects]

hasAspectWithLocalName :: String -> (EnumeratedRoleType ~~~> Boolean)
hasAspectWithLocalName localAspectName roleType = ArrayT do
  aspects <- roleType ###= aspectsClosure
  pure [isJust $ findIndex (test (unsafeRegex (localAspectName <> "$") noFlags)) (unwrap <$> aspects)]
----------------------------------------------------------------------------------------
------- FUNCTIONS FOR ACTIONS
----------------------------------------------------------------------------------------
-- actionsOfRole :: EnumeratedRoleType ~~~> ActionType
-- actionsOfRole rt = ArrayT (getEnumeratedRole rt >>= unwrap >>> _.perspectives >>> pure)
--
-- actionsClosure :: EnumeratedRoleType ~~~> ActionType
-- actionsClosure = aspectsClosure >=> actionsOfRole
--
-- isAutomatic :: ActionType ~~~> Boolean
-- isAutomatic at = ArrayT (getAction at >>= unwrap >>> _.executedByBot >>> singleton >>> pure)
--
-- -- | Get all Actions that are defined for a RoleType or its Aspects.
-- -- | A CalculatedRole has no aspects, hence all Actions returned are defined directly on it.
-- actionsClosure_ :: RoleType ~~~> ActionType
-- actionsClosure_ (ENR t) = actionsClosure t
-- actionsClosure_ t = actionsOfRole_ t
--
-- -- | For a user RoleType, get all Actions defined directly in perspectives of that RoleType
-- actionsOfRole_ :: RoleType ~~~> ActionType
-- actionsOfRole_ (ENR erole) = ArrayT (getEnumeratedRole erole >>= pure <<< perspectives)
--
-- actionsOfRole_ (CR crole) = ArrayT (getCalculatedRole crole >>= pure <<< perspectives)
--
-- actionObjectQfd :: ActionType ~~~> QueryFunctionDescription
-- actionObjectQfd at = ArrayT do
--   action <- getAction at
--   case object action of
--     S _ -> throwError (error "bla")
--     Q calc -> pure [calc]
--
-- -- | From an ActionType, get the view on its object
-- objectView :: ActionType ~~~> ViewType
-- objectView = ArrayT <<< (getAction >=> requiredObjectProperties >>> maybe [] singleton >>> pure)
-- -- objectView at = ArrayT (getAction at >>= requiredObjectProperties >>> maybe [] singleton >>> pure)
--
-- hasVerb :: Verb -> ActionType ~~~> Boolean
-- hasVerb v = (lift <<< getAction) >=> pure <<< eq v <<< verb

----------------------------------------------------------------------------------------
------- GET A PERSPECTIVE
----------------------------------------------------------------------------------------
perspectivesOfRole :: EnumeratedRoleType ~~~> Perspective
perspectivesOfRole rt = ArrayT (getEnumeratedRole rt >>= unwrap >>> _.perspectives >>> pure)

-- | <UserRole> `perspectiveOnADT` <ADT> gives the perspectives (at most one) of the
-- | UserRole on the role represented by the ADT.
-- | PARTIAL: cannot be used during model file parsing (only after PhaseThree).
perspectiveOnADT :: Partial => RoleType -> (ADT EnumeratedRoleType) ~~~> Perspective
perspectiveOnADT userRoleType adt = ArrayT do
  (ps :: Array Perspective) <- perspectivesOfRoleType userRoleType
  pure $ filter (objectOfPerspective >>> eq adt) ps

-- | <UserRole> `perspectiveOnRoleType` <ObjectRole> gives the perspectives (at most one)
-- | of the UserRole on the ObjectRole.
-- | PARTIAL: cannot be used during model file parsing (only after PhaseThree).
perspectiveOnRoleType :: Partial => RoleType -> RoleType ~~~> Perspective
perspectiveOnRoleType userRoleType objectRoleType = (lift $ getRole objectRoleType) >>= lift <<< adtOfRole >>= (perspectiveOnADT userRoleType)

-- | Return all Actions defined with (the first) RoleType as their object, taken
-- | from the transitive Aspect closure of the second RoleType.
-- getPerspectiveOnObject :: RoleType -> RoleType ~~~> ActionType
-- getPerspectiveOnObject objectType ur =
--   (roleTypeAspectsClosure >=> getPerspective objectType) ur

-- | For the ADT EnumeratedRoleType that identifies a type of object (first argument), return
-- | the ActionTypes available in the perspective of the user role (second argument).
-- getPerspective :: RoleType -> RoleType ~~~> ActionType
-- getPerspective objectRoleType userRoleType = ArrayT $ (perspectivesOfRoleType userRoleType) >>= traverse getAction >>= filterA (providesPerspectiveOnRole objectRoleType) >>= pure <<< (map identifier)

-- | For the user role (second argument), and the object role (first argument), compute
-- | for each verb in the perspective the former has on the latter, the properties,
-- | indexed with the Action Verb (PropsAndVerbs).
-- | Results are computed for the user role and its Aspects.
-- | Notice that only RelevantProperties are returned. In case of All, this symbolically
-- | also represents properties of Aspects and the binding hierarchy, but does not make them explicit.
propsAndVerbsForObjectRole :: Partial => RoleType -> RoleType -> MonadPerspectives PropsAndVerbs
propsAndVerbsForObjectRole objectRole userRole' = do
  (perspectives :: Array Perspective) <- objectRole ###= perspectiveOnRoleType userRole'
  pure $ OBJ.fromFoldable (concat (e <$> perspectives))
    where
      e :: Perspective -> Array (Tuple String RelevantProperties)
      e (Perspective{propertyVerbs}) = let
        (lpvs :: Array PropertyVerbs) = concat $ fromFoldable (values (unwrap propertyVerbs))
        in concat (map f lpvs)
      f :: PropertyVerbs -> Array (Tuple String RelevantProperties)
      f (PropertyVerbs pset verbs) = map
        (\verb -> let
          (props :: RelevantProperties) = case pset of
            Universal -> All
            Empty -> Properties []
            PSet ps -> Properties ps
          in (Tuple (show verb) props))
        (verbs :: Array PropertyVerb)

----------------------------------------------------------------------------------------
------- FUNCTIONS ON ROLETYPES
----------------------------------------------------------------------------------------
-- | R1 `specialisesRoleType` R2 is true, iff R2 is an (indirect) Aspect of R1 or if both are equal.
-- | We want to use this function as the computation behind the query step `specialisesRoleType`:
-- |  filter <expression that yields role types> with specialisesRoleType SomeRole
-- | The result must be all role types that, indeed, specialise SomeRole.
-- | hence in the QueryCompiler, we flip `specialisesRoleType`, so we can apply it to SomeRole first
-- | and it is still bound to the second parameter.
-- | This function yields a single result (it is functional in PL)
specialisesRoleType :: RoleType -> (RoleType ~~~> Value)
specialisesRoleType t1 t2 = lift do
  t1' <- typeIncludingAspects t1
  t2' <- typeIncludingAspects t2
  r <- t1' `greaterThanOrEqualTo` t2'
  pure $ Value $ show r

specialisesRoleType_ :: RoleType -> (RoleType -> MonadPerspectives Boolean)
specialisesRoleType_ t1 t2 = do
  t1' <- typeIncludingAspects t1
  t2' <- typeIncludingAspects t2
  t1' `greaterThanOrEqualTo` t2'

roleTypeModelName :: RoleType ~~~> Value
roleTypeModelName rt = maybe empty (pure <<< Value) (deconstructModelName (roletype2string rt))

roleTypeModelName' :: RoleType ~~> Value
roleTypeModelName' rt = maybe empty (pure <<< Value) (deconstructModelName (roletype2string rt))

-- | For a given context type, find the locally defined Enumerated role type that has an aspect role
-- | whose local name is the first parameter value.
localRoleSpecialisation :: String -> ContextType ~~~> EnumeratedRoleType
localRoleSpecialisation localAspectName = COMB.filter allEnumeratedRoles (hasAspectWithLocalName localAspectName)

----------------------------------------------------------------------------------------
------- FUNCTIONS TO FIND VIEWS AND ON VIEWS
----------------------------------------------------------------------------------------
propertiesOfView :: ViewType ~~~> PropertyType
propertiesOfView = ArrayT <<< (getPerspectType >=> pure <<< propertyReferences)

lookForUnqualifiedViewType_ :: String -> (EnumeratedRoleType ~~~> ViewType)
lookForUnqualifiedViewType_ s i = (lift $ getRole (ENR i)) >>= lift <<< adtOfRoleAspectsBinding >>= lookForView (unwrap >>> areLastSegmentsOf s)

lookForUnqualifiedViewType :: String -> (ADT EnumeratedRoleType ~~~> ViewType)
lookForUnqualifiedViewType s = lookForView (unwrap >>> areLastSegmentsOf s)

lookForView :: (ViewType -> Boolean) -> ADT EnumeratedRoleType ~~~> ViewType
lookForView criterium = COMB.filter' (ArrayT <<< allViews) criterium

-- | If no view is specified, all properties can be accessed.
hasProperty :: PropertyType -> ViewType ~~~> Boolean
hasProperty p = lift <<< getView >=> pure <<< isJust <<< elemIndex p <<< propertyReferences

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
-- | PARTIAL: cannot be used during model file parsing (only after PhaseThree).
hasPerspectiveOnRole :: Partial => RoleType -> EnumeratedRoleType ~~~> Boolean
-- hasPerspectiveOnRole ur rt =
hasPerspectiveOnRole ur rt = ArrayT ((hasPerspectiveWithVerb ur rt []) >>= pure <<< singleton)

-- | <RoleType> `roleIsInPerspectiveOf` <userRole>
-- | PARTIAL: cannot be used during model file parsing (only after PhaseThree).
roleIsInPerspectiveOf :: Partial => EnumeratedRoleType -> RoleType ~~~> Boolean
roleIsInPerspectiveOf = flip hasPerspectiveOnRole

-- | PARTIAL: cannot be used during model file parsing (only after PhaseThree).
hasPerspectiveOnRoleWithVerbs :: Partial => Array RoleVerb -> RoleType -> EnumeratedRoleType ~~~> Boolean
hasPerspectiveOnRoleWithVerbs verbs ur rt = ArrayT ((hasPerspectiveWithVerb ur rt verbs) >>= pure <<< singleton)

-- | True if the user role (subjectType (first) argument) has a perspective
-- | on the object (roleType (second) argument) that includes one of the given RoleVerbs,
-- | OR if the role type has the aspect "sys:RootContext$RootUser"
-- | PARTIAL: cannot be used during model file parsing (only after PhaseThree).
hasPerspectiveWithVerb :: Partial => RoleType -> EnumeratedRoleType -> Array RoleVerb -> MonadPerspectives Boolean
hasPerspectiveWithVerb subjectType roleType verbs = do
  objectIsRootUser <- roleType ###>> hasAspect (EnumeratedRoleType "model:System$RootContext$RootUser")
  if objectIsRootUser
    then pure true
    else do
      -- Find for the subject (including its aspects) a perspective
      --    * whose object adt includes a leaf that is the object roleType or one of its aspects, AND
      --    * that supports at least one of the requested RoleVerbs.
      (allObjects :: Array EnumeratedRoleType) <- roleType ###= aspectsClosure
      isJust <$> findPerspective subjectType
        (\perspective@(Perspective{roleVerbs}) -> (not $ null $ intersect allObjects (leavesInADT $ objectOfPerspective perspective)) && (null verbs || perspectiveSupportsOneOfRoleVerbs perspective verbs))

-- | In a Context type, find all enumerated local user roles that have a perspective on a given RoleType.
localEnumeratedRolesWithPerspectiveOnRole :: Partial => RoleType -> ContextType ~~~> RoleType
localEnumeratedRolesWithPerspectiveOnRole rt = COMB.filter enumeratedUserRole
  (\ur -> do
    adt <- lift $ typeExcludingBinding rt
    roleIsInPerspectiveOfUser adt ur
    )
  where
  -- | <RoleType> `roleIsInPerspectiveOfLocalUser` <userRole>
  -- | True iff the userRole or one of its aspects has a Perspective with RoleType as object.
  roleIsInPerspectiveOfUser :: ADT EnumeratedRoleType -> RoleType ~~~> Boolean
  roleIsInPerspectiveOfUser adt userRole' = ArrayT (singleton <<< isJust <$> findPerspective userRole'
    (\perspective -> isPerspectiveOnADT perspective adt))

----------------------------------------------------------------------------------------
------- USER ROLETYPES WITH A PERSPECTIVE ON A PROPERTYTYPE
----------------------------------------------------------------------------------------
hasPerspectiveOnProperty :: RoleType -> EnumeratedPropertyType ~~~> Boolean
hasPerspectiveOnProperty subjectType property = ArrayT do
  singleton <<< isJust <$> findPerspective
    subjectType
    \perspective -> perspectiveSupportsProperty perspective (ENP property)

-- | <RoleType> `propertyIsInPerspectiveOf` <userRole>
propertyIsInPerspectiveOf :: EnumeratedPropertyType -> RoleType ~~~> Boolean
propertyIsInPerspectiveOf = flip hasPerspectiveOnProperty

----------------------------------------------------------------------------------------
------- FIND A SPECIFIC PERSPECTIVE
----------------------------------------------------------------------------------------
-- | Returns just the first perspective for the given (user) RoleType (including its
-- | aspects in the search) that complies with the criterium, or Nothing.
findPerspective :: RoleType -> (Perspective -> Boolean) -> MonadPerspectives (Maybe Perspective)
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
              \perspective -> if criterium perspective
                then put (Just perspective)
                else pure unit
    hasBeenFound :: StateT (Maybe Perspective) MonadPerspectives Boolean
    hasBeenFound = get >>= pure <<< isJust

-- | True iff the subject type has a perspective that includes the property,
-- | qualified with the given PropertyVerb.

-- | PARTIAL: cannot be used during model file parsing (only after PhaseThree).
hasPerspectiveOnPropertyWithVerb :: Partial => RoleType -> EnumeratedRoleType -> EnumeratedPropertyType -> PropertyVerb -> MonadPerspectives Boolean
hasPerspectiveOnPropertyWithVerb subjectType roleType property verb = do
  adt <- getEnumeratedRole roleType >>= roleADT
  isJust <$> findPerspective
    subjectType
    \perspective -> perspective `isPerspectiveOnADT` adt &&
      perspectiveSupportsPropertyForVerb perspective (ENP property) verb

-- perspectiveSupportsProperty
rolesWithPerspectiveOnProperty :: PropertyType -> ContextType ~~~> RoleType
rolesWithPerspectiveOnProperty pt = COMB.filter userRole (propertyIsInPerspectiveOfUser pt)
  where
    -- voor de user of één van zijn aspecten.
    propertyIsInPerspectiveOfUser :: PropertyType -> RoleType ~~~> Boolean
    propertyIsInPerspectiveOfUser property userRole' = lift $ isJust <$> findPerspective userRole'
      (flip perspectiveSupportsProperty property)

-- | The object of the perspective (first argument) must cover the RoleType in the sense
-- | that its EnumeratedRoleTypes form a superset of those of the role type
-- | (second argument), *not including* its binding or aspects.
-- | PARTIAL: cannot be used during model file parsing (only after PhaseThree).
isPerspectiveOnRole :: Partial => Perspective -> RoleType -> MonadPerspectives Boolean
isPerspectiveOnRole p t = typeExcludingBinding t >>= pure <<< isPerspectiveOnADT p

-- | The object of the perspective (first argument) must cover the RoleType in the sense
-- | that its EnumeratedRoleTypes form a superset of those of the role type
-- | (second argument), *including* its binding or aspects.
-- | PARTIAL: cannot be used during model file parsing (only after PhaseThree).
isPerspectiveOnRoleAndAspects :: Partial => Perspective -> RoleType -> MonadPerspectives Boolean
isPerspectiveOnRoleAndAspects p t = typeIncludingAspectsBinding t >>= pure <<< isPerspectiveOnADT p
----------------------------------------------------------------------------------------
------- FUNCTIONS FOR PERSPECTIVES
----------------------------------------------------------------------------------------
perspectivesClosure :: EnumeratedRoleType ~~~> Perspective
perspectivesClosure = aspectsClosure >=> perspectivesOfRole

-- isAutomatic :: ActionType ~~~> Boolean
-- isAutomatic at = ArrayT (getAction at >>= unwrap >>> _.executedByBot >>> singleton >>> pure)

-- | Get all Perspectives that are defined for a RoleType or its Aspects.
-- | A CalculatedRole has no aspects, hence all Perspectives returned are defined directly on it.
perspectivesClosure_ :: RoleType ~~~> Perspective
perspectivesClosure_ (ENR t) = perspectivesClosure t
perspectivesClosure_ t = perspectivesOfRole_ t

-- | For a user RoleType, get all Actions defined directly in perspectives of that RoleType
perspectivesOfRole_ :: RoleType ~~~> Perspective
perspectivesOfRole_ (ENR erole) = ArrayT (getEnumeratedRole erole >>= pure <<< perspectives)
perspectivesOfRole_ (CR crole) = ArrayT (getCalculatedRole crole >>= pure <<< perspectives)

perspectiveObjectQfd :: Partial => Perspective -> QueryFunctionDescription
perspectiveObjectQfd (Perspective{object}) = do
  case object of
    Q calc -> calc
