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

module Perspectives.Types.ObjectGetters where

import Control.Monad.Error.Class (throwError)
import Control.Monad.State (StateT, execStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Control.Plus (empty, map, (<|>))
import Data.Array (cons, elemIndex, filter, filterA, findIndex, fold, foldl, null, singleton)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (for, for_, traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception (error)
import Foreign.Object (fromFoldable, keys)
import Perspectives.CoreTypes (type (~~~>), MonadPerspectives, (###=), type (~~>), (###>>))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Identifiers (areLastSegmentsOf, deconstructModelName, endsWithSegments)
import Perspectives.Instances.Combinators (closure_, conjunction)
import Perspectives.Instances.Combinators (filter', filter) as COMB
import Perspectives.InvertedQuery (RelevantProperties(..), PropsAndVerbs)
import Perspectives.Query.QueryTypes (Calculation(..), QueryFunctionDescription)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Action (Action(..), Verb)
import Perspectives.Representation.Class.Action (object, providesPerspectiveOnProperty, providesPerspectiveOnRole, requiredObjectProperties, verb, objectType)
import Perspectives.Representation.Class.Context (allContextTypes)
import Perspectives.Representation.Class.Context (contextRole, roleInContext, userRole, contextAspectsADT) as ContextClass
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.PersistentType (getAction, getCalculatedRole, getContext, getEnumeratedProperty, getEnumeratedRole, getPerspectType, getView)
import Perspectives.Representation.Class.Role (actionSet, adtOfRoleAspectsBinding, allProperties, allRoles, allViews, getRole, greaterThanOrEqualTo, leavesInADT, perspectives, perspectivesOfRoleType, roleADT, roleAspects, typeIncludingAspects)
import Perspectives.Representation.Context (Context)
import Perspectives.Representation.ExplicitSet (hasElementM)
import Perspectives.Representation.InstanceIdentifiers (Value(..))
import Perspectives.Representation.TypeIdentifiers (ActionType, CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType, EnumeratedRoleType(..), PropertyType(..), RoleType(..), ViewType, propertytype2string, roletype2string)
import Perspectives.Representation.View (propertyReferences)
import Prelude (Unit, bind, eq, flip, pure, show, unit, ($), (<$>), (<<<), (<>), (==), (>=>), (>>=), (>>>), (&&), (||))

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
actionsOfRole :: EnumeratedRoleType ~~~> ActionType
actionsOfRole rt = ArrayT (getEnumeratedRole rt >>= unwrap >>> _.perspectives >>> pure)

actionsClosure :: EnumeratedRoleType ~~~> ActionType
actionsClosure = aspectsClosure >=> actionsOfRole

isAutomatic :: ActionType ~~~> Boolean
isAutomatic at = ArrayT (getAction at >>= unwrap >>> _.executedByBot >>> singleton >>> pure)

-- | Get all Actions that are defined for a RoleType or its Aspects.
-- | A CalculatedRole has no aspects, hence all Actions returned are defined directly on it.
actionsClosure_ :: RoleType ~~~> ActionType
actionsClosure_ (ENR t) = actionsClosure t
actionsClosure_ t = actionsOfRole_ t

-- | For a user RoleType, get all Actions defined directly in perspectives of that RoleType
actionsOfRole_ :: RoleType ~~~> ActionType
actionsOfRole_ (ENR erole) = ArrayT (getEnumeratedRole erole >>= pure <<< perspectives)

actionsOfRole_ (CR crole) = ArrayT (getCalculatedRole crole >>= pure <<< perspectives)

actionObjectQfd :: ActionType ~~~> QueryFunctionDescription
actionObjectQfd at = ArrayT do
  action <- getAction at
  case object action of
    S _ -> throwError (error "bla")
    Q calc -> pure [calc]

-- | From an ActionType, get the view on its object
objectView :: ActionType ~~~> ViewType
objectView = ArrayT <<< (getAction >=> requiredObjectProperties >>> maybe [] singleton >>> pure)
-- objectView at = ArrayT (getAction at >>= requiredObjectProperties >>> maybe [] singleton >>> pure)

hasVerb :: Verb -> ActionType ~~~> Boolean
hasVerb v = (lift <<< getAction) >=> pure <<< eq v <<< verb

----------------------------------------------------------------------------------------
------- GET A PERSPECTIVE
----------------------------------------------------------------------------------------
-- | Return all Actions defined with (the first) RoleType as their object, taken
-- | from the transitive Aspect closure of the second RoleType.
getPerspectiveOnObject :: RoleType -> RoleType ~~~> ActionType
getPerspectiveOnObject objectType ur =
  (roleTypeAspectsClosure >=> getPerspective objectType) ur

-- | For the ADT EnumeratedRoleType that identifies a type of object (first argument), return
-- | the ActionTypes available in the perspective of the user role (second argument).
getPerspective :: RoleType -> RoleType ~~~> ActionType
getPerspective objectRoleType userRoleType = ArrayT $ (perspectivesOfRoleType userRoleType) >>= traverse getAction >>= filterA (providesPerspectiveOnRole objectRoleType) >>= pure <<< (map identifier)

-- | For the user role (second argument), and the object role (first argument), compute
-- | for each verb in the perspective the former has on the latter, the properties,
-- | indexed with the Action Verb (PropsAndVerbs).
-- | Results are computed for the user rule and its Aspects.
-- | Notice that only RelevantProperties are returned. In case of All, this symbolically
-- | also represents properties of Aspects and the binding hierarchy, but does not make them explicit.
propsAndVerbsForObjectRole :: RoleType -> RoleType -> MonadPerspectives PropsAndVerbs
propsAndVerbsForObjectRole objectRole userRole' = do
  actions <- userRole' ###= (roleTypeAspectsClosure >=> getPerspective objectRole)
  fromFoldable <$>
    (for actions (getAction >=> \action -> do
      props <- case requiredObjectProperties action of
        Nothing -> pure All
        Just v -> Properties <$> (v ###= propertiesOfView)
      pure $ Tuple (show $ verb action) props))

propsForObjectRole :: RoleType -> RoleType -> MonadPerspectives RelevantProperties
propsForObjectRole objectRole userRole' = do
  actions <- userRole' ###= (roleTypeAspectsClosure >=> getPerspective objectRole)
  fold <$>
    (for actions (getAction >=> \action -> do
      props <- case requiredObjectProperties action of
        Nothing -> pure All
        Just v -> Properties <$> (v ###= propertiesOfView)
      pure props))

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
  DomeinFile {calculatedRoles, enumeratedRoles} <- retrieveDomeinFile namespace
  eCandidates <- pure $ map (ENR <<< EnumeratedRoleType) (filter (\_id -> _id `endsWithSegments` localName) (keys enumeratedRoles))
  cCandidates <- pure $ map (CR <<< CalculatedRoleType) (filter (\_id -> _id `endsWithSegments` localName) (keys calculatedRoles))
  pure $ eCandidates <> cCandidates

qualifyEnumeratedRoleInDomain :: String -> String ~~~> EnumeratedRoleType
qualifyEnumeratedRoleInDomain localName namespace = ArrayT do
  DomeinFile {enumeratedRoles} <- retrieveDomeinFile namespace
  pure $ map EnumeratedRoleType (filter (\_id -> _id `endsWithSegments` localName) (keys enumeratedRoles))

qualifyCalculatedRoleInDomain :: String -> String ~~~> CalculatedRoleType
qualifyCalculatedRoleInDomain localName namespace = ArrayT do
  DomeinFile {calculatedRoles} <- retrieveDomeinFile namespace
  pure $ map CalculatedRoleType (filter (\_id -> _id `endsWithSegments` localName) (keys calculatedRoles))

qualifyContextInDomain :: String -> String ~~~> ContextType
qualifyContextInDomain localName namespace = ArrayT do
  DomeinFile {contexts} <- retrieveDomeinFile namespace
  pure $ map ContextType (filter (\_id -> _id `endsWithSegments` localName) (keys contexts))

----------------------------------------------------------------------------------------
------- USER ROLETYPES WITH A PERSPECTIVE ON A ROLETYPE
----------------------------------------------------------------------------------------
-- | <user RoleType> `hasPerspectiveOnRole` <RoleType>
-- | True, iff the user RoleType or one of its aspects has a perspective on the role type.
hasPerspectiveOnRole :: RoleType -> EnumeratedRoleType ~~~> Boolean
hasPerspectiveOnRole ur rt = ArrayT (execStateT (hasPerspectiveWithVerb ur rt []) false >>= pure <<< singleton)

-- | <RoleType> `roleIsInPerspectiveOf` <userRole>
roleIsInPerspectiveOf :: EnumeratedRoleType -> RoleType ~~~> Boolean
roleIsInPerspectiveOf = flip hasPerspectiveOnRole

hasPerspectiveOnRoleWithVerbs :: Array Verb -> RoleType -> EnumeratedRoleType ~~~> Boolean
hasPerspectiveOnRoleWithVerbs verbs ur rt = ArrayT (execStateT (hasPerspectiveWithVerb ur rt verbs) false >>= pure <<< singleton)

type Found a = StateT Boolean MonadPerspectives a

hasPerspectiveWithVerb :: RoleType -> EnumeratedRoleType -> Array Verb -> Found Unit
hasPerspectiveWithVerb subjectType roleType verbs = do
  objectIsRootUser <- lift (roleType ###>> hasAspect (EnumeratedRoleType "model:System$RootContext$RootUser"))
  if objectIsRootUser
    then put true
    else do
      (allSubjects :: Array RoleType) <- lift (subjectType ###= roleTypeAspectsClosure)
      for_ allSubjects
        \userRole' -> hasBeenFound >>= if _
          then pure unit
          else do
            (as :: Array ActionType) <- lift (userRole' ###= actionsOfRole_)
            for_ as
              \at -> hasBeenFound >>= if _
                then pure unit
                else do
                  act@(Action{verb}) <- lift $ getAction at
                  adt <- lift $ objectType act
                  if (null verbs || (isJust $ elemIndex verb verbs)) &&
                    (isJust $ elemIndex roleType (leavesInADT adt))
                    then put true
                    else pure unit
  where
    hasBeenFound :: Found Boolean
    hasBeenFound = get

-- | In a Context type, find all enumerated local user roles that have a perspective on a given RoleType.
localEnumeratedRolesWithPerspectiveOnRole :: RoleType -> ContextType ~~~> RoleType
localEnumeratedRolesWithPerspectiveOnRole rt = COMB.filter enumeratedUserRole (roleIsInPerspectiveOfLocalUser rt)
  where
  -- | <RoleType> `roleIsInPerspectiveOfLocalUser` <userRole>
  -- | True iff the userRole or one of its aspects has an Action with RoleType as object.
  roleIsInPerspectiveOfLocalUser :: RoleType -> RoleType ~~~> Boolean
  roleIsInPerspectiveOfLocalUser rt' userRole' = (lift $ typeIncludingAspects userRole') >>= lift <<< actionSet >>= hasElementM g
    where
      g = lift <<< getAction >=> lift <<< providesPerspectiveOnRole rt'

----------------------------------------------------------------------------------------
------- USER ROLETYPES WITH A PERSPECTIVE ON A PROPERTYTYPE
----------------------------------------------------------------------------------------
hasPerspectiveOnProperty :: RoleType -> EnumeratedPropertyType ~~~> Boolean
hasPerspectiveOnProperty ur pt = do
  roleType <- lift (getEnumeratedProperty pt >>= pure <<< _.role <<< unwrap)
  ArrayT (execStateT (hasPerspectiveOnPropertyWithVerb ur roleType pt []) false >>= pure <<< singleton)

-- | <RoleType> `propertyIsInPerspectiveOf` <userRole>
propertyIsInPerspectiveOf :: EnumeratedPropertyType -> RoleType ~~~> Boolean
propertyIsInPerspectiveOf = flip hasPerspectiveOnProperty

hasPerspectiveOnPropertyWithVerb :: RoleType -> EnumeratedRoleType -> EnumeratedPropertyType -> Array Verb -> Found Unit
hasPerspectiveOnPropertyWithVerb subjectType roleType property verbs = do
  allSubjects <- lift (subjectType ###= roleTypeAspectsClosure)
  for_ allSubjects
    \userRole' -> hasBeenFound >>= if _
      then pure unit
      else do
        (as :: Array ActionType) <- lift (userRole' ###= actionsOfRole_)
        for_ as
          \at -> hasBeenFound >>= if _
            then pure unit
            else do
              act@(Action{verb, requiredObjectProperties}) <- lift $ getAction at
              if (null verbs || (isJust $ elemIndex verb verbs))
                then case requiredObjectProperties of
                  Just vt -> do
                    props <- lift (vt ###= propertiesOfView)
                    case elemIndex (ENP property) props of
                      Nothing -> pure unit
                      otherwise -> put true
                  Nothing -> do
                    props <- lift (objectType act >>= allProperties)
                    case elemIndex (ENP property) props of
                      Nothing -> pure unit
                      otherwise -> put true
                else pure unit
  where
    hasBeenFound :: Found Boolean
    hasBeenFound = get

rolesWithPerspectiveOnProperty :: PropertyType -> ContextType ~~~> RoleType
rolesWithPerspectiveOnProperty pt = COMB.filter userRole (propertyIsInPerspectiveOfLocalUser pt)
  where
    propertyIsInPerspectiveOfLocalUser :: PropertyType -> RoleType ~~~> Boolean
    propertyIsInPerspectiveOfLocalUser rt userRole' = (lift $ typeIncludingAspects userRole') >>= lift <<< actionSet >>= hasElementM g
      where
        g = lift <<< getAction >=> lift <<< providesPerspectiveOnProperty rt
