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
-- along with this program.  If not, see <https:--www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE file in the projects root.
-- END LICENSE

-- | This module contains functions to create a JSON structure from a Perspective,
-- | that will be used by the client to build a screen automatically.

module Perspectives.TypePersistence.PerspectiveSerialisation where

import Control.Monad.Error.Class (throwError)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, concat, cons, elemIndex, filter, filterA, find, findIndex, foldl, head, intersect, modifyAt, nub, null, uncons, union)
import Data.Maybe (Maybe(..), fromJust, isJust, isNothing, maybe)
import Data.Newtype (unwrap)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..), fst)
import Effect.Exception (error)
import Foreign.Object (Object, empty, fromFoldable, insert, isEmpty, keys, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~>), AssumptionTracking, MonadPerspectives, (##>), (##>>))
import Perspectives.Data.EncodableMap (lookup) as EM
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.Identifiers (isExternalRole, qualifyWith)
import Perspectives.Instances.ObjectGetters (binding_, context, contextType, getActiveRoleStates, getActiveStates, roleType_)
import Perspectives.ModelDependencies (roleWithId)
import Perspectives.ModelTranslation (translateType)
import Perspectives.Parsing.Arc.AST (PropertyFacet(..))
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), domain, domain2roleType, functional, isContextDomain, makeComposition, mandatory, queryFunction, range, roleInContext2Context, roleInContext2Role, roleRange)
import Perspectives.Query.UnsafeCompiler (context2context, context2role, getDynamicPropertyGetter, getPublicUrl)
import Perspectives.Representation.ADT (ADT(..), allLeavesInADT)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.Class.Property (class PropertyClass)
import Perspectives.Representation.Class.Property (getProperty, isCalculated, functional, mandatory, range, Property(..), constrainingFacets) as PROP
import Perspectives.Representation.Class.Role (allProperties, bindingOfADT, perspectivesOfRoleType, roleKindOfRoleType)
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..), StateSpec(..), expandPropSet, expandPropertyVerbs, expandVerbs)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.ScreenDefinition (WidgetCommonFieldsDef)
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..), pessimistic)
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), ContextType(..), PropertyType(..), RoleKind(..), RoleType(..), propertytype2string, roletype2string)
import Perspectives.Representation.Verbs (PropertyVerb(..), RoleVerb(..), allPropertyVerbs, roleVerbList2Verbs)
import Perspectives.ResourceIdentifiers (createPublicIdentifier, guid)
import Perspectives.TypePersistence.PerspectiveSerialisation.Data (PropertyFacets, RoleInstanceWithProperties, SerialisedPerspective(..), SerialisedPerspective', SerialisedProperty, ValuesWithVerbs)
import Perspectives.Types.ObjectGetters (getContextAspectSpecialisations)
import Prelude (append, bind, discard, eq, flip, map, not, pure, show, unit, void, ($), (<$>), (<<<), (<>), (==), (>=>), (>>=), (||))
import Simple.JSON (writeJSON)

-- | Get the serialisation of the perspective the user role type has on the object role type,
-- | in a given context instance.
perspectiveForContextAndUser' ::
  RoleInstance ->           -- The user role instance
  RoleType ->               -- The user role type
  RoleType ->               -- An object role type, will be matched against the Perspective's roleTypes member.
  (ContextInstance ~~> SerialisedPerspective')
perspectiveForContextAndUser' subject userRoleType objectRoleType cid = ArrayT do
  contextStates <- map ContextState <$> (runArrayT $ getActiveStates cid)
  subjectStates <- map SubjectState <$> (runArrayT $ getActiveRoleStates subject)
  allPerspectives <- lift$  perspectivesOfRoleType userRoleType
  traverse
    ((serialisePerspective contextStates subjectStates cid userRoleType Nothing Nothing))
    (filter
      (isJust <<< elemIndex objectRoleType <<< _.roleTypes <<< unwrap)
      allPerspectives)

perspectiveForContextAndUser ::
  RoleInstance ->           -- The user role instance
  RoleType ->               -- The user role type
  RoleType ->               -- An object role type, will be matched against the Perspective's roleTypes member.
  (ContextInstance ~~> SerialisedPerspective)
perspectiveForContextAndUser subject userRoleType objectRoleType = perspectiveForContextAndUser' subject userRoleType objectRoleType >=> pure <<< SerialisedPerspective <<< writeJSON

-- | Get the serialisation of the perspective the user role type has on the object role type,
-- | in a given context instance.
perspectiveForContextAndUserFromId ::
  RoleInstance ->           -- The user role instance
  WidgetCommonFieldsDef ->
  ContextInstance ->
  AssumptionTracking SerialisedPerspective'
perspectiveForContextAndUserFromId subject {perspectiveId, propertyVerbs, roleVerbs, userRole} cid = do
  contextStates <- map ContextState <$> (runArrayT $ getActiveStates cid)
  subjectStates <- map SubjectState <$> (runArrayT $ getActiveRoleStates subject)
  allPerspectives <- lift $ perspectivesOfRoleType userRole
  perspective <- pure $ unsafePartial fromJust $ head (filter
    (\(Perspective{id}) -> id == perspectiveId)
    allPerspectives)
  serialisePerspective contextStates subjectStates cid userRole propertyVerbs roleVerbs perspective

perspectivesForContextAndUser :: RoleInstance -> RoleType -> (ContextInstance ~~> SerialisedPerspective)
perspectivesForContextAndUser subject userRoleType cid = ArrayT do
  perspectives <- runArrayT $ perspectivesForContextAndUser' subject userRoleType cid
  pure $ (SerialisedPerspective <<< writeJSON) <$> perspectives

perspectivesForContextAndUser' :: RoleInstance -> RoleType -> (ContextInstance ~~> SerialisedPerspective')
perspectivesForContextAndUser' subject userRoleType cid = ArrayT do
  contextStates <- map ContextState <$> (runArrayT $ getActiveStates cid)
  subjectStates <- map SubjectState <$> (runArrayT $ getActiveRoleStates subject)
  -- NOTE that we ignore perspectives that the user role's aspects may have!
  -- These have been added in compile time.
  perspectives <- lift $ perspectivesOfRoleType userRoleType
  (traverse (serialisePerspective contextStates subjectStates cid userRoleType Nothing Nothing) perspectives) >>=
    (filterA sendToClient)
  where
    sendToClient :: SerialisedPerspective' -> AssumptionTracking Boolean
    sendToClient {verbs, roleInstances, properties} = pure $
      if isEmpty roleInstances
        then (isJust $ elemIndex (show Create) verbs) || (isJust $ elemIndex (show CreateAndFill) verbs)
        else (not $ isEmpty properties) || (not $ isEmpty roleInstances)

serialisePerspective ::
  Array StateSpec ->
  Array StateSpec ->
  ContextInstance ->
  RoleType ->
  Maybe PropertyVerbs ->
  Maybe (Array RoleVerb) ->
  Perspective ->
  AssumptionTracking SerialisedPerspective'
serialisePerspective contextStates subjectStates cid userRoleType propertyVerbs' roleVerbs' p@(Perspective {id, object, isEnumerated, roleTypes, roleVerbs, propertyVerbs, actions}) = do
  -- All properties available on the object of the perspective.
  (allProps :: Array PropertyType) <- lift $ allProperties (roleInContext2Role <$> (unsafePartial domain2roleType $ range object))
  -- All PropertyVerbs available on the object of the perspective, given context- and subject state.
  (availablePropertyVerbs :: Array PropertyVerbs) <- pure $ concat (catMaybes $ (flip EM.lookup propertyVerbs) <$> (contextStates <> subjectStates))
  -- All PropertyTypes available for the object of the perspective, given context- and subject state.
  -- Restrict with the given PropertyVerbs.
  (availableProperties :: Array PropertyType) <- case propertyVerbs' of
    Nothing -> pure $ (concat $ expandPropSet allProps <<< (\(PropertyVerbs props _) -> props) <$> availablePropertyVerbs)
    Just (PropertyVerbs restrictedProps _) -> pure $ intersect (expandPropSet allProps restrictedProps) (concat $ expandPropSet allProps <<< (\(PropertyVerbs props _) -> props) <$> availablePropertyVerbs)
  -- Role instances with their property values.
  roleInstances <- roleInstancesWithProperties
    allProps
    (maybeAddIdentifier availableProperties)
    (verbsPerProperty (maybeAddPropertyVerbs availablePropertyVerbs) (maybeAddIdentifier allProps))
    cid
    p
    (case propertyVerbs' of
      Nothing -> allPropertyVerbs
      Just (PropertyVerbs _ verbs) -> expandVerbs verbs)
  -- Additional properties available on instances given object state.
  additionalPropertiesOnInstances <- pure $ foldl union [] (propertiesInInstance <$> roleInstances)
  -- If no properties are available, we'd like to add roleWithId as property. Otherwise, no table can be built.
  serialisedProps <- lift $ traverse makeSerialisedProperty ((maybeAddIdentifier availableProperties) <> additionalPropertiesOnInstances)
  roleKind <- lift $ traverse roleKindOfRoleType (head roleTypes) 
  -- If the binding of the ADT that is the range of the object QueryFunctionDescription, is an external role,
  -- its context type may be created.
  (contextTypesToCreate :: Object String) <- (lift $ maybe [] (allLeavesInADT <<< map roleInContext2Role) <$> 
      (bindingOfADT $ unsafePartial domain2roleType (range object)))
    >>= pure <<< (filter (isExternalRole <<< unwrap))
    >>= lift <<< traverse getEnumeratedRole
    >>= pure <<< map (_.context <<< unwrap)
    >>= \as -> lift $ ( (append as) <<< concat <$> (for as (runArrayT <<< getContextAspectSpecialisations)))
    >>= (\as' -> pure $ nub as')
    >>= (traverse \(ContextType cType) -> Tuple cType <$> translateType cType)
    >>= pure <<< fromFoldable
  identifyingProperty <- computeIdentifyingProperty serialisedProps roleInstances
  cType <- lift (cid ##>> contextType)
  contextIdToAddRoleInstanceTo <- (unsafePartial computeContextFromPerspectiveObject object) >>= (lift <<< context2context) >>= \f -> lift (cid ##> f)
  -- NOTE: there can be more than one roleType.
  (roleType :: String) <- pure (roletype2string <$> unsafePartial fromJust $ head roleTypes)
  (displayName :: String) <- lift $ translateType roleType
  translatedActions <- lift (fromFoldable <$> for (nub $ concat (keys <$> (catMaybes $ (flip EM.lookup actions) <$> (contextStates <> subjectStates)))) 
    \actionName -> do 
      translatedActionName <- translateType (qualifyWith (unwrap cType) actionName)
      pure $ Tuple actionName translatedActionName)
  pure { id
    , displayName
    , isFunctional: pessimistic $ functional object
    , isMandatory: pessimistic $ mandatory object
    , isCalculated: not isEnumerated
    , userRoleType: (roletype2string userRoleType) 
    , roleType: Just roleType
    , contextInstance: cid
    , roleKind
    , verbs: show <$> case roleVerbs' of
        -- No restrictions
        Nothing -> concat (roleVerbList2Verbs <$> (catMaybes $ (flip EM.lookup roleVerbs) <$> (contextStates <> subjectStates)))
        -- else combine the verbs from all active states.
        Just verbs -> (intersect verbs $ concat (roleVerbList2Verbs <$> (catMaybes $ (flip EM.lookup roleVerbs) <$> (contextStates <> subjectStates))))
    , properties: fromFoldable ((\r@({id:propId}) -> Tuple propId r) <$> serialisedProps)
    , actions: translatedActions
    , roleInstances: fromFoldable ((\r@({roleId}) -> Tuple roleId r) <$> roleInstances)
    , contextTypesToCreate
    , contextType: cType
    , contextIdToAddRoleInstanceTo
    , identifyingProperty
    }
  where

  -- The perspective object is by construction a role instance. However, there need not (yet) be one.
  -- We must adapt the object so it becomes a query to construct the context we can create in.
  -- A role can be reached from another role through the filled or filler operation, or from its context.
  -- In the first two cases we have to take the context of the object, so we have to add the `context` step;
  -- in the last case we have to remove the last step.
    computeContextFromPerspectiveObject :: Partial => QueryFunctionDescription -> AssumptionTracking QueryFunctionDescription
    computeContextFromPerspectiveObject qfd = case unsnoc qfd of 
      Just {query, lastStep} -> case queryFunction lastStep of 
        DataTypeGetterWithParameter FilledByF _ -> pure $ makeComposition qfd (SQD (range qfd) (DataTypeGetter ContextF) (CDOM (roleInContext2Context <$> unsafePartial roleRange qfd)) True True)
        DataTypeGetter FillerF -> pure $ makeComposition qfd (SQD (range qfd) (DataTypeGetter ContextF) (CDOM (roleInContext2Context <$> unsafePartial roleRange qfd)) True True)
        DataTypeGetterWithParameter FillerF _ -> pure $ makeComposition qfd (SQD (range qfd) (DataTypeGetter ContextF) (CDOM (roleInContext2Context <$> unsafePartial roleRange qfd)) True True)
        FilledF _ ctxt -> pure $ makeComposition qfd (SQD (range qfd) (DataTypeGetter ContextF) (CDOM $ UET ctxt) True True)
        RolGetter _ -> pure query
      Nothing -> if isContextDomain (domain qfd)
        then pure $ SQD (domain qfd) (DataTypeGetter IdentityF) (domain qfd) True True
        else throwError $ error "Programming error: Cannot compute a context domain from this perspective object."
    
    unsnoc :: QueryFunctionDescription -> Maybe {query :: QueryFunctionDescription, lastStep :: QueryFunctionDescription}
    unsnoc (BQD _ (BinaryCombinator ComposeF) qfd1 qfd2 _ _ _) | queryFunction qfd2 == (BinaryCombinator ComposeF) = 
      case unsnoc qfd2 of
        Just {query, lastStep} -> Just { query: makeComposition qfd1 query, lastStep }
        Nothing -> Nothing
    unsnoc (BQD _ _ qfd1 qfd2 _ _ _ ) = Just {query: qfd1, lastStep: qfd2}
    unsnoc _ = Nothing

    maybeAddIdentifier :: Array PropertyType -> Array PropertyType
    maybeAddIdentifier props = if null props then [CP $ CalculatedPropertyType roleWithId] else props

    maybeAddPropertyVerbs :: Array PropertyVerbs -> Array PropertyVerbs
    maybeAddPropertyVerbs pverbs = if null pverbs then [PropertyVerbs (PSet [CP $ CalculatedPropertyType roleWithId]) (PSet [Consult])] else pverbs


    nameRegex :: Regex
    nameRegex = unsafeRegex "Name" noFlags

    propertiesInInstance :: RoleInstanceWithProperties -> Array PropertyType
    propertiesInInstance = _.objectStateBasedProperties

    computeIdentifyingProperty ::
      Array SerialisedProperty ->
      Array RoleInstanceWithProperties ->
      AssumptionTracking String
    computeIdentifyingProperty serialisedProps roleInstances = case find (\property -> test nameRegex property.id) serialisedProps of
        Just n -> pure n.id
        _ -> if isJust $ find (\property -> property.id == roleWithId) serialisedProps
          then pure roleWithId
          else do
            -- Otherwise, compute the intersection of the props per role instance
            (commonProps :: Array String) <- case uncons (propNames <$> roleInstances) of
              Nothing -> pure []
              Just {head, tail} -> pure $ foldl intersect head tail
            -- Then find an element that matches "Name"
            case find (test nameRegex) commonProps of
              Just n -> pure n
              -- There may be no property shared by all instances that matches "Name";
              Nothing -> case head serialisedProps of
                -- Then we just return the first property that is available because of context- or subject state;
                Just s -> pure s.id
                -- Lacking that,
                Nothing -> case head commonProps of
                  -- we return the first common property;
                  Just c -> pure c
                  -- lacking that we look for the first roleInstance with properties and return the first of those.
                  -- Notice that other instances may not have that property in scope because of their state.
                  -- This is no problem; the client will just display an un-editable cell.
                  Nothing -> case find (not <<< null) (_.objectStateBasedProperties <$> roleInstances) of
                    Just obj -> pure $ propertytype2string $ unsafePartial fromJust $ head obj
                    -- Finally, if no role instance has a property, we return the empty string.
                    -- This perspective will only make it to the client to be shown as a Create button, so we
                    -- will not miss the identifying property.
                    Nothing -> pure ""

    propNames :: RoleInstanceWithProperties -> Array String
    propNames {objectStateBasedProperties} = propertytype2string <$> objectStateBasedProperties

-- Construct a map from PropertyType to Array PropertyVerb.
verbsPerProperty :: Array PropertyVerbs -> Array PropertyType -> Object (Array PropertyVerb)
verbsPerProperty pvArr allProps = verbsPerProperty' pvArr allProps empty

verbsPerProperty' :: Array PropertyVerbs -> Array PropertyType -> Object (Array PropertyVerb) -> Object (Array PropertyVerb)
verbsPerProperty' pvArr allProps verbsPerPropMap = foldl
  (\vPerPropMap (PropertyVerbs props verbs) -> let
    (theseProps :: Array PropertyType) = expandPropSet allProps props
    (theseVerbs :: Array PropertyVerb) = expandVerbs verbs
    in
      foldl (\theMap property -> let
        prop = propertytype2string property
        in case lookup prop theMap of
          Nothing -> insert prop theseVerbs theMap
          Just verbs' -> insert prop (verbs' `union` theseVerbs) theMap)
          vPerPropMap
          theseProps)
  verbsPerPropMap
  pvArr

-- | Returns all properties available on the roles in the range of the query function description
-- | (the perspective's object).
serialiseProperties :: QueryFunctionDescription -> Array PropertyVerbs -> MonadPerspectives (Array SerialisedProperty)
serialiseProperties object pverbs = do
  allProps <- allProperties (roleInContext2Role <$> (unsafePartial domain2roleType $ range object))
  (x :: Array (Tuple PropertyType (Array PropertyVerb))) <- pure $ concat (expandPropertyVerbs allProps <$> pverbs)
  (y :: Array (Tuple PropertyType (Array PropertyVerb))) <- pure $ case uncons x of
    Just {head, tail} -> foldl add [head] tail
    Nothing -> []
  traverse (\(Tuple pt verbs) -> makeSerialisedProperty pt) y
  where
    -- Replace two Tuples with the same PropertyType with a single Tuple, with the union of their PropertyVerbs.
    add :: Array (Tuple PropertyType (Array PropertyVerb))
      -> (Tuple PropertyType (Array PropertyVerb))
      -> Array (Tuple PropertyType (Array PropertyVerb))
    add cumulator n@(Tuple prop verbs) = case findIndex (eq prop <<< fst) cumulator of
        Nothing -> cons n cumulator
        Just i -> unsafePartial fromJust $ modifyAt i (\(Tuple _ vs) -> (Tuple prop (union vs verbs))) cumulator

makeSerialisedProperty :: PropertyType -> MonadPerspectives SerialisedProperty
makeSerialisedProperty pt = do
  propType <- PROP.getProperty pt
  case propType of
    (PROP.E prop) -> makeSerialisedProperty' prop
    (PROP.C prop) -> makeSerialisedProperty' prop
  where
    makeSerialisedProperty' :: forall r i. PropertyClass r i => r -> MonadPerspectives SerialisedProperty
    makeSerialisedProperty' propType = do
      isFunctional <- PROP.functional propType
      isMandatory <- PROP.mandatory propType
      isCalculated <- PROP.isCalculated propType
      range <- PROP.range propType
      displayName <- translateType (propertytype2string pt)
      pure { id: unwrap $ identifier propType
      , displayName
      , isFunctional
      , isMandatory
      , isCalculated
      , range: show range
      , constrainingFacets: serialisePropertyFacets $ PROP.constrainingFacets propType
      }

    serialisePropertyFacets :: Array PropertyFacet -> PropertyFacets
    serialisePropertyFacets facets = foldl (\pr facet -> case facet of
      MinLength x -> pr {minLength = Just x}
      MaxLength x -> pr {maxLength = Just x}
      Pattern s label -> pr {pattern = Just $ {regex: show s, label}}
      WhiteSpace r -> pr {whiteSpace = Just $ show r}
      Enumeration items -> pr {enumeration = Just items}
      MaxInclusive s -> pr {maxInclusive = Just s}
      MinInclusive s -> pr {minInclusive = Just s}
      MaxExclusive s -> pr {maxExclusive = Just s}
      MinExclusive s -> pr {minExclusive = Just s}
      TotalDigits i -> pr {totalDigits = Just i}
      FractionDigits i -> pr {fractionDigits = Just i})
      { minLength: Nothing
      , maxLength: Nothing
      , pattern: Nothing
      , whiteSpace: Nothing
      , enumeration: Nothing
      , maxInclusive: Nothing
      , maxExclusive: Nothing
      , minInclusive: Nothing
      , minExclusive: Nothing
      , totalDigits: Nothing
      , fractionDigits: Nothing
      }
      facets

-----------------------------------------------------------------------------------------
-- INSTANCES
-----------------------------------------------------------------------------------------
-- | A user may have a perspective on an object role that depends on object state.
roleInstancesWithProperties ::
  Array PropertyType ->
  Array PropertyType ->
  -- PropertyVerbs that are applicable given the context- and subject state.
  Object (Array PropertyVerb) ->
  ContextInstance ->
  Perspective ->
  Array PropertyVerb ->
  AssumptionTracking (Array RoleInstanceWithProperties)
roleInstancesWithProperties allProps contextSubjectStateBasedProps subjectContextStateBasedPropertyVerbs cid (Perspective{object, roleVerbs, propertyVerbs, actions}) restrictingVerbs = do
  (roleGetter :: ContextInstance ~~> RoleInstance) <- lift $ context2role object
  -- These are all instances of the object of the perspective, regardless of their state.
  (roleInstances :: Array RoleInstance) <- runArrayT $ roleGetter cid
  -- Property getters for all properties available given context- and subject state.
  (propertyGetters :: Object Getter) <- fromFoldable <$> for contextSubjectStateBasedProps
    (\propertyType -> do
      getter <- lift $ getDynamicPropertyGetter (propertytype2string propertyType) (roleInContext2Role <$> unsafePartial roleRange object)
      pure $ Tuple (propertytype2string propertyType) getter)
  -- If an instance has no properties at all, we should hide the instance.
  evalStateT
    (catMaybes <$> for roleInstances roleInstanceWithProperties)
    propertyGetters

  where
    roleInstanceWithProperties ::
      RoleInstance ->
      StateT (Object Getter) AssumptionTracking (Maybe RoleInstanceWithProperties)
    roleInstanceWithProperties roleId = do
      (roleStates :: Array StateSpec) <- lift $ map ObjectState <$> (runArrayT $ getActiveRoleStates roleId)
      -- PropertyVerbs based on the states of the RoleInstance.
      (objectStateBasedPropertyVerbs :: Array PropertyVerbs) <- pure $ (concat (catMaybes $ (flip EM.lookup propertyVerbs) <$> roleStates))
      -- RoleVerbs based on the states of the RoleInstance.
      objectStateBasedRoleVerbs <- pure $ show <$> concat (roleVerbList2Verbs <$> (catMaybes $ (flip EM.lookup roleVerbs) <$> roleStates))
      -- PropertyTypes based on the states of the RoleInstance.
      (objectStateBasedProperties :: Array PropertyType) <- pure (concat $ expandPropSet allProps <<< (\(PropertyVerbs props _) -> props) <$> objectStateBasedPropertyVerbs)
      -- Add getters that we not yet have
      void $ for objectStateBasedProperties (\propertyType -> do
        getters <- get
        if isNothing (lookup (propertytype2string propertyType) getters)
          then do
            getter <- lift $ lift $ getDynamicPropertyGetter
              (propertytype2string propertyType)
              (roleInContext2Role <$> (unsafePartial roleRange object))
            put (insert (propertytype2string propertyType) getter getters)
          else pure unit)
      -- Get all getters from state.
      (allGetters :: Object Getter) <- get
      -- Add verbs based on state of the role instance to the map of
      -- property to verbs based on context- and subject state.
      (localVerbsPerProperty :: Object (Array PropertyVerb)) <- pure $ verbsPerProperty' objectStateBasedPropertyVerbs allProps subjectContextStateBasedPropertyVerbs
      -- Apply for each property available based on context, subject or object
      -- state, the getter to the role instance.
      -- The first Tuple member is the string representation of the property id.
      (valuesAndVerbs :: Array (Tuple String ValuesWithVerbs)) <- for (objectStateBasedProperties <> contextSubjectStateBasedProps)
        \propertyType -> do
          getter <- pure $ unsafePartial fromJust $ lookup (propertytype2string propertyType) allGetters
          vals <- lift (runArrayT $ getter roleId)
          pure $ Tuple (propertytype2string propertyType)
            { values: unwrap <$> vals
            , propertyVerbs: show <$> intersect restrictingVerbs (unsafePartial fromJust $ lookup (propertytype2string propertyType) localVerbsPerProperty)}
      kind <- lift $ lift (roleType_ roleId >>= roleKindOfRoleType <<< ENR)
      filler <- lift $ lift $ binding_ roleId
      publicUrl <-  case kind of 
        -- Only report a result for ContextRole kinds. 
        ContextRole -> (do
          meRole <- lift $ lift $ binding_ roleId
          case meRole of 
            Nothing -> pure Nothing
            -- We'll look for public roles in the (type of the) context of the filler of the ContextRole instance.
            Just erole -> do
              ctxt <- lift $ lift (erole ##>> context)
              murl <- lift $ lift $ getPublicUrl ctxt
              case murl of 
                Nothing -> pure Nothing
                Just url -> do 
                  schemeLess <- lift $ lift $ guid (unwrap erole)
                  pure $ Just (createPublicIdentifier url schemeLess))
        _ -> pure Nothing
      cType <- lift $ lift (cid ##>> contextType)
      translatedActions <- lift $ lift (fromFoldable <$> for (nub $ concat (keys <$> (catMaybes $ (flip EM.lookup actions) <$> roleStates)))
        \actionName -> do 
          translatedActionName <- translateType (qualifyWith (unwrap cType) actionName)
          pure $ Tuple actionName translatedActionName)
      pure $ Just
        { roleId: (unwrap roleId)
        , objectStateBasedRoleVerbs
        , propertyValues: fromFoldable valuesAndVerbs
        , actions: translatedActions
        , objectStateBasedProperties
        , publicUrl
        , filler
      }

-- | The verbs in this type contain both those based on context- and subject state,
-- | and those based on object state.

type PropertyGetter =
  { propertyName :: String              -- property id
  , getter :: Getter
  }

type Getter = RoleInstance ~~> Value
