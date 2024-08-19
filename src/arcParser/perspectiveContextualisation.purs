module Perspectives.Parsing.Arc.PhaseThree.PerspectiveContextualisation where

import Prelude

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.State (StateT, evalStateT, execStateT, get, gets, modify, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Array (catMaybes, concat, cons, difference, filter, filterA, findIndex, foldM, fromFoldable, intercalate, length, many, modifyAt, null)
import Data.Array.Partial (tail, head) as AP
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (Map, empty, fromFoldable, insert, lookup, values) as Map
import Data.Map (toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for_, traverse)
import Data.Tuple (Tuple(..), snd)
import Foreign.Object (Object, insert, lookup, values, fromFoldable) as OBJ
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes ((###=), MP)
import Perspectives.Data.EncodableMap (EncodableMap(..))
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.ExecuteInTopologicalOrder (executeInTopologicalOrder)
import Perspectives.Identifiers (Namespace, buitenRol, deconstructBuitenRol, typeUri2LocalName_, isExternalRole, startsWithSegments)
import Perspectives.Instances.Combinators (closure)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, getsDF, modifyDF, withDomeinFile)
import Perspectives.Query.QueryTypes (Domain(..), RoleInContext, domain2roleInContext, domain2roleType, range, replaceRange, roleInContext2Role)
import Perspectives.Query.QueryTypes (RoleInContext(..)) as QT
import Perspectives.Representation.ADT (ADT, allLeavesInADT, equalsOrGeneralises_)
import Perspectives.Representation.Class.Identifiable (identifier_)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.Class.Role (allLocalAliases, toConjunctiveNormalForm_)
import Perspectives.Representation.Context (Context(..)) as CONTEXT
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..), StateSpec)
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedPropertyType, EnumeratedRoleType(..), PropertyType(..), RoleKind(..), RoleType(..), externalRoleType, propertytype2string, roletype2string)
import Perspectives.Representation.Verbs (RoleVerbList)
import Perspectives.Types.ObjectGetters (allEnumeratedRoles, aspectRoles, aspectsOfRole, equalsOrSpecialisesRoleInContext)

contextualisePerspectives :: PhaseThree Unit
contextualisePerspectives = do
  df@{id, namespace} <- lift $ gets _.dfr
  -- Take the DomeinFile from PhaseTwoState and temporarily store it in the cache.
  withDomeinFile
    id
    (DomeinFile df)
    (contextualisePerspectives' df namespace) 
  where
    -- Notice that only EnumeratedRoles have Aspects (this includes external roles).
    contextualisePerspectives' :: DomeinFileRecord -> Namespace -> PhaseThree Unit
    contextualisePerspectives' {enumeratedRoles} namespace = void $ executeInTopologicalOrder 
      identifier_
      -- Only count aspects defined in this namespace as dependencies for the sorting!
      (\(EnumeratedRole{roleAspects}) -> filter (flip startsWithSegments namespace) (unwrap <<< _.role <<< unwrap <$> roleAspects))
      (filter (\(EnumeratedRole{kindOfRole}) -> kindOfRole == UserRole) (OBJ.values enumeratedRoles))
      contextualisePerspectives''
      
    -- The EnumeratedRole has kind UserRole by virtue of the filtering above.
    -- Stores the EnumeratedRole in PhaseTwoState.
    contextualisePerspectives'' :: EnumeratedRole -> PhaseThree EnumeratedRole
    contextualisePerspectives'' role@(EnumeratedRole r@{perspectives, roleAspects, context}) = if null roleAspects
      then pure role
      else do
        -- As contextualising requires other EnumeratedRoles that may by now have been 
        -- changed in PhaseTwoState, always retrieve the aspects again from PhaseTwoState!
        -- Only the current role has not yet been modified and can be used as is (because of topological ordering).
        userRoleAspects <- catMaybes <$> traverse getRole roleAspects
        -- Enrich the user role's perspectives with the perspectives of his aspects.
        perspectives' <- traverse (contextualisePerspective userRoleAspects) perspectives
        -- The roles in the context that are taken as is from context aspects.
        aspectObjectRolesInContext <- lift $ lift (context ###= aspectRoles)
        -- If an Aspect user role has a perspective on an (Aspect) role that has
        -- been added _as is_ to the user's context, add that perspective to the 
        -- user's perspectives.
        aspectPerspectives <- lift $ lift $ execWriterT (for_ userRoleAspects (writePerspectiveOnAddedRole aspectObjectRolesInContext))
        rolesWithAspects <- collectRolesWithAspects context
        -- If an Aspect user role has a perspective on an Aspect Object that is 
        -- specialised in the context, where the user does not have a perspective on that 
        -- specialisation, contextualise the perspective:
        --    * replace its object with the specialised role(s)
        --    * apply property mappings.
        -- and add it to the user role.
        -- We can safely use `perspectives` instead of the enriched `perspectives'`, because we only use the object and that hasn't changed.
        contextualisedAspectPerspectives <- lift $ lift $ execWriterT (for_ userRoleAspects (writeContextualisedPerspective context perspectives rolesWithAspects))
        
        saveRole (EnumeratedRole r {perspectives = perspectives' <> aspectPerspectives <> contextualisedAspectPerspectives})
    
    saveRole :: EnumeratedRole -> PhaseThree EnumeratedRole
    saveRole r = (modifyDF \(dfr@{enumeratedRoles}) -> dfr {enumeratedRoles = OBJ.insert (identifier_ r) r enumeratedRoles}) *> pure r

    getRole :: QT.RoleInContext -> PhaseThree (Maybe EnumeratedRole)
    getRole (QT.RoleInContext{role}) = do
      mRole <- (getsDF \{enumeratedRoles} -> OBJ.lookup (unwrap role) enumeratedRoles) 
      case mRole of 
        Nothing -> lift $ lift $ Just <$> getEnumeratedRole role
        _ -> pure mRole

    -- Write an aspect user role perspective (unmodified) if its object generalises (or is equal to) one of the aspect roles that were added to the context (potentalObjects).
    writePerspectiveOnAddedRole :: Array EnumeratedRoleType -> EnumeratedRole -> WriterT (Array Perspective) MP Unit
    writePerspectiveOnAddedRole potentialObjects (EnumeratedRole{perspectives:aspectUserPerspectives}) =
      for_ aspectUserPerspectives \(p@(Perspective{object:aspectUserPerspectiveObject})) -> do
        -- Include fillers in the expansion.
        aspectPerspectivesObjectDNF <- lift $ toConjunctiveNormalForm_ (unsafePartial domain2roleInContext $ range aspectUserPerspectiveObject)
        found <- not <<< null <$> filterA (lift <<< (\(potentialObject :: EnumeratedRoleType) -> do 
          expandedPotentialObject <- getEnumeratedRole potentialObject >>= pure <<< _.completeType <<< unwrap 
          -- expandedAspectPerspectivesObject -> expandedPotentialObject
          pure (aspectPerspectivesObjectDNF `equalsOrGeneralises_` expandedPotentialObject)
          )) potentialObjects
        if found
          then tell [p]
          else pure unit

    -- Write an aspect user role perspective (contextualised) if its object is specialised in the context,
    -- but only if the user role's own perspectives do not cover that specialisation.
    writeContextualisedPerspective :: ContextType -> Array Perspective -> RolesWithAspects -> EnumeratedRole -> WriterT (Array Perspective) MP Unit
    writeContextualisedPerspective context ownPerspectives rolesWithAspects (EnumeratedRole{perspectives:aspectUserPerspectives}) = for_ aspectUserPerspectives
      \(Perspective precord@{object, propertyVerbs}) -> if null (allLeavesInADT (roleInContext2Role <$> (unsafePartial domain2roleInContext $ range object)) `difference` (concat <$> fromFoldable $ Map.values rolesWithAspects) )
        -- all role types in the aspect object can be substituted with role types in the context we contextualise in.
        then do
          (substitutions :: Array Substitution) <- pure $ createSubstitutions rolesWithAspects
          for_ substitutions \substitution -> do
            (contextualisedObject :: ADT RoleInContext) <- pure $ contextualiseADT (unsafePartial domain2roleInContext $ range object) context substitution
            (roleTypes :: Array RoleType) <- pure (ENR <$> allLeavesInADT (roleInContext2Role <$> contextualisedObject))
            covered <- lift $ coveredByOwnPerspectives contextualisedObject
            if covered
              then pure unit
              else do
                propertyVerbs' <- lift $ applyPropertyMapping contextualisedObject propertyVerbs
                tell [Perspective precord 
                  { object = replaceRange object (RDOM contextualisedObject)
                  , roleTypes = roleTypes
                  , displayName = (intercalate ", " (typeUri2LocalName_ <<< roletype2string <$> roleTypes))
                  -- Apply property mappings found in the contextualisedObject ADT to the propertyVerbs.
                  , propertyVerbs = propertyVerbs'
                  }]
        else pure unit

      where
        -- Retrieve the property mappings in the ADT and replace any propertytype in the PropertyVerbs with its local destination property.
        applyPropertyMapping :: ADT RoleInContext -> EncodableMap StateSpec (Array PropertyVerbs) -> MP (EncodableMap StateSpec (Array PropertyVerbs))
        applyPropertyMapping adt pverbs = do
          (aliases :: OBJ.Object EnumeratedPropertyType) <- OBJ.fromFoldable <$> allLocalAliases (roleInContext2Role <$> adt)
          pure $ pverbs <#> \(pverbArr :: Array PropertyVerbs) -> pverbArr <#> (\(PropertyVerbs ptypeset pverbset) -> 
            PropertyVerbs (ptypeset <#> \propType -> case OBJ.lookup (propertytype2string propType) aliases of
                Nothing -> propType
                Just et -> ENP et)
              pverbset)

        -- None of the perspectives in `ownPerspectives` (those of the specialised user role) is on an object (whose range) equals (or is a generalisation of, 
        -- which is unlikely to happen) the contextualised object ADT.
        coveredByOwnPerspectives :: ADT QT.RoleInContext -> MP Boolean
        coveredByOwnPerspectives objectADT = foldM
          (\found (ownPerspectiveObjectADT :: ADT RoleInContext) -> if found
            then pure true
            -- Does ownPerspectiveObjectADT cover objectADT?
            -- objectADT -> ownPerspectiveObjectADT
            -- e.g. objectADT is an Aspect of ownPerspectiveObjectADT, or fills it.
            else objectADT `equalsOrSpecialisesRoleInContext` ownPerspectiveObjectADT)
          false
          (ownPerspectives <#> (\(Perspective{object}) -> unsafePartial domain2roleInContext $ range object))

        -- notCoveredByOwnPerspectives objectADT = isNothing $ find
        --   (\(ownPerspectiveObjectADT :: ADT RoleInContext) -> ownPerspectiveObjectADT `equalsOrGeneralisesRoleInContext` objectADT)
        --   (ownPerspectives <#> (\(Perspective{object}) -> unsafePartial domain2roleInContext $ range object))

        createSubstitutions :: RolesWithAspects -> Array Substitution
        createSubstitutions rwas = case runExcept $ evalStateT (many createSubstitution) (Just (toUnfoldable rwas)) of
          Left _ -> []
          Right ss -> ss

          where
          createSubstitution :: StateT (Maybe (Array (Tuple EnumeratedRoleType (Array EnumeratedRoleType)))) (Except String) Substitution
          createSubstitution = do
            x <- get
            case x of 
              Nothing -> throwError "We're done"
              Just (x' :: (Array (Tuple EnumeratedRoleType (Array EnumeratedRoleType)))) -> do
                case findIndex ((>) 1 <<< length <<< snd) x' of
                  -- In this case, none of the substitution arrays holds more than one element. 
                  -- That means that the current substitution is the last one.
                  Nothing -> put Nothing
                  -- In this case, the substitution Tuple at i holds more than one element.
                  -- We'll pop that element and store the resulting substitutions in state.
                  Just i -> put $ modifyAt i (\(Tuple e as) -> Tuple e (unsafePartial AP.tail as)) x'
                -- Create a map from the array of tuples consisting of the role type and its first replacement
                pure $ Map.fromFoldable $ (\(Tuple e as) -> Tuple (unsafePartial AP.head as) e) <$> x'

    contextualisePerspective :: Array EnumeratedRole -> Perspective -> PhaseThree Perspective
    contextualisePerspective aspects p = foldM contextualiseWithAspect p aspects

      where
        contextualiseWithAspect :: Perspective -> EnumeratedRole -> PhaseThree Perspective
        contextualiseWithAspect perspective (EnumeratedRole{perspectives:aspectPerspectives}) = 
          foldM contextualiseWithAspectPerspective perspective aspectPerspectives

        contextualiseWithAspectPerspective :: Perspective -> Perspective -> PhaseThree Perspective
        contextualiseWithAspectPerspective 
          perspective@(Perspective r@{object, roleVerbs, propertyVerbs, actions}) 
          (Perspective{actions:aspectActions, object:aspectObject, roleVerbs:aspectRoleVerbs, propertyVerbs:aspectPropertyVerbs}) = do
          -- is the object of the perspective a specialisation of the object of the aspect perspective?
          -- aspectObject -> object
          isASpecialisation <- lift $ lift ((unsafePartial domain2roleType $ range object) `equalsOrSpecialisesRoleInContext` (unsafePartial domain2roleType $ range aspectObject))
          if isASpecialisation 
            then 
              pure $ Perspective r 
                { roleVerbs = addAspectRoleVerbs roleVerbs aspectRoleVerbs
                , propertyVerbs = addAspectPropertyVerbs propertyVerbs aspectPropertyVerbs
                , actions = actions <> aspectActions
                }
            -- We cannot establish here that the aspect perspective should be added to the role.
            else pure perspective
        -- In this situation we have established that the context is a specialisation of the aspect context, the
        -- subject role is a specialisation of the aspect subject role, and the 
        -- object role is a specialisation of the aspect object role.
        -- So for any type of state (context, subject, or object), an aspect resource state applies to the specialised resource as well.
        -- When we serialise a perspective, we combine the RoleVerbs from all active states.
        -- So all we have to do here is to copy entries from the aspectRoleVerbMap to the roleVerbMap for keys that are not in the roleVerbMap,
        -- and add the verbs for keys that are.
        -- When we serialise a perspective, we combine the PropertyVerbs from all active states, too. 
        -- So we can handle the PropertyVerbs in the same way.
        addAspectRoleVerbs :: EncodableMap StateSpec RoleVerbList -> EncodableMap StateSpec RoleVerbList -> EncodableMap StateSpec RoleVerbList
        addAspectRoleVerbs = combineMaps

        addAspectPropertyVerbs :: EncodableMap StateSpec (Array PropertyVerbs) 
          -> EncodableMap StateSpec (Array PropertyVerbs) 
          -> EncodableMap StateSpec (Array PropertyVerbs)
        addAspectPropertyVerbs = combineMaps
        
        combineMaps :: forall v. Semigroup v => EncodableMap StateSpec v -> EncodableMap StateSpec v -> EncodableMap StateSpec v
        combineMaps (EncodableMap roleMap) (EncodableMap aspectRoleMap) = EncodableMap $ foldlWithIndex
          (\stateSpec resultingMap roleValue -> case Map.lookup stateSpec resultingMap of
            Nothing -> Map.insert stateSpec roleValue resultingMap
            Just value -> Map.insert stateSpec (roleValue <> value) resultingMap
          )
          roleMap
          aspectRoleMap

type AspectRole = EnumeratedRoleType
type RolesWithAspects = Map.Map EnumeratedRoleType (Array AspectRole)

-- A Map of EnumeratedRoleTypes in the context (including the external role) and each of their Aspect roles.
collectRolesWithAspects :: ContextType -> PhaseThree RolesWithAspects
collectRolesWithAspects ct = do
  allEnumeratedRolesInContext <- lift $ lift (ct ###= allEnumeratedRoles)
  execStateT (for_ (cons (externalRoleType ct) allEnumeratedRolesInContext) getAspects) Map.empty
  where
    getAspects :: EnumeratedRoleType -> StateT RolesWithAspects PhaseThree Unit
    getAspects erole = do
      aspects <- lift $ lift $ lift (erole ###= closure aspectsOfRole)
      if null aspects
        then pure unit
        else void $ modify \m -> Map.insert erole aspects m

-- Replace the first type (Aspect role) by the second type (role in context that is being contextualised).
type Substitution = Map.Map EnumeratedRoleType EnumeratedRoleType

contextualiseADT :: ADT QT.RoleInContext -> ContextType -> Substitution -> ADT QT.RoleInContext
contextualiseADT adt context substitution = adt <#> \ric@(QT.RoleInContext{role}) -> case Map.lookup role substitution of
  -- This should not happen.
  Nothing -> ric
  Just s -> QT.RoleInContext{context, role: s}

----------------------------------------------------------------------------------------
------- ADDASPECTSTOEXTERNALROLES
----------------------------------------------------------------------------------------

addAspectsToExternalRoles :: PhaseThree Unit
addAspectsToExternalRoles = do
  df@{enumeratedRoles, contexts} <- lift $ gets _.dfr
  enumeratedRoles' <- pure ((addAspectToExternalRole contexts) <$> enumeratedRoles)
  modifyDF \dfr -> dfr {enumeratedRoles = enumeratedRoles'}
  where
    addAspectToExternalRole :: OBJ.Object CONTEXT.Context -> EnumeratedRole -> EnumeratedRole
    addAspectToExternalRole ctxts erole@(EnumeratedRole erecord@{id}) = if isExternalRole (unwrap id)
      then case OBJ.lookup (deconstructBuitenRol (unwrap id)) ctxts of
        -- we can safely ignore this case: it is not going to happen.
        Nothing -> erole
        Just (CONTEXT.Context{contextAspects}) -> EnumeratedRole $ erecord {roleAspects = (\context@(ContextType aspect) -> QT.RoleInContext{context, role: EnumeratedRoleType $ buitenRol aspect}) <$> contextAspects}
      else erole

