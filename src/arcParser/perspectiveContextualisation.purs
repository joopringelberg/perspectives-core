module Perspectives.Parsing.Arc.PhaseThree.PerspectiveContextualisation where

import Prelude

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.State (StateT, evalStateT, execStateT, get, gets, modify, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Array (catMaybes, concat, cons, difference, filter, find, findIndex, foldM, fromFoldable, intercalate, length, many, modifyAt, null)
import Data.Array.Partial (tail, head) as AP
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (Map, empty, fromFoldable, insert, lookup, values) as Map
import Data.Map (toUnfoldable)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Newtype (unwrap)
import Data.Traversable (for_, traverse)
import Data.Tuple (Tuple(..), snd)
import Foreign.Object (Object, insert, lookup, values)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes ((###=))
import Perspectives.Data.EncodableMap (EncodableMap(..))
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.ExecuteInTopologicalOrder (executeInTopologicalOrder)
import Perspectives.Identifiers (Namespace, buitenRol, deconstructBuitenRol, deconstructLocalName_, isExternalRole)
import Perspectives.Instances.Combinators (closure)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, getsDF, modifyDF, withDomeinFile)
import Perspectives.Query.QueryTypes (Domain(..), RoleInContext, domain2roleInContext, domain2roleType, range, replaceRange, roleInContext2Role)
import Perspectives.Query.QueryTypes (RoleInContext(..)) as QT
import Perspectives.Representation.ADT (ADT(..), allLeavesInADT, equalsOrGeneralisesADT)
import Perspectives.Representation.Class.Identifiable (identifier_)
import Perspectives.Representation.Context (Context(..)) as CONTEXT
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs, StateSpec)
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..), RoleKind(..), RoleType(..), externalRoleType, roletype2string)
import Perspectives.Representation.Verbs (RoleVerbList)
import Perspectives.Types.ObjectGetters (allEnumeratedRoles, aspectRoles, aspectsOfRole, lessThanOrEqualTo)

contextualisePerspectives :: PhaseThree Unit
contextualisePerspectives = do
  df@{_id} <- lift $ gets _.dfr
  -- Take the DomeinFile from PhaseTwoState and temporarily store it in the cache.
  withDomeinFile
    _id
    (DomeinFile df)
    (contextualisePerspectives' df _id)
  where
    -- Notice that only EnumeratedRoles have Aspects (this includes external roles).
    contextualisePerspectives' :: DomeinFileRecord -> Namespace -> PhaseThree Unit
    contextualisePerspectives' {enumeratedRoles} ns = void $ executeInTopologicalOrder 
      identifier_
      (\(EnumeratedRole{roleAspects}) -> unwrap <<< _.role <<< unwrap <$> roleAspects)
      (filter (\(EnumeratedRole{kindOfRole}) -> kindOfRole == UserRole) (values enumeratedRoles))
      contextualisePerspectives''
      
    -- The EnumeratedRole has kind UserRole by virtue of the filtering above.
    -- Stores the EnumeratedRole in PhaseTwoState.
    contextualisePerspectives'' :: EnumeratedRole -> PhaseThree Unit
    contextualisePerspectives'' (EnumeratedRole r@{perspectives, roleAspects, context}) = if null roleAspects
      then pure unit
      else do
        -- As contextualising requires other EnumeratedRoles that may by now have been 
        -- changed in PhaseTwoState, always retrieve the aspects again from PhaseTwoState!
        -- Only the current role has not yet been modified and can be used as is.
        userRoleAspects <- catMaybes <$> traverse getRole roleAspects
        -- Enrich the user role's perspectives with the perspectives of his aspects.
        perspectives' <- traverse (contextualisePerspective userRoleAspects) perspectives
        -- The roles in the context that are taken as is from context aspects.
        aspectObjectRolesInContext <- lift $ lift (context ###= aspectRoles)
        -- If an Aspect user role has a perspective on an (Aspect) role that has
        -- been added as is to the user's context, add that perspective to the 
        -- user's perspectives.
        aspectPerspectives <- pure $ execWriter (for_ userRoleAspects (writePerspectiveOnAddedRole aspectObjectRolesInContext))
        rolesWithAspects <- collectRolesWithAspects context
        -- If an Aspect user role has a perspective on an Aspect Object that is 
        -- specialised in the context, where the user does not have a perspective on that 
        -- specialisation, contextualise the perspective (replace its object with the specialised role(s))
        -- and add it to the user role.
        contextualisedAspectPerspectives <- pure $ execWriter (for_ userRoleAspects (writeContextualisedPerspective context perspectives rolesWithAspects))
        
        saveRole (EnumeratedRole r {perspectives = perspectives' <> aspectPerspectives <> contextualisedAspectPerspectives})
    
    saveRole :: EnumeratedRole -> PhaseThree Unit
    saveRole r = modifyDF \(dfr@{enumeratedRoles}) -> dfr {enumeratedRoles = insert (identifier_ r) r enumeratedRoles}

    getRole :: QT.RoleInContext -> PhaseThree (Maybe EnumeratedRole)
    getRole (QT.RoleInContext{role}) = getsDF \{enumeratedRoles} -> lookup (unwrap role) enumeratedRoles

    -- Write an aspect user role perspective (unmodified) if its object generalises (or is equal to) one of the aspect roles that were added to the context (potentalObjects).
    writePerspectiveOnAddedRole :: Array EnumeratedRoleType -> EnumeratedRole -> Writer (Array Perspective) Unit
    writePerspectiveOnAddedRole potentialObjects (EnumeratedRole{perspectives:aspectUserPerspectives}) = do
      for_ aspectUserPerspectives \(p@(Perspective{object:aspectUserPerspectiveObject})) -> if isJust $ find 
          (\potentialObject -> (roleInContext2Role <$> (unsafePartial domain2roleInContext $ range aspectUserPerspectiveObject)) `equalsOrGeneralisesADT` ST potentialObject) 
          potentialObjects
        then tell [p]
        else pure unit

    -- Write an aspect user role perspective (contextualised) if its object is specialised in the context,
    -- but only if the user role's own perspectives do not cover that specialisation.
    writeContextualisedPerspective :: ContextType -> Array Perspective -> RolesWithAspects -> EnumeratedRole -> Writer (Array Perspective) Unit
    writeContextualisedPerspective context ownPerspectives rolesWithAspects (EnumeratedRole{perspectives:aspectUserPerspectives}) = for_ aspectUserPerspectives
      \(Perspective precord@{object}) -> if null (allLeavesInADT (roleInContext2Role <$> (unsafePartial domain2roleInContext $ range object)) `difference` (concat <$> fromFoldable $ Map.values rolesWithAspects) )
        -- all role types in the aspect object can be substituted with role types in the context we contextualise in.
        then do
          (substitutions :: Array Substitution) <- pure $ createSubstitutions rolesWithAspects
          for_ substitutions \substitution -> let
            (contextualisedObject :: ADT RoleInContext) = contextualiseADT (unsafePartial domain2roleInContext $ range object) context substitution
            (roleTypes :: Array RoleType) = (ENR <$> allLeavesInADT (roleInContext2Role <$> contextualisedObject))
            in 
              if notCoveredByOwnPerspectives contextualisedObject
                then 
                  tell [Perspective precord 
                    { object = replaceRange object (RDOM contextualisedObject)
                    , roleTypes = roleTypes
                    , displayName = (intercalate ", " (deconstructLocalName_ <<< roletype2string <$> roleTypes))
                    }]
                else pure unit
        else pure unit

      where
        -- None of the perspectives in `ownPerspectives` (those of the specialised user role) is on an object (whose range) equals (or is a generalisation of, 
        -- which is unlikely to happen) the contextualised object ADT.
        notCoveredByOwnPerspectives :: ADT QT.RoleInContext -> Boolean
        notCoveredByOwnPerspectives objectADT = isNothing $ find
          (\ownPerspectiveObjectADT -> ownPerspectiveObjectADT `equalsOrGeneralisesADT` objectADT)
          (ownPerspectives <#> (\(Perspective{object}) -> unsafePartial domain2roleInContext $ range object))

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
          perspective@(Perspective r@{object, roleVerbs, propertyVerbs}) 
          (Perspective{object:aspectObject, roleVerbs:aspectRoleVerbs, propertyVerbs:aspectPropertyVerbs}) = do
          -- is the object of the perspective a specialisation of the object of the aspect perspective?
          isASpecialisation <- lift $ lift ((unsafePartial domain2roleType $ range aspectObject) `lessThanOrEqualTo` (unsafePartial domain2roleType $ range object))
          if isASpecialisation 
            then 
              -- add aspect RoleVerbs (EncodableMap StateSpec RoleVerbList)
              pure $ Perspective r 
                { roleVerbs = addAspectRoleVerbs roleVerbs aspectRoleVerbs
                , propertyVerbs = addAspectPropertyVerbs propertyVerbs aspectPropertyVerbs
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
    addAspectToExternalRole :: Object CONTEXT.Context -> EnumeratedRole -> EnumeratedRole
    addAspectToExternalRole ctxts erole@(EnumeratedRole erecord@{_id}) = if isExternalRole (unwrap _id)
      then case lookup (deconstructBuitenRol (unwrap _id)) ctxts of
        -- we can safely ignore this case: it is not going to happen.
        Nothing -> erole
        Just (CONTEXT.Context{contextAspects}) -> EnumeratedRole $ erecord {roleAspects = (\context@(ContextType aspect) -> QT.RoleInContext{context, role: EnumeratedRoleType $ buitenRol aspect}) <$> contextAspects}
      else erole