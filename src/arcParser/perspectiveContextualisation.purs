module Perspectives.Parsing.Arc.PhaseThree.PerspectiveContextualisation where

import Prelude

import Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, filter, foldM, null)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Foreign.Object (insert, values, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.Data.EncodableMap (EncodableMap(..))
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.ExecuteInTopologicalOrder (executeInTopologicalOrder)
import Perspectives.Identifiers (Namespace)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, getsDF, modifyDF, withDomeinFile)
import Perspectives.Query.QueryTypes (RoleInContext(..)) as QT
import Perspectives.Query.QueryTypes (domain2roleType, range)
import Perspectives.Representation.Class.Identifiable (identifier_)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs, StateSpec)
import Perspectives.Representation.TypeIdentifiers (RoleKind(..))
import Perspectives.Representation.Verbs (RoleVerbList)
import Perspectives.Types.ObjectGetters (lessThanOrEqualTo)

contextualisePerspectives :: PhaseThree Unit
contextualisePerspectives = do
  df@{_id} <- lift $ State.gets _.dfr
  -- Take the DomeinFile from PhaseTwoState and temporarily store it in the cache.
  withDomeinFile
    _id
    (DomeinFile df)
    (contextualisePerspectives' df _id)
  where
    -- Notice that only EnumeratedRoles have Aspects.
    contextualisePerspectives' :: DomeinFileRecord -> Namespace -> PhaseThree Unit
    contextualisePerspectives' {enumeratedRoles} ns = void $ executeInTopologicalOrder 
      identifier_
      (\(EnumeratedRole{roleAspects}) -> unwrap <<< _.role <<< unwrap <$> roleAspects)
      (filter (\(EnumeratedRole{kindOfRole}) -> kindOfRole == UserRole) (values enumeratedRoles))
      contextualisePerspectives''
      

    -- Stores the EnumeratedRole in PhaseTwoState.
    contextualisePerspectives'' :: EnumeratedRole -> PhaseThree Unit
    contextualisePerspectives'' (EnumeratedRole r@{perspectives, roleAspects}) = if null roleAspects
      then pure unit
      else do
        -- As contextualising requires other EnumeratedRoles that may by now have been 
        -- changed in PhaseTwoState, always retrieve the aspects again from PhaseTwoState!
        -- Only the current role has not yet been modified and can be used as is.
        aspects <- catMaybes <$> traverse getRole roleAspects
        perspectives' <- traverse (contextualisePerspective aspects) perspectives
        saveRole (EnumeratedRole r {perspectives = perspectives'})
    
    saveRole :: EnumeratedRole -> PhaseThree Unit
    saveRole r = modifyDF \(dfr@{enumeratedRoles}) -> dfr {enumeratedRoles = insert (identifier_ r) r enumeratedRoles}

    getRole :: QT.RoleInContext -> PhaseThree (Maybe EnumeratedRole)
    getRole (QT.RoleInContext{role}) = getsDF \{enumeratedRoles} -> lookup (unwrap role) enumeratedRoles

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
