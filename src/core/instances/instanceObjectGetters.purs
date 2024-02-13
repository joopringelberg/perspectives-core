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

module Perspectives.Instances.ObjectGetters where 

import Control.Monad.Error.Class (try)
import Control.Monad.Writer (lift, tell)
import Control.Plus (empty)
import Data.Array (concat, elemIndex, filter, filterA, findIndex, foldMap, foldl, head, index, length, null, singleton, union)
import Data.FoldableWithIndex (foldWithIndexM)
import Data.Iterable (toArray)
import Data.Map (Map, lookup) as Map
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (class Newtype, ala, unwrap)
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff.AVar (AVar, read)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Foreign.Object (Object, keys, lookup, values)
import LRUCache (rvalues)
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (context_me, context_preferredUserRoleType, context_pspType, context_publicUrl, context_rolInContext, context_rolInContext_, rol_allTypes, rol_binding, rol_context, rol_gevuldeRol, rol_gevuldeRollen, rol_id, rol_properties, rol_pspType)
import Perspectives.ContextRolAccessors (getContextMember, getRolMember)
import Perspectives.CoreTypes (type (~~>), ArrayWithoutDoubles(..), InformedAssumption(..), MP, MonadPerspectives, (##>>))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.Error.Boundaries (handlePerspectContextError', handlePerspectRolError')
import Perspectives.Identifiers (LocalName, deconstructBuitenRol, typeUri2LocalName, typeUri2ModelUri)
import Perspectives.InstanceRepresentation (PerspectRol(..), externalRole, states) as IP
import Perspectives.InstanceRepresentation (PerspectRol)
import Perspectives.InstanceRepresentation.PublicUrl (PublicUrl)
import Perspectives.Instances.Combinators (orElse)
import Perspectives.ModelDependencies (perspectivesUsers)
import Perspectives.Persistence.API (getViewOnDatabase)
import Perspectives.Persistent (entitiesDatabaseName, getPerspectContext, getPerspectEntiteit, getPerspectRol)
import Perspectives.PerspectivesState (roleCache)
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.Class.PersistentType (getContext, getEnumeratedRole)
import Perspectives.Representation.Class.Role (actionsOfRoleType)
import Perspectives.Representation.Context (Context(..)) as CONTEXT
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.Perspective (StateSpec(..)) as SP
import Perspectives.Representation.TypeIdentifiers (ActionIdentifier(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType, StateIdentifier)
import Perspectives.ResourceIdentifiers (isInPublicScheme, takeGuid)
import Perspectives.SetupCouchdb (filledRolesFilter, roleFromContextFilter)
import Prelude (class Show, bind, discard, eq, flip, identity, map, not, pure, show, ($), (&&), (*>), (<$>), (<<<), (<>), (==), (>=>), (>>=), (>>>))

-----------------------------------------------------------
-- FUNCTIONS FROM CONTEXT
-----------------------------------------------------------
-- | Because we never change the ExternalRole of a Context, we have no need
-- | to track it as a dependency.
externalRole :: ContextInstance ~~> RoleInstance
externalRole ci = ArrayT $ (try $ lift $ getContextMember IP.externalRole ci) >>=
  handlePerspectContextError' "externalRole" []
    \erole -> (tell $ ArrayWithoutDoubles [External ci]) *> pure [erole]

getEnumeratedRoleInstances :: EnumeratedRoleType -> (ContextInstance ~~> RoleInstance)
getEnumeratedRoleInstances rn c = ArrayT $ (lift $ try $ getPerspectEntiteit c >>= flip context_rolInContext rn) >>=
  handlePerspectContextError' "getEnumeratedRoleInstances" []
    \(Tuple rn' instances) -> (tell $ ArrayWithoutDoubles [RoleAssumption c (EnumeratedRoleType rn')]) *> pure instances

getEnumeratedRoleInstances_ :: EnumeratedRoleType -> (ContextInstance ~~> RoleInstance)
getEnumeratedRoleInstances_ rn c = ArrayT $ (lift $ try $ getPerspectEntiteit c >>= flip context_rolInContext_ rn) >>=
  handlePerspectContextError' "getEnumeratedRoleInstances_" []
    \instances -> (tell $ ArrayWithoutDoubles [RoleAssumption c rn]) *> pure instances

getUnlinkedRoleInstances :: EnumeratedRoleType -> (ContextInstance ~~> RoleInstance)
getUnlinkedRoleInstances rn c = ArrayT $ try 
  (lift do
    db <- entitiesDatabaseName
    filledRolesInDatabase :: Array RoleInstance <- getViewOnDatabase db "defaultViews/roleFromContext" (Just $ [unwrap rn, takeGuid (unwrap c)])
    filledRolesInCache :: Array RoleInstance <- (do 
      cache <- roleCache
      cachedRoleAvars <- liftAff $ liftEffect $ (rvalues cache >>= toArray)
      cachedRoles <- lift $ traverse read cachedRoleAvars
      pure $ rol_id <$> filter (roleFromContextFilter rn c) cachedRoles
      )
    pure $ filledRolesInDatabase `union` filledRolesInCache)
  >>=
  handlePerspectRolError' "getUnlinkedRoleInstances" []
    \(roles :: Array RoleInstance) -> (tell $ ArrayWithoutDoubles [RoleAssumption c rn]) *> pure roles

-- | Because we never change the type of a Context, we have no real need
-- | to track it as a dependency.
contextType :: ContextInstance ~~> ContextType
contextType cid  = ArrayT $ (lift $ try $ getContextMember (\c -> [context_pspType c]) cid) >>=
  handlePerspectContextError' "contextType" [] (pure <<< identity)

contextType_ :: ContextInstance -> MP ContextType
contextType_ = getContextMember context_pspType

-- TODO. Fix the issue that an unlinked role does not show up for
-- a public context.
-- | For a public context, never returns an instance. 
-- | This is because member 'me' is a purely local optimization to quickly find the users' role.
-- | However, all users share the same global context, so this cannot be done.
getMe :: ContextInstance ~~> RoleInstance
getMe ctxt = ArrayT $ (try $ lift $ getPerspectContext ctxt) >>=
  handlePerspectContextError' "getMe" []
    \c -> do
      tell $ ArrayWithoutDoubles [Me ctxt]
      pure $ maybe [] singleton (context_me c)

getPreferredUserRoleType :: ContextInstance ~~> RoleType
getPreferredUserRoleType ctxt = ArrayT $ (try $ lift $ getPerspectContext ctxt) >>=
  handlePerspectRolError' "getPreferredUserRoleType" []
    \c -> do
      tell $ ArrayWithoutDoubles [Me ctxt]
      pure $ maybe [] singleton (context_preferredUserRoleType c)

getActiveStates :: ContextInstance ~~> StateIdentifier
getActiveStates ci = ArrayT $ (try $ lift $ getContextMember IP.states ci) >>=
  handlePerspectContextError' "getActiveStates" []
    \states -> (tell $ ArrayWithoutDoubles [State ci]) *> pure states

getActiveStates_ :: ContextInstance -> MonadPerspectives (Array StateIdentifier)
getActiveStates_ ci = (try $ getContextMember IP.states ci) >>=
  handlePerspectContextError' "getActiveStates_" [] pure <<< identity

contextIsInState :: StateIdentifier -> ContextInstance -> MonadPerspectives Boolean
contextIsInState stateId ci = getActiveStates_ ci >>= pure <<< isJust <<< elemIndex stateId

-- | Get the ContextAction names for the context and user role instance, depending on the state
-- | of both.
getContextActions :: RoleType -> RoleInstance -> ContextInstance ~~> ActionIdentifier
getContextActions userRoleType userRoleInstance cid = ArrayT do
  (stateActionMap :: Map.Map SP.StateSpec (Object Action)) <- lift $ actionsOfRoleType userRoleType
  (contextStates :: Array StateIdentifier) <- runArrayT $ getActiveStates cid
  (userStates :: Array StateIdentifier) <- runArrayT $ getActiveRoleStates userRoleInstance
  -- Try each state of the subject and each state of the context
  pure $ ActionIdentifier <$> foldl
    (\(cumulatedActions :: Array String) (nextState :: SP.StateSpec) -> case Map.lookup nextState stateActionMap of
      Nothing -> cumulatedActions
      Just (actions :: Object Action) -> cumulatedActions <> keys actions)
    []
    ((SP.ContextState <$> contextStates) <> (SP.SubjectState <$> userStates))

-- | Returns the name of the model that defines the context type as a String Value.
contextModelName :: ContextInstance ~~> Value
contextModelName (ContextInstance cid) = maybe empty (pure <<< Value) (typeUri2ModelUri cid)

indexedContextName :: ContextInstance ~~> Value
indexedContextName = contextType >=> \cType -> ArrayT $ do
  (CONTEXT.Context c) <- lift $ getContext cType
  case c.indexedContext of 
    Nothing -> pure []
    Just i -> pure [Value $ unwrap i]

publicUrl :: ContextInstance -> MonadPerspectives (Maybe PublicUrl)
publicUrl ci = (try $ getContextMember context_publicUrl ci) >>=
  (handlePerspectContextError' "publicUrl" Nothing (pure <<< identity))

-----------------------------------------------------------
-- FUNCTIONS FROM ROLE
-----------------------------------------------------------
-- | The ability to retrieve the Context of a RoleInstance depends on that RoleInstance being a Role of that Context.
context :: RoleInstance ~~> ContextInstance
context rid = ArrayT $ (lift $ try $ getPerspectEntiteit rid) >>= handlePerspectRolError' "context" []
  \(r :: IP.PerspectRol) -> do
  -- See: Implementing the Functional Reactive Pattern for a full justification of not
  -- recording an assumption.
  -- In short: a client who requests the context of rid, must have another request
  -- that yields rid in the first place. This request is dependent on that other
  -- request, client side. This means that, if rid is removed, the client is notified
  -- of that change and consequently is no longer interested in its context.
  tell $ ArrayWithoutDoubles [Context rid]
  pure $ [rol_context r]

context_ :: RoleInstance -> MonadPerspectives (Array ContextInstance)
context_ rid = (try $ getPerspectEntiteit rid) >>=
  handlePerspectRolError' "context_" []
  (pure <<< singleton <<< rol_context)

context' :: RoleInstance -> MonadPerspectives ContextInstance
context' rid = getPerspectEntiteit rid >>=  pure <<< rol_context

binding :: RoleInstance ~~> RoleInstance
binding r = ArrayT $ (lift $ try $ getPerspectEntiteit r) >>=
  handlePerspectRolError' "binding" []
  \(role :: IP.PerspectRol) -> do
    tell $ ArrayWithoutDoubles [Filler r]
    case rol_binding role of
      Nothing -> pure []
      (Just b) -> pure [b]

binding_ :: RoleInstance -> MonadPerspectives (Maybe RoleInstance)
binding_ r = (try $ getPerspectEntiteit r) >>=
  handlePerspectRolError' "binding_" Nothing
    \(role :: IP.PerspectRol) -> do
      case rol_binding role of
        Nothing -> pure Nothing
        (Just b) -> pure $ Just b

-- | Just the fillers that come instances of a particular ContextType.
bindingInContext :: ContextType -> RoleInstance ~~> RoleInstance
bindingInContext cType r = ArrayT do
  (fillers :: Array RoleInstance) <- runArrayT $ binding r
  filterA
    (\filler -> (lift $ try $ getPerspectEntiteit filler) >>=
      handlePerspectRolError' "bindingInContext" false
        \(role :: IP.PerspectRol) -> do
          fillerContextType <-  lift ((rol_context role) ##>> contextType)
          pure (eq cType fillerContextType))
    fillers

bottom :: RoleInstance ~~> RoleInstance
bottom r = ArrayT do
  (bs :: Array RoleInstance) <- runArrayT $ binding r
  case head bs of
    Nothing -> pure [r]
    Just b -> runArrayT $ bottom b

-- | The most deeply nested role in the chain.
bottom_ :: RoleInstance -> MP RoleInstance
bottom_ r = do
  (mbinding :: Maybe RoleInstance) <- binding_ r
  case mbinding of
    Nothing -> pure r
    Just b -> bottom_ b

-- | The TheWorld$PerspectivesUsers bottom in the chain, or nothing
perspectivesUsersRole_ :: RoleInstance -> MP (Maybe RoleInstance)
perspectivesUsersRole_ r = do
  EnumeratedRoleType rt <- roleType_ r
  if rt == perspectivesUsers
    then pure $ Just r
    else do 
      (mbinding :: Maybe RoleInstance) <- binding_ r
      case mbinding of
        Nothing -> pure Nothing
        Just b -> perspectivesUsersRole_ b

-- | From the instance of a Role (fillerId) of any kind, find the instances of the Role of the given
-- | type (filledType) that are filled with it. The type of filledType (EnumeratedRoleType) may
-- | be psp:Context$externalRole.
-- getFilledRoles
getFilledRoles :: ContextType -> EnumeratedRoleType -> (RoleInstance ~~> RoleInstance)
getFilledRoles filledContextType filledType fillerId = ArrayT $ (lift $ try $ getPerspectEntiteit fillerId) >>=
  handlePerspectRolError' "getFilledRoles" []
    \(filler :: IP.PerspectRol) -> case rol_gevuldeRol filler filledContextType filledType of
      {context:ct, role, instances} -> do
        tell $ ArrayWithoutDoubles [FilledRolesAssumption fillerId ct role]
        pure instances

getAllFilledRoles :: RoleInstance -> MonadPerspectives (Array RoleInstance)
getAllFilledRoles rid = (try $ getPerspectEntiteit rid) >>=
  handlePerspectRolError' "getFilledRoles" []
    \(filler :: IP.PerspectRol) -> pure $ concat $ concat (values <$> values (rol_gevuldeRollen filler))


-- | Retrieve from the database all roles filled by this one.
-- | Is especially useful for a public filler, as that carries no inverse administration of the (private) roles it fills.
getFilledRolesFromDatabase :: RoleInstance ~~> RoleInstance
getFilledRolesFromDatabase rid = ArrayT $ try 
  (lift $ getFilledRolesFromDatabase_ rid)
  >>=
  handlePerspectRolError' "getFilledRolesFromDatabase" []
    \(roles :: Array RoleInstance) -> (tell $ ArrayWithoutDoubles [Filler rid]) *> pure roles

getFilledRolesFromDatabase_ :: RoleInstance -> MonadPerspectives (Array RoleInstance)
getFilledRolesFromDatabase_ rid = try 
  (do
    db <- entitiesDatabaseName
    filledRolesInDatabase :: Array RoleInstance <- getViewOnDatabase db "defaultViews/filledRolesView" (Just $ unwrap rid)
    filledRolesInCache :: Array RoleInstance <- (do 
      cache <- roleCache
      cachedRoleAvars :: Array (AVar PerspectRol) <- liftAff $ liftEffect $ (rvalues cache >>= toArray)
      cachedRoles :: Array PerspectRol <- lift $ traverse read cachedRoleAvars
      pure $ rol_id <$> filter (filledRolesFilter rid) cachedRoles
      )
    pure $ filledRolesInDatabase `union` filledRolesInCache
  )
  >>=
  handlePerspectRolError' "getFilledRolesFromDatabase_" []
    \(roles :: Array RoleInstance) -> pure roles

getProperty :: EnumeratedPropertyType -> (RoleInstance ~~> Value)
getProperty pn r = ArrayT $ (lift $ try $ getPerspectEntiteit r) >>=
  handlePerspectRolError' "getProperty" []
    \((IP.PerspectRol{properties}) :: IP.PerspectRol) -> do
      tell $ ArrayWithoutDoubles [Property r pn]
      case (lookup (unwrap pn) properties) of
        Nothing -> pure []
        (Just p) -> pure p

-- | From a Property value getter, create a function that tries to find a value for the property
-- | on the role or its binding, recursively.
makeChainGetter :: (RoleInstance ~~> Value) -> (RoleInstance ~~> Value)
-- A beautiful definition that will not terminate:
-- makeChainGetter getter = disjunction getter (makeChainGetter (binding >=> getter))
makeChainGetter getter r = ArrayT do
  results <- runArrayT $ getter r
  if null results
    then do
      bnd <- runArrayT $ binding r
      case head bnd of
        Nothing -> pure []
        Just b -> runArrayT $ makeChainGetter getter b
    else pure results

-- | Turn a function that returns strings into one that returns Booleans.
makeBoolean :: forall a. (a ~~> Value) -> (a ~~> Boolean)
makeBoolean f = f >>> map (((==) "true") <<< unwrap)

-- | Get the values for the property with the local name that are directly represented on the instance of a rol of type r, including AspectProperties.
-- | E.g. getUnqualifiedProperty "voornaam"
-- | NOTE: only adds an Assumption if a property value with a matching name can be found.
getUnqualifiedProperty :: LocalName -> (RoleInstance ~~> Value)
getUnqualifiedProperty ln r = ArrayT $ (lift $ try $ getPerspectEntiteit r) >>=
  handlePerspectRolError' "getUnqualifiedProperty" []
    \(role@(IP.PerspectRol{properties}) :: IP.PerspectRol) -> case findIndex (test (unsafeRegex (ln <> "$") noFlags)) (keys properties) of
      Nothing -> pure []
      (Just i) -> do
        pn <- pure (unsafePartial $ fromJust (index (keys $ rol_properties role) i))
        tell $ ArrayWithoutDoubles [Property r (EnumeratedPropertyType pn)]
        -- tell [assumption (unwrap r) pn]
        case (lookup pn properties) of
          Nothing -> pure []
          (Just p) -> pure p

-- | Because we never change the type of a Role, we have no real need
-- | to track it as a dependency.
roleType :: RoleInstance ~~> EnumeratedRoleType
roleType = ArrayT <<< lift <<< (getRolMember \r -> [rol_pspType r])

roleType_ :: RoleInstance -> MP EnumeratedRoleType
roleType_ = getRolMember rol_pspType

allRoleTypes_ :: RoleInstance -> MP (Array EnumeratedRoleType)
allRoleTypes_ = getRolMember rol_allTypes

hasType :: EnumeratedRoleType -> RoleInstance ~~> Boolean
hasType rt rid = ArrayT do
  t <- lift $ getRolMember rol_pspType rid
  pure $ [eq t rt]

-- | All the roles that bind the role instance.
allRoleBinders :: RoleInstance ~~> RoleInstance
allRoleBinders r = ArrayT $ (lift $ try $ getPerspectEntiteit r) >>=
  handlePerspectRolError' "allRoleBinders" []
    \((IP.PerspectRol{filledRoles}) :: IP.PerspectRol) ->
      foldWithIndexM
        (\cIndex (vals :: Array RoleInstance) (roleMap :: Object (Array RoleInstance)) -> do
          vals' <- foldWithIndexM
            (\rIndex (cum :: Array RoleInstance) (vals' :: Array RoleInstance) -> do
              tell $ ArrayWithoutDoubles [FilledRolesAssumption r (ContextType cIndex)(EnumeratedRoleType rIndex)]
              pure (cum <> vals'))
            []
            roleMap
          pure (vals <> vals'))
        []
        filledRoles

-- | `isMe` has an internal error boundary. On failure, it returns false.
-- | If the role instance is a public resource, checks the binding regardless of the value of member 'me'.
-- | This is because 'me' is a purely local optimization that is never synchronized, but this fails for 
-- | obvious reasons for public resources.
-- TODO. If the identifier is in the pub: scheme, try to find a local resource with the same GUID.
-- This involves getting the type of the role instance and looking up where I store its instances.
-- If found, apply isMe to it.
-- Otherwise return false.
-- We must do this because the public version will never have a useful version of 'isMe'. Neither will it 
-- bottom out in a role that has a value for 'isMe': it will be public roles all to the bottom.
isMe :: RoleInstance -> MP Boolean
isMe ri = (try $ getPerspectRol ri) >>=
  handlePerspectRolError' "isMe" false
    \(IP.PerspectRol{isMe: me, binding: bnd, pspType}) -> if isInPublicScheme (unwrap ri)
      then case bnd of 
        Nothing -> pure false
        Just b -> isMe b
      else if me
        then pure true
        else case bnd of
          Nothing -> pure false
          Just b -> isMe b

notIsMe :: RoleInstance -> MP Boolean
notIsMe = isMe >=> pure <<< not

type Filler = RoleInstance
type Filled = RoleInstance

-- | Is Filled (ultimately) filled by Filler?
-- | Recursively checks each filler of filled until filler is found or the bottom has been reached.
-- | filled `filledBy_` filler
filledBy_ :: Filled_ -> Filler_ -> MP Boolean
filledBy_ (Filled_ filled) (Filler_ filler) =
  if filled == filler
    then pure true
    else do
      -- NOTE: binding is: get the filler
      mfiller <- binding_ filled
      case mfiller of 
        Nothing -> pure false
        Just nextFilled -> (Filled_ nextFilled) `filledBy_` (Filler_ filler)

-- | filled ##= filledBy filler
-- | equals:
-- | filled `filledBy_` filler
filledBy :: Filler_ -> Filled_ ~~> Boolean
filledBy filler filled = ArrayT $ do
  b <- lift (filled `filledBy_` filler)
  pure [b]

filledByOperator :: (RoleInstance ~~> RoleInstance) ->
  (RoleInstance ~~> RoleInstance) ->
  (RoleInstance ~~> Value)
filledByOperator sourceOfFilledRoles sourceOfFillerRoles = fillsOperator sourceOfFillerRoles sourceOfFilledRoles


-- Filler ##= (fillsCombinator (RoleInstance ~~> Filled)) RoleInstance
fillsCombinator :: (RoleInstance ~~> Filled) ->
  (Filler ~~> Value)
fillsCombinator sourceOfFilledRoles filler = ArrayT do
  (bools :: Array Boolean) <- runArrayT $ (sourceOfFilledRoles >=> filledBy (Filler_ filler) <<< Filled_) filler
  -- If there are no bindingRoles, this function must return false.
  if null bools
    then pure [Value $ show false]
    else pure [Value $ show $ ala Conj foldMap bools]

-- | Is Filled (ultimately) filled by Filler?
-- | Recursively checks each filler of filled until filler is found or the bottom has been reached.
-- | filler `fills_` filled
fills_ :: Filler_ -> Filled_ -> MP Boolean
fills_ = flip filledBy_

-- firstArg `infixedFunction` secondArg

-- | filler ##= fills filled
-- | should read as:
-- | filler `fills` filled
-- | Recursively checks fillers.
fills :: Filler_ -> Filled_ ~~> Boolean
fills filler filled = ArrayT $ do
  b <- lift (filler `fills_` filled)
  pure [b]

newtype Filler_ = Filler_ RoleInstance
derive instance Newtype Filler_ _
instance Show Filler_ where show (Filler_ s) = show s

newtype Filled_  = Filled_ RoleInstance
derive instance Newtype Filled_ _
instance Show Filled_ where show (Filled_ s) = show s
  
-- | A boolean operator for Arc expressions.
-- | Is true if the (first) filler ultimately fills the (first) filled.
fillsOperator :: (RoleInstance ~~> Filled) ->
  (RoleInstance ~~> Filler) ->
  (RoleInstance ~~> Value)
fillsOperator sourceOfFillerRoles sourceOfFilledRoles originRole = ArrayT do
  fillerRoles <- runArrayT (sourceOfFillerRoles originRole)
  filledRoles <- runArrayT (sourceOfFilledRoles originRole)
  case head fillerRoles, head filledRoles of
    Just fillerRole, Just filledRole | length fillerRoles == 1 && length filledRoles == 1 -> do
      result <- (unsafePartial fromJust <<< head) <$> (runArrayT $ (fills (Filler_ fillerRole)) (Filled_ filledRole))
      pure [Value $ show result]
    _, _ -> pure [Value $ show false]

-- Filled ##= (filledByCombinator (RoleInstance ~~> Filler)) RoleInstance
filledByCombinator :: (RoleInstance ~~> Filled) ->
  (Filler ~~> Value)
filledByCombinator sourceOfFillerRoles filled = ArrayT do
  (bools :: Array Boolean) <- runArrayT $ (sourceOfFillerRoles >=> filledBy (Filler_ filled) <<< Filled_) filled
  -- If there are no bindingRoles, this function must return false.
  if null bools
    then pure [Value $ show false]
    else pure [Value $ show $ ala Conj foldMap bools]

-- | Returns all the instances of the same role (including the argument instance!).
siblings :: RoleInstance ~~> RoleInstance
siblings rid = ArrayT $ (lift $ try $ getPerspectRol rid) >>=
  handlePerspectRolError' "siblings" []
    \(IP.PerspectRol{pspType, context:ctxt}) -> runArrayT $ getEnumeratedRoleInstances pspType ctxt

getActiveRoleStates :: RoleInstance ~~> StateIdentifier
getActiveRoleStates ri = ArrayT $ (try $ lift $ getRolMember (_.states <<< unwrap) ri) >>=
  handlePerspectRolError' "getActiveRoleStates" []
    \states -> (tell $ ArrayWithoutDoubles [RoleState ri]) *> pure states

getActiveRoleStates_ :: RoleInstance -> MonadPerspectives (Array StateIdentifier)
getActiveRoleStates_ ci = (try $ getRolMember (_.states <<< unwrap) ci) >>=
  handlePerspectRolError' "getActiveRoleStates_" [] pure <<< identity

roleIsInState :: StateIdentifier -> RoleInstance -> MonadPerspectives Boolean
roleIsInState stateId ri = getActiveRoleStates_ ri >>= pure <<< isJust <<< elemIndex stateId

-- | Returns the name of the model that defines the role type as a String Value.
roleModelName :: RoleInstance ~~> Value
roleModelName (RoleInstance rid) = maybe empty (pure <<< Value) (typeUri2ModelUri rid)

-- | Return the value of the local property "Name", or return the last segment of the role type name.
getRoleName :: RoleInstance ~~> Value
getRoleName = orElse
  (getUnqualifiedProperty "Name")
  (roleType >=> ArrayT <<< pure <<< map Value <<< (maybe [] singleton) <<< typeUri2LocalName <<< deconstructBuitenRol <<< unwrap)

indexedRoleName :: RoleInstance ~~> Value
indexedRoleName = roleType >=> \rType -> ArrayT $ do
  (EnumeratedRole r) <- lift $ getEnumeratedRole rType
  case r.indexedRole of 
    Nothing -> pure []
    Just i -> pure [Value $ unwrap i]

