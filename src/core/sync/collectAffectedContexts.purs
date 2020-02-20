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

module Perspectives.CollectAffectedContexts where

import Control.Monad.AvarMonadAsk (modify) as AA
import Control.Monad.Error.Class (catchError)
import Control.Monad.Reader (lift)
import Data.Array (filterA, head, union)
import Data.Array.NonEmpty (fromArray, toArray)
import Data.Lens (Traversal', Lens', over, preview, traversed)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Symbol (SProxy(..))
import Data.Traversable (for, traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (rol_isMe)
import Perspectives.CoreTypes (type (~~>), MonadPerspectives, MP, MonadPerspectivesTransaction, (##=), (##>>))
import Perspectives.DomeinCache (modifyDomeinFileInCache, retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.Identifiers (deconstructModelName)
import Perspectives.Instances.ObjectGetters (getRole)
import Perspectives.Instances.ObjectGetters (roleType) as OG
import Perspectives.InvertedQuery (InvertedQuery(..))
import Perspectives.Persistent (getPerspectRol)
import Perspectives.Query.Compiler (getHiddenFunction)
import Perspectives.Representation.EnumeratedRole (EnumeratedRoleRecord)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..))
import Perspectives.Sync.AffectedContext (AffectedContext(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.TypesForDeltas (ContextDelta(..), DeltaType(..), RoleBindingDelta(..), RolePropertyDelta(..))
import Prelude (Unit, bind, const, discard, join, not, pure, ($), (<<<), (==), (>=>), (>>=))
import Unsafe.Coerce (unsafeCoerce)

-- | From the ContextDelta, collect affected contexts. Compile all queries that are used to collect
-- | affected context instances, first. Add the contexts to the Transaction.
-- | Adds users for SYNCHRONISATION.
-- | Guarantees RULE TRIGGERING for `context` and `role <Type>` steps.
aisInContextDelta :: ContextDelta -> MonadPerspectivesTransaction ContextDelta
aisInContextDelta (ContextDelta dr@{id, roleType, roleInstance}) = do
  otherUsers <- usersWithPerspectiveOnRoleInstance id roleType roleInstance
  pure $ ContextDelta dr {users = otherUsers}

usersWithPerspectiveOnRoleInstance ::  ContextInstance -> EnumeratedRoleType -> RoleInstance -> MonadPerspectivesTransaction (Array RoleInstance)
usersWithPerspectiveOnRoleInstance id roleType roleInstance = do
  users1 <- do
    contextCalculations <- lift2 $ compileDescriptions _onContextDelta_context roleType
    (for contextCalculations \(InvertedQuery{compilation, userTypes}) -> do
      -- Find all affected contexts, starting from the role instance of the Delta.
      (affectedContexts :: Array ContextInstance) <- lift2 (roleInstance ##= (unsafeCoerce $ unsafePartial $ fromJust compilation) :: RoleInstance ~~> ContextInstance)
      handleAffectedContexts affectedContexts userTypes) >>= pure <<< join
  roleCalculations <- lift2 $ compileDescriptions _onContextDelta_role roleType
  users2 <- for roleCalculations (\(InvertedQuery{compilation, userTypes}) -> do
    -- Find all affected contexts, starting from the role instance of the Delta.
    affectedContexts <- lift2 (id ##= (unsafeCoerce $ unsafePartial $ fromJust compilation) :: ContextInstance ~~> ContextInstance)
    handleAffectedContexts affectedContexts userTypes) >>= pure <<< join
  -- Remove 'me'
  lift $ lift $ filterA (getPerspectRol >=> pure <<< not <<< rol_isMe) (union users1 users2)

-- Adds an AffectedContext to the transaction and returns user instances.
handleAffectedContexts :: Array ContextInstance -> Array EnumeratedRoleType -> MonadPerspectivesTransaction (Array RoleInstance)
handleAffectedContexts affectedContexts userTypes = case fromArray affectedContexts of
  Nothing -> pure []
  Just contextInstances -> do
    addAffectedContexts $ AffectedContext {contextInstances, userTypes}
    (for userTypes \er -> for (toArray contextInstances) \ci -> lift $ lift $ (ci ##= getRole er)) >>= pure <<< join <<< join

addAffectedContexts :: AffectedContext -> MonadPerspectivesTransaction Unit
addAffectedContexts as = lift $ AA.modify \(Transaction r@{affectedContexts}) -> Transaction (r {affectedContexts = union [as] affectedContexts})

-- | Computes the AffectedContexts and adds them to the Transaction.
-- | Adds the users that should receive it, to the Delta.
-- | Adds users for SYNCHRONISATION, guarantees RULE TRIGGERING.
-- | A binding represents two types of step: `binding` and `binder <TypeOfBinder>`
-- | This function finds all queries recorded on the types of the roles represented
-- | by id, the new binding and the old binding, that have such steps.
-- Implementation note: because we accept 'dangling' roles (roles with no context) we catch
-- errors when computing affected contexts. 
aisInRoleDelta :: RoleBindingDelta -> MonadPerspectivesTransaction RoleBindingDelta
aisInRoleDelta (RoleBindingDelta dr@{id, binding, oldBinding, deltaType}) = do
  binderType <- lift2 (id ##>> OG.roleType)
  bindingCalculations <- lift2 $ compileDescriptions _onRoleDelta_binding binderType
  users1 <- for bindingCalculations (\(InvertedQuery{compilation, userTypes}) -> do
    -- Find all affected contexts, starting from the role instance of the Delta.
    affectedContexts <- lift2 $ catchError (id ##= (unsafeCoerce $ unsafePartial $ fromJust compilation) :: RoleInstance ~~> ContextInstance) (pure <<< const [])
    handleAffectedContexts affectedContexts userTypes) >>= pure <<< join
  users2 <- case binding of
    Just bnd -> do
      bindingType <- lift2 (bnd ##>> OG.roleType)
      binderCalculations <- lift2 $ compileDescriptions _onRoleDelta_binder bindingType
      for binderCalculations (\(InvertedQuery{compilation, userTypes, description}) -> do
        -- Find all affected contexts, starting from the role instance of the Delta.
        affectedContexts <- lift2 $ catchError (bnd ##= (unsafeCoerce $ unsafePartial $ fromJust compilation) :: RoleInstance ~~> ContextInstance) (pure <<< const [])
        handleAffectedContexts affectedContexts userTypes) >>= pure <<< join
    Nothing -> pure []

  users3 <- case oldBinding of
    Just bnd | deltaType == Change -> do
      bindingType <- lift2 (bnd ##>> OG.roleType)
      binderCalculations <- lift2 $ compileDescriptions _onRoleDelta_binder bindingType
      for binderCalculations (\(InvertedQuery{compilation, userTypes}) -> do
        -- Find all affected contexts, starting from the role instance of the Delta.
        affectedContexts <- lift2 $ catchError (bnd ##= (unsafeCoerce $ unsafePartial $ fromJust compilation) :: RoleInstance ~~> ContextInstance) (pure <<< const [])
        handleAffectedContexts affectedContexts userTypes) >>= pure <<< join
    otherwise -> pure []

  -- remove 'me'.
  otherUsers <- lift $ lift $ filterA (getPerspectRol >=> pure <<< not <<< rol_isMe) (union users1 (union users2 users3))
  pure $ RoleBindingDelta dr {users = otherUsers}

-- | Adds users for SYNCHRONISATION, guarantees RULE TRIGGERING.
aisInPropertyDelta :: RolePropertyDelta -> MonadPerspectivesTransaction RolePropertyDelta
aisInPropertyDelta (RolePropertyDelta dr@{id, property})= do
  calculations <- lift2 $ compileDescriptions' property
  users <- for calculations (\(InvertedQuery{compilation, userTypes}) -> do
    -- Find all affected contexts, starting from the role instance of the Delta.
    affectedContexts <- lift2 (id ##= (unsafeCoerce $ unsafePartial $ fromJust compilation) :: RoleInstance ~~> ContextInstance)
    handleAffectedContexts affectedContexts userTypes) >>= pure <<< join
  otherUsers <- lift $ lift $ filterA (getPerspectRol >=> pure <<< not <<< rol_isMe) users
  pure $ RolePropertyDelta dr {users = otherUsers}
  where
    compileDescriptions' :: EnumeratedPropertyType -> MonadPerspectives (Array InvertedQuery)
    compileDescriptions' rt@(EnumeratedPropertyType ert) =  do
      modelName <- pure $ (unsafePartial $ fromJust $ deconstructModelName ert)
      (df :: DomeinFile) <- retrieveDomeinFile modelName
      -- Get the AffectedContextCalculations in onPropertyDelta.
      (calculations :: Array InvertedQuery) <- pure $ unsafePartial $ fromJust $ preview (onPropertyDelta rt) df
      -- Compile the descriptions.
      if areCompiled calculations
        then pure calculations
        else do
          compiledCalculations <- traverse compile calculations
          -- Put the compiledCalculations back in the DomeinFile in cache (not in Couchdb!).
          modifyDomeinFileInCache (over (onPropertyDelta rt) (const compiledCalculations)) modelName
          pure compiledCalculations
      where
        onPropertyDelta :: EnumeratedPropertyType -> Traversal' DomeinFile (Array InvertedQuery)
        onPropertyDelta (EnumeratedPropertyType x) = _Newtype <<< prop (SProxy :: SProxy "enumeratedProperties") <<< at x <<< traversed <<< _Newtype <<< prop (SProxy :: SProxy "onPropertyDelta")

-- | Changes the model in cache (but not in Couchdb). For a given lens that retrieves one of onRoleDelta_binder,
-- | onRoleDelta_binding, onContextDelta_role or onContextDelta_context, and an EnumeratedRoleType, compile the
-- | description in the AffectedContextCalculations.
compileDescriptions :: CalculationsLens -> EnumeratedRoleType -> MonadPerspectives (Array InvertedQuery)
compileDescriptions onX rt@(EnumeratedRoleType ert) =  do
  modelName <- pure $ (unsafePartial $ fromJust $ deconstructModelName ert)
  (df :: DomeinFile) <- retrieveDomeinFile modelName
  -- Get the AffectedContextCalculations in onContextDelta_context.
  (calculations :: Array InvertedQuery) <- pure $ unsafePartial $ fromJust $ preview (onDelta rt) df
  -- Compile the descriptions.
  if areCompiled calculations
    then pure calculations
    else do
      compiledCalculations <- traverse compile calculations
      -- Put the compiledCalculations back in the DomeinFile in cache (not in Couchdb!).
      modifyDomeinFileInCache (over (onDelta rt) (const compiledCalculations)) modelName
      pure compiledCalculations
  where
    onDelta :: EnumeratedRoleType -> Traversal' DomeinFile (Array InvertedQuery)
    onDelta (EnumeratedRoleType x) = _Newtype <<< prop (SProxy :: SProxy "enumeratedRoles") <<< at x <<< traversed <<< _Newtype <<< onX

areCompiled :: Array InvertedQuery -> Boolean
areCompiled ar = case head ar of
  Nothing -> true
  Just (InvertedQuery{compilation}) -> isJust compilation

compile :: InvertedQuery -> MP InvertedQuery
compile ac@(InvertedQuery{description, compilation, userTypes}) = case compilation of
  Just c -> pure ac
  Nothing -> do
    c <- getHiddenFunction description
    pure $ InvertedQuery{description, compilation: Just (unsafeCoerce c), userTypes}

_onContextDelta_context :: CalculationsLens
_onContextDelta_context = prop (SProxy :: SProxy "onContextDelta_context")

_onContextDelta_role :: CalculationsLens
_onContextDelta_role = prop (SProxy :: SProxy "onContextDelta_role")

_onRoleDelta_binding :: CalculationsLens
_onRoleDelta_binding = prop (SProxy :: SProxy "onRoleDelta_binding")

_onRoleDelta_binder :: CalculationsLens
_onRoleDelta_binder = prop (SProxy :: SProxy "onRoleDelta_binder")

type CalculationsLens = Lens' EnumeratedRoleRecord (Array InvertedQuery)

lift2 :: forall a. MonadPerspectives a -> MonadPerspectivesTransaction a
lift2 = lift <<< lift
