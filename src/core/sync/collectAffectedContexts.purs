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
import Control.Monad.Reader (lift)
import Data.Array (cons, head, union)
import Data.Array.NonEmpty (fromArray, toArray)
import Data.Lens (Traversal', Lens', over, preview, traversed)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Symbol (SProxy(..))
import Data.Traversable (for, traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~>), MonadPerspectives, MP, MonadPerspectivesTransaction, (##=), (##>>))
import Perspectives.DomeinCache (modifyDomeinFileInCache, retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.Identifiers (deconstructModelName)
import Perspectives.Instances.ObjectGetters (getRole)
import Perspectives.Instances.ObjectGetters (roleType) as OG
import Perspectives.InvertedQuery (InvertedQuery(..))
import Perspectives.Query.Compiler (getHiddenFunction)
import Perspectives.Representation.EnumeratedRole (EnumeratedRoleRecord)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..))
import Perspectives.Sync.AffectedContext (AffectedContext(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.TypesForDeltas (ContextDelta(..), RolePropertyDelta(..), RoleBindingDelta(..))
import Prelude (Unit, bind, const, discard, join, pure, ($), (<<<), (>>=))
import Unsafe.Coerce (unsafeCoerce)

-- | From the ContextDelta, collect affected contexts. Compile all queries that are used to collect
-- | affected context instances, first. Add the contexts to the Transaction.
aisInContextDelta :: ContextDelta -> MonadPerspectivesTransaction ContextDelta
aisInContextDelta (ContextDelta dr@{id, roleType, roleInstance}) = do
  users1 <- case roleInstance of
    Just ri -> do
      contextCalculations <- lift2 $ compileDescriptions _onContextDelta_context roleType
      (for contextCalculations \(InvertedQuery{compilation, userTypes}) -> do
        -- Find all affected contexts, starting from the role instance of the Delta.
        (affectedContexts :: Array ContextInstance) <- lift2 (ri ##= (unsafeCoerce $ unsafePartial $ fromJust compilation) :: RoleInstance ~~> ContextInstance)
        handleAffectedContexts affectedContexts userTypes) >>= pure <<< join
    Nothing -> pure []
  roleCalculations <- lift2 $ compileDescriptions _onContextDelta_role roleType
  users2 <- for roleCalculations (\(InvertedQuery{compilation, userTypes}) -> do
    -- Find all affected contexts, starting from the role instance of the Delta.
    affectedContexts <- lift2 (id ##= (unsafeCoerce $ unsafePartial $ fromJust compilation) :: ContextInstance ~~> ContextInstance)
    handleAffectedContexts affectedContexts userTypes) >>= pure <<< join
  pure $ ContextDelta dr {users = union users1 users2}

handleAffectedContexts :: Array ContextInstance -> Array EnumeratedRoleType -> MonadPerspectivesTransaction (Array RoleInstance)
handleAffectedContexts affectedContexts userTypes = case fromArray affectedContexts of
  Nothing -> pure []
  Just contextInstances -> do
    addAffectedContexts $ AffectedContext {contextInstances, userTypes}
    (for userTypes \er -> for (toArray contextInstances) \ci -> lift $ lift $ (ci ##= getRole er)) >>= pure <<< join <<< join

addAffectedContexts :: AffectedContext -> MonadPerspectivesTransaction Unit
addAffectedContexts as = lift $ AA.modify \(Transaction r@{affectedContexts}) -> Transaction (r {affectedContexts = cons as affectedContexts})

aisInRoleDelta :: RoleBindingDelta -> MonadPerspectivesTransaction RoleBindingDelta
aisInRoleDelta (RoleBindingDelta dr@{id, binding}) = do
  binderType <- lift2 (id ##>> OG.roleType)
  bindingCalculations <- lift2 $ compileDescriptions _onRoleDelta_binding binderType
  users1 <- for bindingCalculations (\(InvertedQuery{compilation, userTypes}) -> do
    -- Find all affected contexts, starting from the role instance of the Delta.
    affectedContexts <- lift2 (id ##= (unsafeCoerce $ unsafePartial $ fromJust compilation) :: RoleInstance ~~> ContextInstance)
    handleAffectedContexts affectedContexts userTypes) >>= pure <<< join

  users2 <- case binding of
    Just bnd -> do
      bindingType <- lift2 (bnd ##>> OG.roleType)
      binderCalculations <- lift2 $ compileDescriptions _onRoleDelta_binding bindingType
      for binderCalculations (\(InvertedQuery{compilation, userTypes}) -> do
        -- Find all affected contexts, starting from the role instance of the Delta.
        affectedContexts <- lift2 (bnd ##= (unsafeCoerce $ unsafePartial $ fromJust compilation) :: RoleInstance ~~> ContextInstance)
        handleAffectedContexts affectedContexts userTypes) >>= pure <<< join
    Nothing -> pure []
  pure $ RoleBindingDelta dr {users = union users1 users2}

aisInPropertyDelta :: RolePropertyDelta -> MonadPerspectivesTransaction RolePropertyDelta
aisInPropertyDelta (RolePropertyDelta dr@{id, property})= do
  calculations <- lift2 $ compileDescriptions' property
  users <- for calculations (\(InvertedQuery{compilation, userTypes}) -> do
    -- Find all affected contexts, starting from the role instance of the Delta.
    affectedContexts <- lift2 (id ##= (unsafeCoerce $ unsafePartial $ fromJust compilation) :: RoleInstance ~~> ContextInstance)
    handleAffectedContexts affectedContexts userTypes) >>= pure <<< join
  pure $ RolePropertyDelta dr {users = users}
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
