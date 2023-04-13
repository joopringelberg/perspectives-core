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

module Perspectives.DomeinFile

  where

import Control.Monad.State (State, execState, modify)
import Data.Array (cons)
import Data.Eq.Generic (genericEq)
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Show.Generic (genericShow)
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Object (Object, empty, insert, lookup)
import Partial.Unsafe (unsafePartial)
import Persistence.Attachment (class Attachment)
import Perspectives.Couchdb.Revision (class Revision, Revision_, changeRevision, getRev)
import Perspectives.Data.EncodableMap (EncodableMap, addAll, removeAll)
import Perspectives.Data.EncodableMap (empty) as EM
import Perspectives.Identifiers (typeUri2ModelUri)
import Perspectives.InvertedQuery (InvertedQuery)
import Perspectives.Representation.Action (AutomaticAction)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..), InvertedQueryKey)
import Perspectives.Representation.ScreenDefinition (ScreenDefinition, ScreenKey)
import Perspectives.Representation.State (State(..), Notification) as PEState
import Perspectives.Representation.TypeIdentifiers (ContextType, DomeinFileId(..), EnumeratedRoleType, RoleType, StateIdentifier(..))
import Perspectives.Representation.UserGraph (UserGraph(..))
import Perspectives.Representation.View (View)
import Prelude (class Eq, class Show, Unit, bind, pure, unit, void, ($), (<<<))

newtype DomeinFile = DomeinFile DomeinFileRecord

type DomeinFileRecord =
  { _rev :: Revision_
  , _id :: String
  , namespace :: String
  , contexts :: Object Context
  , enumeratedRoles :: Object EnumeratedRole
  , calculatedRoles :: Object CalculatedRole
  , enumeratedProperties :: Object EnumeratedProperty
  , calculatedProperties :: Object CalculatedProperty
  , views :: Object View
  , states :: Object PEState.State
  , arc :: String
  , referredModels :: Array DomeinFileId
  -- Keys are DomeinFileIds.
  , invertedQueriesInOtherDomains :: Object (Array SeparateInvertedQuery)
  , upstreamStateNotifications :: Object (Array UpstreamStateNotification)
  , upstreamAutomaticEffects :: Object (Array UpstreamAutomaticEffect)
  , userGraph :: UserGraph
  , screens :: EncodableMap ScreenKey ScreenDefinition
  }

derive instance genericDomeinFile :: Generic DomeinFile _

derive instance newtypeDomeinFile :: Newtype DomeinFile _

instance encodeDomeinFile :: Encode DomeinFile where
  encode = genericEncode $ defaultOptions --{unwrapSingleConstructors = true}

instance decodeDomeinFile :: Decode DomeinFile where
  decode (json :: Foreign) = do
    rev <- getRev json
    a <- genericDecode defaultOptions json
    pure (changeRevision rev a)

instance showDomeinFile :: Show DomeinFile where
  show = genericShow

instance eqDomeinFile :: Eq DomeinFile where
  eq = genericEq

instance identifiableDomeinFile :: Identifiable DomeinFile DomeinFileId where
  identifier (DomeinFile{_id}) = DomeinFileId _id
  displayName (DomeinFile{_id}) = _id

instance revisionDomeinFile :: Revision DomeinFile where
  rev = _._rev <<< unwrap
  changeRevision s = over DomeinFile (\vr -> vr {_rev = s})

instance Attachment DomeinFile where
  setAttachment d _ = d
  -- TODO. Handle screen attachments here!
  getAttachments _ = Nothing

-------------------------------------------------------------------------------
---- UPSTREAMSTATENOTIFICATION
-------------------------------------------------------------------------------
newtype UpstreamStateNotification = UpstreamStateNotification
  { stateId :: StateIdentifier
  , isOnEntry :: Boolean
  , notification :: PEState.Notification
  , qualifiedUsers :: Array RoleType
  }

derive instance Generic UpstreamStateNotification _
instance Show UpstreamStateNotification where show = genericShow
instance Eq UpstreamStateNotification where eq = genericEq
instance Encode  UpstreamStateNotification where encode = genericEncode defaultOptions
instance Decode  UpstreamStateNotification where decode = genericDecode defaultOptions

-------------------------------------------------------------------------------
---- UPSTREAMAUTOMATICEFFECT
-------------------------------------------------------------------------------
newtype UpstreamAutomaticEffect = UpstreamAutomaticEffect
  { stateId :: StateIdentifier
  , isOnEntry :: Boolean
  , automaticAction :: AutomaticAction
  , qualifiedUsers :: Array RoleType
  }

derive instance Generic UpstreamAutomaticEffect _
instance Show UpstreamAutomaticEffect where show = genericShow
instance Eq UpstreamAutomaticEffect where eq = genericEq
instance Encode  UpstreamAutomaticEffect where encode = genericEncode defaultOptions
instance Decode  UpstreamAutomaticEffect where decode = genericDecode defaultOptions

-------------------------------------------------------------------------------
---- INVERTEDQUERYCOLLECTION
-------------------------------------------------------------------------------
data SeparateInvertedQuery =
  -- Type of the role instance; Type of the context instance to store on; InvertedQuery
  RoleInvertedQuery EnumeratedRoleType TypeName InvertedQuery |                  -- `role` step
  -- Type of the context of the role instance; Type of the role instance to store on; InvertedQuery
  ContextInvertedQuery ContextType TypeName InvertedQuery |                      -- `context` step
  -- Triple keys; Type of the role instance to store on; InvertedQuery
  FilledByInvertedQuery (Array InvertedQueryKey) TypeName InvertedQuery |        -- `filledBy` step
  FillsInvertedQuery (Array InvertedQueryKey) TypeName InvertedQuery |           -- `fills` step
  -- EnumeratedRoleTypes to index with; EnumeratedProperty to store on; InvertedQuery
  OnPropertyDelta (Array EnumeratedRoleType) TypeName InvertedQuery              -- `Value2Role` step.

type TypeName = String

derive instance genericSeparateInvertedQuery :: Generic SeparateInvertedQuery _

instance showSeparateInvertedQuery :: Show SeparateInvertedQuery where
  show = genericShow

derive instance eqSeparateInvertedQuery :: Eq SeparateInvertedQuery

instance encodeSeparateInvertedQuery :: Encode SeparateInvertedQuery where
  encode = genericEncode defaultOptions

instance decodeSeparateInvertedQuery :: Decode SeparateInvertedQuery where
  decode = genericDecode defaultOptions

addInvertedQueryForDomain :: TypeName -> InvertedQuery -> (TypeName -> InvertedQuery -> SeparateInvertedQuery) -> DomeinFileRecord -> DomeinFileRecord
addInvertedQueryForDomain typeName iq collectionConstructor dfr@{invertedQueriesInOtherDomains} = case typeUri2ModelUri typeName of
  Nothing -> dfr
  Just modelName -> let
    invertedQueriesInOtherDomains' = case lookup modelName invertedQueriesInOtherDomains of
      Nothing -> insert modelName [collectionConstructor typeName iq] invertedQueriesInOtherDomains
      Just separateQueries -> insert modelName (cons (collectionConstructor typeName iq) separateQueries) invertedQueriesInOtherDomains
    in
    dfr {invertedQueriesInOtherDomains = invertedQueriesInOtherDomains'}
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

defaultDomeinFileRecord :: DomeinFileRecord
defaultDomeinFileRecord =
  { _rev: Nothing
  , _id: ""
  , namespace: ""
  , contexts: empty
  , enumeratedRoles: empty
  , calculatedRoles: empty
  , enumeratedProperties: empty
  , calculatedProperties: empty
  , views: empty
  , states: empty
  , arc: ""
  , referredModels: []
  , invertedQueriesInOtherDomains: empty
  , upstreamStateNotifications: empty
  , upstreamAutomaticEffects: empty
  , userGraph: UserGraph $ EM.empty
  , screens: EM.empty
}

defaultDomeinFile :: DomeinFile
defaultDomeinFile = DomeinFile defaultDomeinFileRecord

type DomeinFileEnumeratedRoles = Object EnumeratedRole

setRevision :: String -> DomeinFile -> DomeinFile
setRevision vs (DomeinFile dff) = DomeinFile $ dff {_rev = Just vs}

-- | Returns a table with indexed names as key and ContextType as value.
indexedContexts :: DomeinFile -> Object ContextType
indexedContexts (DomeinFile{contexts}) = execState indexedContexts_ empty
  where
    indexedContexts_ :: State (Object ContextType) Unit
    indexedContexts_ = for_ contexts \(Context{_id, indexedContext}) -> case indexedContext of
      Nothing -> pure unit
      Just i -> void $ modify \table -> insert (unwrap i) _id table

-- | Returns a table with indexed names as key and ContextType as value.
indexedRoles :: DomeinFile -> Object EnumeratedRoleType
indexedRoles (DomeinFile{enumeratedRoles}) = execState indexedRoles_ empty
  where
    indexedRoles_ :: State (Object EnumeratedRoleType) Unit
    indexedRoles_ = for_ enumeratedRoles \(EnumeratedRole{_id, indexedRole}) -> case indexedRole of
      Nothing -> pure unit
      Just i -> void $ modify \table -> insert (unwrap i) _id table

addUpstreamNotification :: UpstreamStateNotification -> StateIdentifier -> DomeinFileRecord -> DomeinFileRecord
addUpstreamNotification notification (StateIdentifier s) dfr@{upstreamStateNotifications} = let 
    domeinName = unsafePartial fromJust $ typeUri2ModelUri s
  in 
    dfr 
      {upstreamStateNotifications = case lookup domeinName upstreamStateNotifications of
        Nothing -> insert domeinName [notification] upstreamStateNotifications
        Just ns -> insert domeinName (cons notification ns) upstreamStateNotifications}

addUpstreamAutomaticEffect :: UpstreamAutomaticEffect -> StateIdentifier -> DomeinFileRecord -> DomeinFileRecord
addUpstreamAutomaticEffect effect (StateIdentifier s) dfr@{upstreamAutomaticEffects} = let 
    domeinName = unsafePartial fromJust $ typeUri2ModelUri s
  in 
    dfr 
      {upstreamAutomaticEffects = case lookup domeinName upstreamAutomaticEffects of
        Nothing -> insert domeinName [effect] upstreamAutomaticEffects
        Just ns -> insert domeinName (cons effect ns) upstreamAutomaticEffects}

addDownStreamNotification :: UpstreamStateNotification -> State DomeinFileRecord Unit
addDownStreamNotification = modifyDownstreamNotification true

removeDownStreamNotification :: UpstreamStateNotification -> State DomeinFileRecord Unit
removeDownStreamNotification = modifyDownstreamNotification false

modifyDownstreamNotification :: Boolean -> UpstreamStateNotification -> State DomeinFileRecord Unit
modifyDownstreamNotification add (UpstreamStateNotification{stateId, isOnEntry, notification, qualifiedUsers}) = void $ modify \dfr@{states} ->
  case lookup (unwrap stateId) states of 
    Nothing -> dfr
    Just s -> dfr {states = insert (unwrap stateId) (modifyState s) states}
    where
    modifyState :: PEState.State -> PEState.State
    modifyState (PEState.State sr@{notifyOnEntry, notifyOnExit}) = 
      if isOnEntry 
        then if add
          then PEState.State sr {notifyOnEntry = addAll notification notifyOnEntry qualifiedUsers}
          else PEState.State sr {notifyOnEntry = removeAll notification notifyOnEntry qualifiedUsers}
        else if add
          then PEState.State sr {notifyOnExit = addAll notification notifyOnExit qualifiedUsers}
          else PEState.State sr {notifyOnExit = removeAll notification notifyOnExit qualifiedUsers}

addDownStreamAutomaticEffect :: UpstreamAutomaticEffect -> State DomeinFileRecord Unit
addDownStreamAutomaticEffect = modifyDownstreamAutomaticEffect true

removeDownStreamAutomaticEffect :: UpstreamAutomaticEffect -> State DomeinFileRecord Unit
removeDownStreamAutomaticEffect = modifyDownstreamAutomaticEffect false

modifyDownstreamAutomaticEffect :: Boolean -> UpstreamAutomaticEffect -> State DomeinFileRecord Unit
modifyDownstreamAutomaticEffect add (UpstreamAutomaticEffect{stateId, isOnEntry, automaticAction, qualifiedUsers}) = void $ modify \dfr@{states} ->
  case lookup (unwrap stateId) states of 
    Nothing -> dfr
    Just s -> dfr {states = insert (unwrap stateId) (modifyState s) states}
    where
    modifyState :: PEState.State -> PEState.State
    modifyState (PEState.State sr@{automaticOnEntry, automaticOnExit}) = 
      if isOnEntry 
        then if add
          then PEState.State sr {automaticOnEntry = addAll automaticAction automaticOnEntry qualifiedUsers}
          else PEState.State sr {automaticOnEntry = removeAll automaticAction automaticOnEntry qualifiedUsers}
        else if add
          then PEState.State sr {automaticOnExit = addAll automaticAction automaticOnExit qualifiedUsers}
          else PEState.State sr {automaticOnExit = removeAll automaticAction automaticOnExit qualifiedUsers}

