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

module Perspectives.Representation.Class.Action where

import Control.Monad.Error.Class (throwError)
import Data.Array (difference, elemIndex, null)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Query.QueryTypes (Calculation(..), QueryFunctionDescription, domain2roleType, range)
import Perspectives.Representation.ADT (ADT)
import Perspectives.Representation.Action (Action(..), Verb)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.PersistentType (getView)
import Perspectives.Representation.Class.Role (adtOfRoleAndBinding, getRole, leavesInADT)
import Perspectives.Representation.SideEffect (SideEffect(..))
import Perspectives.Representation.TypeIdentifiers (ActionType, EnumeratedRoleType, PropertyType, RoleType, ViewType)
import Perspectives.Representation.View (propertyReferences)
import Prelude (pure, ($), (<<<), (<>), (||), (>>=), (<$>), (<*>))

-----------------------------------------------------------
-- ACTION TYPE CLASS
-----------------------------------------------------------
class ActionClass c where
  subject :: c -> RoleType
  verb :: c -> Verb
  object :: c -> Calculation
  objectQfd :: c -> MonadPerspectives QueryFunctionDescription
  objectType :: c -> MonadPerspectives (ADT EnumeratedRoleType)
  indirectObject :: c -> Maybe RoleType
  requiredObjectProperties :: c -> Maybe ViewType
  requiredSubjectProperties :: c -> Maybe ViewType
  requiredIndirectObjectProperties :: c -> Maybe ViewType
  condition :: c -> MonadPerspectives QueryFunctionDescription
  effect :: c -> MonadPerspectives QueryFunctionDescription
  isExecutedByBot :: c -> Boolean
  -- | The object of the action must cover the given RoleType in the sense that all EnumeratedRoleTypes in the
  -- | ADT of the latter must occur in that of the former.
  providesPerspectiveOnRole :: RoleType -> c -> MonadPerspectives Boolean
  -- One of the views must contain PropertyType
  providesPerspectiveOnProperty :: PropertyType -> c -> MonadPerspectives Boolean

instance actionActionClass :: ActionClass Action where
  subject = _.subject <<< unwrap
  verb = _.verb <<< unwrap
  object = _.object <<< unwrap
  objectQfd r = case object r of
    Q calc -> pure calc
    S _ -> throwError (error ("Attempt to acces Object of an Action before the expression has been compiled. This counts as a system programming error." <> (unwrap $ (identifier r :: ActionType))))
  objectType r = objectQfd r >>= pure <<< unsafePartial domain2roleType <<< range
  -- objectType r = case object r of
  --   Q calc -> pure $ unsafePartial domain2roleType $ range calc
  --   S _ -> throwError (error ("Attempt to acces Condition of an Action before the expression has been compiled. This counts as a system programming error." <> (unwrap $ (identifier r :: ActionType))))
  indirectObject = _.indirectObject <<< unwrap
  requiredObjectProperties = _.requiredObjectProperties <<< unwrap
  requiredSubjectProperties = _.requiredSubjectProperties <<< unwrap
  requiredIndirectObjectProperties = _.requiredIndirectObjectProperties <<< unwrap
  condition r = case (unwrap r).condition of
    Q qd -> pure qd
    otherwise -> throwError (error ("Attempt to acces Condition of an Action before the expression has been compiled. This counts as a system programming error." <> (unwrap $ (identifier r :: ActionType))))
  effect (Action{_id, effect:et}) = case et of
    (Just (EF ar)) -> pure ar
    otherwise -> throwError (error ("Attempt to access the Effect of an Action before the expression has been compiled. This counts as a system programming error." <> (unwrap $ _id)))
  isExecutedByBot r = (unwrap r).executedByBot
  providesPerspectiveOnRole rt r = null <$> (difference <$> (leavesInADT <$> objectType r) <*> (leavesInADT <$> (getRole rt >>= adtOfRoleAndBinding)))
  providesPerspectiveOnProperty pt r = (||) <$> maybe (pure true) isInView (requiredObjectProperties r) <*> ((||) <$> maybe (pure true) isInView (requiredIndirectObjectProperties r) <*> maybe (pure true) isInView (requiredSubjectProperties r))
    where
      isInView :: ViewType -> MonadPerspectives Boolean
      isInView vt = getView vt >>= \v -> pure $ isJust $ elemIndex pt (propertyReferences v)
