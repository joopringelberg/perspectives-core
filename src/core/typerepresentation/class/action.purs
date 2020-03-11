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

module Perspectives.Representation.Class.Action where

import Control.Monad.Error.Class (throwError)
import Data.Array (elemIndex)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)

import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Query.QueryTypes (QueryFunctionDescription, Calculation(..))
import Perspectives.Representation.Action (Action(..), Verb)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.PersistentType (getView)
import Perspectives.Representation.SideEffect (SideEffect(..))
import Perspectives.Representation.TypeIdentifiers (ActionType, EnumeratedRoleType, PropertyType, RoleType, ViewType)
import Perspectives.Representation.View (propertyReferences)
import Prelude (pure, ($), (<<<), (<>), (==), (||), (>>=), (<$>), (<*>))

-----------------------------------------------------------
-- ACTION TYPE CLASS
-----------------------------------------------------------
class ActionClass c where
  subject :: c -> EnumeratedRoleType
  verb :: c -> Verb
  object :: c -> RoleType
  indirectObject :: c -> Maybe RoleType
  requiredObjectProperties :: c -> Maybe ViewType
  requiredSubjectProperties :: c -> Maybe ViewType
  requiredIndirectObjectProperties :: c -> Maybe ViewType
  condition :: c -> MonadPerspectives QueryFunctionDescription
  effect :: c -> MonadPerspectives QueryFunctionDescription
  isExecutedByBot :: c -> Boolean
  -- Either the object, or the indirect object of the action must equal the given RoleType.
  providesPerspectiveOnRole :: RoleType -> c -> Boolean
  -- One of the views must contain PropertyType
  providesPerspectiveOnProperty :: PropertyType -> c -> MonadPerspectives Boolean

instance actionActionClass :: ActionClass Action where
  subject = _.subject <<< unwrap
  verb = _.verb <<< unwrap
  object = _.object <<< unwrap
  indirectObject = _.indirectObject <<< unwrap
  requiredObjectProperties = _.requiredObjectProperties <<< unwrap
  requiredSubjectProperties = _.requiredSubjectProperties <<< unwrap
  requiredIndirectObjectProperties = _.requiredIndirectObjectProperties <<< unwrap
  condition r = case (unwrap r).condition of
    Q qd -> pure qd
    otherwise -> throwError (Custom ("Attempt to acces Condition of an Action before the expression has been compiled. This counts as a system programming error." <> (unwrap $ (identifier r :: ActionType))))
  effect (Action{_id, effect:et}) = case et of
    (Just (EF ar)) -> pure ar
    otherwise -> throwError (Custom ("Attempt to access the Effect of an Action before the expression has been compiled. This counts as a system programming error." <> (unwrap $ _id)))
  isExecutedByBot r = (unwrap r).executedByBot
  providesPerspectiveOnRole rt r = rt == (unwrap r).object || (Just rt) == (unwrap r).indirectObject
  providesPerspectiveOnProperty pt r = (||) <$> maybe (pure true) isInView (requiredObjectProperties r) <*> ((||) <$> maybe (pure true) isInView (requiredObjectProperties r) <*> maybe (pure true) isInView (requiredSubjectProperties r))
    where
      isInView :: ViewType -> MonadPerspectives Boolean
      isInView vt = getView vt >>= \v -> pure $ isJust $ elemIndex pt (propertyReferences v)
