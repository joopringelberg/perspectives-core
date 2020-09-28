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

module Perspectives.Checking.Authorization where

import Prelude

import Data.Array (elemIndex, findIndex)
import Data.Either (Either(..))
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Foreign.Object (lookup) as OBJ
import Perspectives.CoreTypes (MonadPerspectives, (##>>))
import Perspectives.Instances.ObjectGetters (roleType, typeOfSubjectOfAction)
import Perspectives.InvertedQuery (InvertedQuery(..), RelevantProperties(..))
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Representation.Action (Verb)
import Perspectives.Representation.Class.PersistentType (getEnumeratedProperty)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, PropertyType(..), RoleType)
import Perspectives.TypesForDeltas (SubjectOfAction)

-- | True iff the user role represented by the SubjectOfAction argument has perspective
-- | on the role type that includes an Action
-- |  * with a view that holds the property,
-- |  * and the given Verb.
roleHasPerspectiveOnPropertyWithVerb :: SubjectOfAction -> RoleInstance -> EnumeratedPropertyType -> Verb -> MonadPerspectives (Either PerspectivesError Boolean)
roleHasPerspectiveOnPropertyWithVerb subject roleInstance property verb = do
  (subjectType :: RoleType) <- typeOfSubjectOfAction subject
  role <- roleInstance ##>> roleType
  EnumeratedProperty{onPropertyDelta} <- getEnumeratedProperty property
  case findIndex
    (\(InvertedQuery {userTypes}) -> case lookup subjectType userTypes of
      Nothing -> false
      Just pAndV -> case OBJ.lookup (show verb) pAndV of
        Nothing -> false
        Just All -> true
        Just (Properties props) -> case elemIndex (ENP property) props of
          Nothing -> false
          _ -> true)
    onPropertyDelta of
      Nothing -> pure $ Left $ UnauthorizedForProperty "Auteur" subjectType role property verb
      otherwise -> pure $ Right true


-- roleHasPerspectiveOnPropertyWithVerb subject roleInstance property verb = do
--   (subjectType :: RoleType) <- typeOfSubjectOfAction subject
--   role <- roleInstance ##>> roleType
--   (actions :: Array ActionType) <- subjectType ###=
--     (filter (getPerspectiveOnObject (ENR role)) (hasVerb verb))
--   if null actions
--     then pure $ Left $ UnauthorizedForProperty "Auteur" subjectType role property verb
--     else do
--       views <- "ignore" ###= (filter ((\_ -> ArrayT $ pure actions) >=> objectView) (hasProperty (ENP property)))
--       if null views
--         -- If no view has been provided to the relevant action, any property is accessible.
--         then pure $ Right true
--         else do
--           views' <- "ignore" ###= (filter (\_ -> ArrayT $ pure views)
--                 (hasProperty (ENP property)))
--           if null views'
--             then pure $ Left $ UnauthorizedForProperty "Auteur" subjectType role property verb
--             else pure $ Right true
