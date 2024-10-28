-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2021 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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

module Perspectives.Representation.Action where

import Prelude

import Control.Alt ((<|>)) 
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Perspectives.Query.QueryTypes (QueryFunctionDescription(..), domain, functional, range)
import Perspectives.Repetition (Duration, Repeater)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.ThreeValuedLogic as THREE
import Simple.JSON (class ReadForeign, class WriteForeign, read', writeImpl)

data AutomaticAction = 
  ContextAction (TimeFacets 
    ( effect :: QueryFunctionDescription )
    )
  | 
  RoleAction (TimeFacets 
    ( currentContextCalculation :: QueryFunctionDescription
    , effect :: QueryFunctionDescription )
    )

type TimeFacets f = 
  { startMoment :: Maybe Duration
    , endMoment :: Maybe Duration
    , repeats :: Repeater
    | f
  }

effectOfAction :: AutomaticAction -> QueryFunctionDescription
effectOfAction (ContextAction {effect}) = effect
effectOfAction (RoleAction action) = action.effect

derive instance genericAutomaticAction :: Generic AutomaticAction _
instance showAutomaticAction :: Show AutomaticAction where show = genericShow
instance eqAutomaticAction :: Eq AutomaticAction where eq = genericEq

instance WriteForeign AutomaticAction where
  writeImpl (ContextAction r) = writeImpl { constructor: "ContextAction", r}
  writeImpl (RoleAction r) = writeImpl { constructor: "RoleAction", r}

instance ReadForeign AutomaticAction where
  readImpl f = 
    -- order matters here!
    do 
      {r} :: {r :: TimeFacets ( effect :: QueryFunctionDescription, currentContextCalculation :: QueryFunctionDescription )} <- read' f
      pure $ RoleAction r
    <|>
    do 
      {r} :: {r :: TimeFacets ( effect :: QueryFunctionDescription )} <- read' f
      pure $ ContextAction r

newtype Action = Action QueryFunctionDescription
derive instance genericAction :: Generic Action _
derive instance newtypeAction :: Newtype Action _
instance showAction :: Show Action where show = genericShow
instance eqAction :: Eq Action where eq = genericEq

derive newtype instance WriteForeign Action
derive newtype instance ReadForeign Action

instance Semigroup Action where
  append (Action qfd1) (Action qfd2)= Action $ makeSequence qfd1 qfd2
    where
    makeSequence :: QueryFunctionDescription -> QueryFunctionDescription -> QueryFunctionDescription
    makeSequence left right = BQD (domain left) (BinaryCombinator SequenceF) left right (range right) (THREE.and (functional left) (functional right)) (THREE.or (functional left) (functional right))
