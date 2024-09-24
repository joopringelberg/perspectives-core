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

module Perspectives.Representation.Class.Property where

import Control.Monad.Error.Class (throwError)
import Control.Plus ((<|>))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Parsing.Arc.AST (PropertyFacet)
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), RoleInContext(..))
import Perspectives.Query.QueryTypes (range) as QT
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.Class.Identifiable (class Identifiable, identifier, identifier_)
import Perspectives.Representation.Class.PersistentType (getCalculatedProperty, getEnumeratedProperty, getPerspectType)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.Range (Range)
import Perspectives.Representation.ThreeValuedLogic (bool2threeValued)
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), EnumeratedPropertyType(..), EnumeratedRoleType, PropertyType(..))
import Perspectives.Types.ObjectGetters (enumeratedRoleContextType)
import Prelude (pure, (>>=), ($), (<>), bind, (>=>), (<<<))

-- TODO. Controleer of de opzet van RoleClass.expandedADT hier van toepassing is.
-----------------------------------------------------------
-- PROPERTY TYPE CLASS
-----------------------------------------------------------
class (Identifiable r i) <= PropertyClass r i | r -> i, i -> r where
  role :: r -> EnumeratedRoleType
  range :: r -> MonadPerspectives Range
  mandatory :: r -> MonadPerspectives Boolean
  functional :: r -> MonadPerspectives Boolean
  selfonly :: r -> MonadPerspectives Boolean
  authoronly :: r -> MonadPerspectives Boolean
  isCalculated :: r -> MonadPerspectives Boolean
  calculation :: r -> MonadPerspectives QueryFunctionDescription
  constrainingFacets :: r -> Array PropertyFacet

instance calculatedPropertyPropertyClass :: PropertyClass CalculatedProperty CalculatedPropertyType where
  role r = (unwrap r).role
  range r = do
    c <- calculation r
    case QT.range c of
      (VDOM rn _) -> pure rn
      otherwise -> throwError (error $ "Non-Value typed result for property " <> identifier_ r)
  -- Hoe bepaal je of een Calculated property mandatory is? En wat betekent het?
  -- De betekenis is praktisch: als de property mandatory is, weet je dat er altijd een waarde is (al dan niet berekend).
  -- Een berekende property is alleen mandatory als al zijn componenten dat zijn; anders kan de berekening altijd leeg zijn. Maar dat is dus niet waar: 'exists' levert altijd een waarde op, of zijn argument nu mandatory is of niet.
  mandatory r = pure true
  -- Hoe bepaal je of een Calculated property functional is? En wat betekent het?
  -- Betekenis: je weet of er één of meerdere waarden zijn. Dat kan nuttig zijn als je een GUI opbouwt.
  functional r = pure true
  authoronly r = pure false
  selfonly r = pure false
  isCalculated _ = pure true
  calculation r = case (unwrap r).calculation of
    Q calc -> pure calc
    otherwise -> throwError (error ("Attempt to acces QueryFunctionDescription of a CalculatedProperty before the expression has been compiled. This counts as a system programming error. For property: " <> (unwrap $ (identifier r :: CalculatedPropertyType))))
  constrainingFacets r = []

instance enumeratedPropertyPropertyClass :: PropertyClass EnumeratedProperty EnumeratedPropertyType where
  role r = (unwrap r).role
  range r = pure (unwrap r).range
  functional r = pure (unwrap r).functional
  mandatory r = pure (unwrap r).mandatory
  selfonly r = pure (unwrap r).selfonly
  authoronly r = pure (unwrap r).authoronly
  isCalculated _ = pure false
  calculation r = do
    context <- enumeratedRoleContextType (role r)
    pure $ SQD (RDOM (ST $ RoleInContext {context, role: (role r)})) (PropertyGetter (ENP (identifier r))) (VDOM (unwrap r).range (Just $ ENP (identifier r))) (bool2threeValued (unwrap r).functional) (bool2threeValued (unwrap r).mandatory)
  constrainingFacets r = (unwrap r).constrainingFacets

rangeOfPropertyType :: PropertyType -> MonadPerspectives Range
rangeOfPropertyType (ENP pt) = getEnumeratedProperty pt >>= range
rangeOfPropertyType (CP pt) = getCalculatedProperty pt >>= range

data Property = E EnumeratedProperty | C CalculatedProperty

-----------------------------------------------------------
-- FUNCTIONS ON PROPERTY
-----------------------------------------------------------
getCalculation :: Property -> MonadPerspectives QueryFunctionDescription
getCalculation (E r) = calculation r
getCalculation (C r) = calculation r

-----------------------------------------------------------
-- FUNCTIONS ON PROPERTYTYPE
-----------------------------------------------------------
getProperty :: PropertyType -> MonadPerspectives Property
getProperty (ENP e) = getPerspectType e >>= pure <<< E
getProperty (CP c) = getPerspectType c >>= pure <<< C

propertyTypeIsFunctional :: PropertyType -> MonadPerspectives Boolean
propertyTypeIsFunctional = getProperty >=> (case _ of
  E r -> functional r
  C r -> functional r)

propertyTypeIsMandatory :: PropertyType -> MonadPerspectives Boolean
propertyTypeIsMandatory = getProperty >=> (case _ of
  E r -> mandatory r
  C r -> mandatory r)

propertyTypeIsSelfOnly :: PropertyType -> MonadPerspectives Boolean
propertyTypeIsSelfOnly = getProperty >=> (case _ of
  E r -> selfonly r
  C r -> selfonly r)

propertyTypeIsAuthorOnly :: PropertyType -> MonadPerspectives Boolean
propertyTypeIsAuthorOnly = getProperty >=> (case _ of
  E r -> authoronly r
  C r -> authoronly r)

propertyTypeIsCalculated :: PropertyType -> MonadPerspectives Boolean
propertyTypeIsCalculated = getProperty >=> (case _ of
  E r -> pure true
  C r -> pure false)

-----------------------------------------------------------
-- FUNCTIONS ON STRING
-----------------------------------------------------------
getPropertyType :: String -> MonadPerspectives PropertyType
getPropertyType s = ((getEnumeratedProperty $ EnumeratedPropertyType s) >>= pure <<< ENP <<< identifier)
  <|> ((getCalculatedProperty $ CalculatedPropertyType s) >>= pure <<< CP <<< identifier)
