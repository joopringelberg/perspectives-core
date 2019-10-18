module Perspectives.Representation.Class.Property where

import Control.Monad.Error.Class (throwError)
import Data.Newtype (unwrap)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..))
import Perspectives.Query.QueryTypes (range) as QT
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.Calculation (Calculation(..))
import Perspectives.Representation.Class.Identifiable (class Identifiable, identifier)
import Perspectives.Representation.Class.PersistentType (getCalculatedProperty, getEnumeratedProperty)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty, Range)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType, EnumeratedPropertyType, EnumeratedRoleType, PropertyType(..))
import Prelude (pure, (>>=), ($), (<>), bind)

-- TODO. Controleer of de opzet van RoleClass.expandedADT hier van toepassing is.
-----------------------------------------------------------
-- PROPERTY TYPE CLASS
-----------------------------------------------------------
class (Identifiable r i) <= PropertyClass r i | r -> i, i -> r where
  role :: r -> EnumeratedRoleType
  range :: r -> MonadPerspectives Range
  mandatory :: r -> MonadPerspectives Boolean
  functional :: r -> MonadPerspectives Boolean
  calculation :: r -> MonadPerspectives QueryFunctionDescription

instance calculatedPropertyPropertyClass :: PropertyClass CalculatedProperty CalculatedPropertyType where
  role r = (unwrap r).role
  range r = do
    c <- calculation r
    case QT.range c of
      (VDOM rn) -> pure rn
      otherwise -> throwError (error "")
  -- Hoe bepaal je of een Calculated property mandatory is? En wat betekent het?
  -- De betekenis is praktisch: als de property mandatory is, weet je dat er altijd een waarde is (al dan niet berekend).
  -- Een berekende property is alleen mandatory als al zijn componenten dat zijn; anders kan de berekening altijd leeg zijn. Maar dat is dus niet waar: 'exists' levert altijd een waarde op, of zijn argument nu mandatory is of niet.
  mandatory r = pure true
  -- Hoe bepaal je of een Calculated property functional is? En wat betekent het?
  -- Betekenis: je weet of er één of meerdere waarden zijn. Dat kan nuttig zijn als je een GUI opbouwt.
  functional r = pure true
  calculation r = case (unwrap r).calculation of
    Q calc -> pure calc
    otherwise -> throwError (error ("Attempt to acces QueryFunctionDescription of a CalculatedProperty before the expression has been compiled. This counts as a system programming error." <> (unwrap $ (identifier r :: CalculatedPropertyType))))

instance enumeratedPropertyPropertyClass :: PropertyClass EnumeratedProperty EnumeratedPropertyType where
  role r = (unwrap r).role
  range r = pure (unwrap r).range
  functional r = pure (unwrap r).functional
  mandatory r = pure (unwrap r).mandatory
  calculation r = pure $ SQD (RDOM (ST (role r))) (PropertyGetter (ENP (identifier r))) (VDOM (unwrap r).range)

rangeOfPropertyType :: PropertyType -> MonadPerspectives Range
rangeOfPropertyType (ENP pt) = getEnumeratedProperty pt >>= range
rangeOfPropertyType (CP pt) = getCalculatedProperty pt >>= range

data Property = E EnumeratedProperty | C CalculatedProperty
