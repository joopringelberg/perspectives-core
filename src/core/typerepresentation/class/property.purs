module Perspectives.Representation.Class.Property where

import Control.Monad.Error.Class (throwError)
import Data.Newtype (unwrap)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives, MP)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..))
import Perspectives.Query.QueryTypes (range) as QT
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.Class.Identifiable (class Identifiable, identifier)
import Perspectives.Representation.Class.PersistentType (getPerspectType)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty, Range)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType, EnumeratedPropertyType, EnumeratedRoleType, PropertyType(..))
import Prelude (pure, (>=>), (>>=))

-----------------------------------------------------------
-- PROPERTY TYPE CLASS
-----------------------------------------------------------
class (Identifiable r i) <= PropertyClass r i | r -> i, i -> r where
  role :: r -> EnumeratedRoleType
  range :: r -> MonadPerspectives Range
  functional :: r -> MonadPerspectives Boolean
  mandatory :: r -> MonadPerspectives Boolean
  calculation :: r -> QueryFunctionDescription

instance calculatedPropertyPropertyClass :: PropertyClass CalculatedProperty CalculatedPropertyType where
  role r = (unwrap r).role
  range = rangeOfCalculatedProperty >=> range
  functional = rangeOfCalculatedProperty >=> functional
  mandatory = rangeOfCalculatedProperty >=> mandatory
  calculation r = (unwrap r).calculation

propertyCalculationRange :: QueryFunctionDescription -> MonadPerspectives EnumeratedPropertyType
propertyCalculationRange qfd = case QT.range qfd of
  (PDOM p) -> pure p
  otherwise -> throwError (error ("range of calculation of a property is not an enumerated property."))

rangeOfCalculatedProperty :: CalculatedProperty -> MonadPerspectives EnumeratedProperty
rangeOfCalculatedProperty cp = propertyCalculationRange (calculation cp) >>= getPerspectType

rangeOfCalculation_ :: forall r i. PropertyClass r i => r -> MonadPerspectives EnumeratedProperty
rangeOfCalculation_ cp = propertyCalculationRange (calculation cp) >>= getPerspectType

rangeOfCalculation :: forall r i. PropertyClass r i => r -> MonadPerspectives EnumeratedPropertyType
rangeOfCalculation cp = propertyCalculationRange (calculation cp)

instance enumeratedPropertyPropertyClass :: PropertyClass EnumeratedProperty EnumeratedPropertyType where
  role r = (unwrap r).role
  range r = pure (unwrap r).range
  functional r = pure (unwrap r).functional
  mandatory r = pure (unwrap r).mandatory
  calculation r = SQD (RDOM (ST (role r))) (PropertyGetter (ENP (identifier r))) (PDOM (identifier r))

data Property = E EnumeratedProperty | C CalculatedProperty

effectivePropertyType :: PropertyType -> MonadPerspectives EnumeratedPropertyType
effectivePropertyType (ENP s) = ((getPerspectType s) :: MP EnumeratedProperty) >>= rangeOfCalculation
effectivePropertyType (CP s) = ((getPerspectType s) :: MP CalculatedProperty) >>= rangeOfCalculation
