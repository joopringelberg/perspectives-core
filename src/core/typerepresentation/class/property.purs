module Perspectives.Representation.Class.Property where

import Control.Monad.Error.Class (throwError)
import Data.Newtype (unwrap)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives, MP)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.PersistentType (getPerspectType)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty, Range)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, EnumeratedRoleType, PropertyType(..))
import Prelude (pure, (>=>), (>>=))

-----------------------------------------------------------
-- PROPERTY TYPE CLASS
-----------------------------------------------------------
class PropertyClass r where
  role :: r -> EnumeratedRoleType
  range :: r -> MonadPerspectives Range
  functional :: r -> MonadPerspectives Boolean
  mandatory :: r -> MonadPerspectives Boolean
  calculation :: r -> QueryFunctionDescription

instance calculatedPropertyPropertyClass :: PropertyClass CalculatedProperty where
  role r = (unwrap r).role
  range = rangeOfCalculatedProperty >=> range
  functional = rangeOfCalculatedProperty >=> functional
  mandatory = rangeOfCalculatedProperty >=> mandatory
  calculation r = (unwrap r).calculation

rangeOfCalculatedProperty :: CalculatedProperty -> MonadPerspectives EnumeratedProperty
rangeOfCalculatedProperty cp = propertyCalculationRange (calculation cp) >>= getPerspectType

rangeOfCalculation :: forall r. PropertyClass r => r -> MonadPerspectives EnumeratedPropertyType
rangeOfCalculation cp = propertyCalculationRange (calculation cp)

rangeOfCalculation_ :: forall r. PropertyClass r => r -> MonadPerspectives EnumeratedProperty
rangeOfCalculation_ cp = propertyCalculationRange (calculation cp) >>= getPerspectType

propertyCalculationRange :: QueryFunctionDescription -> MonadPerspectives EnumeratedPropertyType
propertyCalculationRange cd = case cd of
  SQD _ _ (PDOM p) -> pure p
  UQD _ _ _ (PDOM p) -> pure p
  BQD _ _ _ _ (PDOM p) -> pure p
  otherwise -> throwError (error ("range of calculation of a property is not an enumerated property."))

instance enumeratedPropertyPropertyClass :: PropertyClass EnumeratedProperty where
  role r = (unwrap r).role
  range r = pure (unwrap r).range
  functional r = pure (unwrap r).functional
  mandatory r = pure (unwrap r).mandatory
  calculation r = SQD (RDOM (role r)) (PropertyGetter "FunctionName" (ENP (identifier r))) (PDOM (identifier r))

effectivePropertyType :: PropertyType -> MonadPerspectives EnumeratedPropertyType
effectivePropertyType (ENP s) = ((getPerspectType s) :: MP EnumeratedProperty) >>= rangeOfCalculation
effectivePropertyType (CP s) = ((getPerspectType s) :: MP CalculatedProperty) >>= rangeOfCalculation
