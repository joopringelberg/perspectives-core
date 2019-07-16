module Perspectives.Representation.Class.Property where

import Control.Monad.Error.Class (throwError)
import Data.Newtype (unwrap)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Query.QueryTypes (QueryFunctionDescription(..), Domain(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.Persistent (CalculatedPropertyType, getPerspectType)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty, Range)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType, PropertyType(..))
import Prelude (pure, (<>), show, (>=>))

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
  range = rangeOfCalculation >=> range
  functional = rangeOfCalculation >=> functional
  mandatory = rangeOfCalculation >=> mandatory
  calculation r = (unwrap r).calculation

rangeOfCalculation :: CalculatedProperty -> MonadPerspectives EnumeratedProperty
rangeOfCalculation cp = case calculation cp of
  QD _ _ (PDOM p) -> getPerspectType p
  otherwise -> throwError (error ("range of calculation of " <> show (identifier cp :: CalculatedPropertyType) <> " is not an enumerated property."))

instance enumeratedPropertyPropertyClass :: PropertyClass EnumeratedProperty where
  role r = (unwrap r).role
  range r = pure (unwrap r).range
  functional r = pure (unwrap r).functional
  mandatory r = pure (unwrap r).mandatory
  calculation r = QD (RDOM (role r)) (PropertyGetter "FunctionName" (ENP (identifier r))) (PDOM (identifier r))
