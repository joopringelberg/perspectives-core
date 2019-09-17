module Perspectives.Query.QueryTypes where

-- | A queryfunction with its domain and range.
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Foreign (unsafeFromForeign, unsafeToForeign)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.QueryFunction (QueryFunction)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON', writeJSON)

-- | A description of a calculation with its domain and range.
data QueryFunctionDescription =
  SQD Domain QueryFunction Range |
  UQD Domain QueryFunction QueryFunctionDescription Range |
  BQD Domain QueryFunction QueryFunctionDescription QueryFunctionDescription Range

range :: QueryFunctionDescription -> Range
range (SQD _ _ r) = r
range (UQD _ _ _ r) = r
range (BQD _ _ _ _ r) = r

derive instance genericRepQueryFunctionDescription :: Generic QueryFunctionDescription _

instance eqQueryFunctionDescription :: Eq QueryFunctionDescription where
  eq d1@(SQD _ _ _) d2@(SQD _ _ _ ) = eq d1 d2
  eq d1@(UQD _ _ _ _) d2@(UQD _ _ _ _) = eq d1 d2
  eq d1@(BQD _ _ _ _ _) d2@(BQD _ _ _ _ _) = eq d1 d2
  eq _ _ = false

instance writeForeignQueryFunctionDescription :: WriteForeign QueryFunctionDescription where
  writeImpl q = unsafeToForeign (writeJSON q)

instance readForeignQueryFunctionDescription :: ReadForeign QueryFunctionDescription where
  readImpl q = readJSON' (unsafeFromForeign q)

instance showQueryFunctionDescription :: Show QueryFunctionDescription where
  show q@(SQD _ _ _)= show q
  show q@(UQD _ _ _ _)= show q
  show q@(BQD _ _ _ _ _)= show q

-- | The QueryCompilerEnvironment contains the domain of the queryStep. It also holds
-- | an array of variables that have been declared.
data Domain = RDOM (ADT EnumeratedRoleType) | PDOM EnumeratedPropertyType | CDOM (ADT ContextType)

sumOfDomains :: Domain -> Domain -> Maybe Domain
sumOfDomains (RDOM a) (RDOM b) = Just (RDOM (SUM [a, b]))
sumOfDomains (PDOM a) (PDOM b) = if (a == b ) then Just (PDOM a) else Nothing
sumOfDomains (CDOM a) (CDOM b) = Just (CDOM (SUM [a, b]))
sumOfDomains _ _ = Nothing

productOfDomains :: Domain -> Domain -> Maybe Domain
productOfDomains (RDOM a) (RDOM b) = Just (RDOM (PROD [a, b]))
productOfDomains (PDOM a) (PDOM b) = if (a == b ) then Just (PDOM a) else Nothing
productOfDomains (CDOM a) (CDOM b) = Just (CDOM (PROD [a, b]))
productOfDomains _ _ = Nothing

type Range = Domain

derive instance genericDomain :: Generic Domain _

instance showDomain :: Show Domain where
  show = genericShow

instance eqDomain :: Eq Domain where
  eq = genericEq
