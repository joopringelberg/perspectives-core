module Perspectives.Query.QueryTypes where

-- | A queryfunction with its domain and range.
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Foreign (unsafeFromForeign, unsafeToForeign)
import Perspectives.Representation.QueryFunction (QueryFunction)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON', writeJSON)

-- | A description of a calculation with its domain and range.
-- | The calculation can be elementary (EQD): then we apply a queryFunction, such as
-- | RolGetter.
-- | It can also be composed. In that case the
-- TODO. BULLSHIT!!
data QueryFunctionDescription = EQD Domain QueryFunction Range | CQD Domain QueryFunctionDescription Range

derive instance genericRepQueryFunctionDescription :: Generic QueryFunctionDescription _

instance eqQueryFunctionDescription :: Eq QueryFunctionDescription where
  eq e1@(EQD _ _ _) e2@(EQD _ _ _)= eq e1 e2
  eq e1@(CQD _ _ _) e2@(CQD _ _ _)= eq e1 e2
  eq _ _ = false

instance writeForeignQueryFunctionDescription :: WriteForeign QueryFunctionDescription where
  writeImpl q = unsafeToForeign (writeJSON q)

instance readForeignQueryFunctionDescription :: ReadForeign QueryFunctionDescription where
  readImpl q = readJSON' (unsafeFromForeign q)

instance showQueryFunctionDescription :: Show QueryFunctionDescription where
  show e@(EQD _ _ _) = show e
  show e@(CQD _ _ _) = show e

-- | The QueryCompilerEnvironment contains the domain of the queryStep. It also holds
-- | an array of variables that have been declared.
data Domain = RDOM EnumeratedRoleType | PDOM EnumeratedPropertyType | CDOM ContextType

type Range = Domain

derive instance genericDomain :: Generic Domain _

instance showDomain :: Show Domain where
  show = genericShow

instance eqDomain :: Eq Domain where
  eq = genericEq
