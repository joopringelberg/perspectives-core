module Perspectives.Representation.Calculation where

-- | The Calculation data structure represents two phases in the transformation of the surface syntax for
-- | [Espressions](Perspectives.Parsing.Arc.Expression.AST.html#t:Step):
-- |   * the parse tree, in terms of the Step data structure
-- |   * the QueryFunctionDescription (Perspectives.Query.QueryTypes), the format that can be compiled to an executable function.
-- | Representing the two phases cannot be avoided because we can only compile the parse tree to a QueryFunctionDescription
-- | after the model file has been otherwise completely transformed. This is because we check, while compiling the Step
-- | AST, whether the range of a step complies with the domain of the following step.

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Kishimen (genericSumToVariant)
import Perspectives.Parsing.Arc.Expression.AST (Step)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)
import Prelude (class Eq, class Show, (<<<))
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)


data Calculation = S Step | Q QueryFunctionDescription
derive instance genericRepVerb :: Generic Calculation _
instance writeForeignVerb :: WriteForeign Calculation where
  writeImpl = writeImpl <<< genericSumToVariant
instance readForeignVerb :: ReadForeign Calculation where
  readImpl f = readImpl f
instance showVerb :: Show Calculation where
  show = genericShow
instance eqVerb :: Eq Calculation where
  eq = genericEq
