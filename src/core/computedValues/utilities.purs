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

-- | This module defines External Core functions for model://perspectives.domains#Couchdb.

module Perspectives.Extern.Utilities where

import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Data.Array (head, singleton)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..))
import Data.String (replace) as String
import Data.String.Regex (regex, replace) as REGEX
import Data.String.Regex.Flags (noFlags)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Perspectives.CoreTypes (MonadPerspectivesQuery)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Identifiers (getFirstMatch)
import Perspectives.Instances.ObjectGetters (context, contextType, roleType)
import Perspectives.Instances.Values (parseInt)
import Perspectives.Parsing.Arc.Expression (step)
import Perspectives.Parsing.Arc.Expression.AST (Step)
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseTwoDefs (evalPhaseTwo')
import Perspectives.Parsing.Messages (MultiplePerspectivesErrors)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Query.ExpandPrefix (expandPrefix)
import Perspectives.Query.ExpressionCompiler (compileExpression)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription, RoleInContext(..))
import Perspectives.Query.UnsafeCompiler (compileFunction)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Prelude (class Show, bind, pure, show, ($), (<<<), (<>), (>=>), (>>=))
import Text.Parsing.Parser (ParseError)
import Unsafe.Coerce (unsafeCoerce)

-- TODO: verander naar echte gegenereerde identifiers.
genSym :: RoleInstance -> MonadPerspectivesQuery String
genSym _ = pure "geheim"

-- | Returns a random number in the closed interval [l;u].
random :: Array String -> Array String -> RoleInstance -> MonadPerspectivesQuery Value
random lower_ upper_ _ = ArrayT case head lower_, head upper_ of
  Just l, Just u -> do 
    lowerBound <- try $ liftAff $ parseInt l
    upperBound <- try $ liftAff $ parseInt u
    case lowerBound, upperBound of 
      Right lower, Right upper -> (liftEffect (randomInt lower upper)) >>= pure <<< singleton <<< Value <<< show
      _, _ -> pure []
  _, _ -> pure []

roleIdentifier :: RoleInstance -> MonadPerspectivesQuery String
roleIdentifier (RoleInstance id) = pure id

contextIdentifier :: ContextInstance -> MonadPerspectivesQuery String
contextIdentifier (ContextInstance id) = pure id

systemIdentifier :: RoleInstance -> MonadPerspectivesQuery String
systemIdentifier _ = lift $ lift getSystemIdentifier

replace :: Array String -> Array String -> String -> MonadPerspectivesQuery String
replace patterns replacements value = case head patterns, head replacements of
  Just pattern, Just replacement -> pure $ String.replace (Pattern pattern) (Replacement replacement) value
  _, _ -> pure value

replaceR :: Array String -> Array String -> String -> MonadPerspectivesQuery String
replaceR patterns replacements value = case head patterns, head replacements of
  Just regexString, Just replacement -> do
    theRegex <- pure $ REGEX.regex regexString noFlags
    case theRegex of
      Left e -> pure value
      Right r -> pure $ REGEX.replace r replacement value
  _, _ -> pure value

selectR :: Array String -> String -> MonadPerspectivesQuery String
selectR patterns value = ArrayT case head patterns of
  Just regexString -> case REGEX.regex regexString noFlags of
    Left e -> pure []
    Right r -> case getFirstMatch r value of
      Nothing -> pure []
      Just s -> pure [s]
  Nothing -> pure []

-- | See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat for the values of Locale and 
-- | the shape that the options can have.

-- | E.g. "En-US" or "nl-NL"
type Locale = String
-- | E.g. {weekday: "long", year: "numeric", month: "long", day: "numeric"}. See: https://tc39.es/ecma402/#sec-createdatetimeformat.
type FormatOptions = String

formatDateTime_ :: Array String -> Array String -> Array String -> RoleInstance -> MonadPerspectivesQuery String
formatDateTime_ dts locales optionss _ = ArrayT $ case head dts, head locales, head optionss of
  Just dt, Just locale, Just options -> do
    epoch <- parseInt dt
    runArrayT $ formatDateTime epoch locale options
  _, _, _ -> pure []

formatDateTime :: Int -> Locale -> FormatOptions -> MonadPerspectivesQuery String
formatDateTime dt locale options = liftEffect $ runEffectFn3 formatDateTimeImpl dt locale options

foreign import formatDateTimeImpl :: EffectFn3 Int Locale FormatOptions String

data EvaluationResult = Success (Array String) | PE ParseError | PSPE MultiplePerspectivesErrors

instance Show EvaluationResult where
  show (Success values) = "result#" <> show values
  show (PE parseError) = "error#" <> show parseError
  show (PSPE errs) = "error#" <> show errs

-- | Evaluate the expression and apply it to the role instance.
-- | The result is a (series of) error message(s) that will start with "error#" or a valid result in string form preceded by "result#".
evalExpression :: String -> RoleInstance -> MonadPerspectivesQuery String
evalExpression expr roleId@(RoleInstance id) = do 
  rt <- roleType roleId
  ct <- (context >=> contextType) roleId
  (r :: Either ParseError Step) <- liftAff $ runIndentParser expr step 
  case r of 
    Left e -> pure $ show (PE e)
    Right (parseTree :: Step) -> do 
      s <- liftAff $ evalPhaseTwo' (expandPrefix parseTree)
      case s of 
        Left e -> pure $ show (PSPE e)
        Right parseTree' -> do 
          (t :: Either MultiplePerspectivesErrors QueryFunctionDescription) <- lift $ lift $ evalPhaseTwo' 
            (compileExpression (RDOM $ ST $ RoleInContext {context: ct, role: rt}) parseTree')
          case t of 
            Left errs -> pure $ show (PSPE errs)
            Right qfd -> do 
              calculator <- lift $ lift $ compileFunction qfd
              result <- calculator id
              pure $ "result#" <> result

evalExpression_ :: Array String -> RoleInstance -> MonadPerspectivesQuery String
evalExpression_ exprArray roleId = ArrayT case head exprArray of 
  Just expr -> runArrayT $ evalExpression expr roleId
  _ -> pure []

-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "model://perspectives.domains#Utilities$GenSym" {func: unsafeCoerce genSym, nArgs: 0, isFunctional: True}
  , Tuple "model://perspectives.domains#Utilities$RoleIdentifier" {func: unsafeCoerce roleIdentifier, nArgs: 0, isFunctional: True }
  , Tuple "model://perspectives.domains#Utilities$ContextIdentifier" {func: unsafeCoerce contextIdentifier, nArgs: 0, isFunctional: True}
  , Tuple "model://perspectives.domains#Utilities$SystemIdentifier" {func: unsafeCoerce systemIdentifier, nArgs: 0, isFunctional: True}
  , Tuple "model://perspectives.domains#Utilities$Replace" {func: unsafeCoerce replace, nArgs: 2, isFunctional: True}
  , Tuple "model://perspectives.domains#Utilities$ReplaceR" {func: unsafeCoerce replaceR, nArgs: 2, isFunctional: True}
  , Tuple "model://perspectives.domains#Utilities$SelectR" {func: unsafeCoerce selectR, nArgs: 1, isFunctional: True}
  , Tuple "model://perspectives.domains#Utilities$Random" {func: unsafeCoerce random, nArgs: 2, isFunctional: True}
  , Tuple "model://perspectives.domains#Utilities$FormatDateTime" {func: unsafeCoerce formatDateTime_, nArgs: 3, isFunctional: True}
  , Tuple "model://perspectives.domains#Utilities$EvalExpression" {func: unsafeCoerce evalExpression_, nArgs: 1, isFunctional: Unknown}
  ]
