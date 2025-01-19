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

import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web (Response, printError, request)
import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Trans.Class (lift)
import Data.Array (head, singleton)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), Replacement(..))
import Data.String (replace) as String
import Data.String.Regex (regex, replace) as REGEX
import Data.String.Regex.Flags (noFlags)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Effect.Random (randomRange)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import IDBKeyVal (idbSet) as IDBKeyVal
import Parsing (ParseError)
import Perspectives.Authenticate (getMyPublicKey)
import Perspectives.CoreTypes (MonadPerspectivesQuery, MonadPerspectivesTransaction)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.Error.Boundaries (handleExternalFunctionError)
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Identifiers (getFirstMatch)
import Perspectives.Instances.ObjectGetters (bottom, context, contextType, roleType)
import Perspectives.Instances.Values (parseNumber)
import Perspectives.Parsing.Arc.Expression (step)
import Perspectives.Parsing.Arc.Expression.AST (Step)
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseTwoDefs (evalPhaseTwo')
import Perspectives.Parsing.Messages (MultiplePerspectivesErrors)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.PerspectivesState (setCurrentLanguage) as PState
import Perspectives.Query.ExpandPrefix (ensureModel, expandPrefix)
import Perspectives.Query.ExpressionCompiler (compileExpression)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription, RoleInContext(..))
import Perspectives.Query.UnsafeCompiler (compileFunction)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.ResourceIdentifiers (createCuid)
import Prelude (class Show, bind, discard, pure, show, void, ($), (<<<), (<>), (>=>), (>>=), (*>))
import Simple.JSON (readJSON, write, writeJSON)
import Unsafe.Coerce (unsafeCoerce)

-- TODO: verander naar echte gegenereerde identifiers.
genSym :: RoleInstance -> MonadPerspectivesQuery String
genSym _ = try 
  (lift $ lift createCuid)
  >>= handleExternalFunctionError "model://perspectives.domains#Utilities$GenSym"

-- | Returns a random int in the closed interval [l;u].
random :: Array String -> Array String -> RoleInstance -> MonadPerspectivesQuery Value
random lower_ upper_ _ = try 
  (ArrayT case head lower_, head upper_ of
    Just l, Just u -> do 
      lowerBound <- try $ liftAff $ parseNumber l 
      upperBound <- try $ liftAff $ parseNumber u
      case lowerBound, upperBound of 
        Right lower, Right upper -> (liftEffect (randomRange lower upper)) >>= pure <<< singleton <<< Value <<< show <<< floor
        _, _ -> pure []
    _, _ -> pure [])
  >>= handleExternalFunctionError "model://perspectives.domains#Utilities$Random"

roleIdentifier :: RoleInstance -> MonadPerspectivesQuery String
roleIdentifier (RoleInstance id) = pure id

contextIdentifier :: ContextInstance -> MonadPerspectivesQuery String
contextIdentifier (ContextInstance id) = pure id

systemIdentifier :: RoleInstance -> MonadPerspectivesQuery String
systemIdentifier _ = try
  (lift $ lift getSystemIdentifier)
  >>= handleExternalFunctionError "model://perspectives.domains#Utilities$SystemIdentifier"

bottomIdentifier :: RoleInstance -> MonadPerspectivesQuery String
bottomIdentifier = bottom >=> pure <<< unwrap

replace :: Array String -> Array String -> String -> MonadPerspectivesQuery String
replace patterns replacements value = try
  (case head patterns, head replacements of
    Just pattern, Just replacement -> pure $ String.replace (Pattern pattern) (Replacement replacement) value
    _, _ -> pure value)
  >>= handleExternalFunctionError "model://perspectives.domains#Utilities$Replace"

replaceR :: Array String -> Array String -> String -> MonadPerspectivesQuery String
replaceR patterns replacements value = try
  (case head patterns, head replacements of
    Just regexString, Just replacement -> do
      theRegex <- pure $ REGEX.regex regexString noFlags
      case theRegex of
        Left e -> pure value
        Right r -> pure $ REGEX.replace r replacement value
    _, _ -> pure value)
  >>= handleExternalFunctionError "model://perspectives.domains#Utilities$ReplaceR"

selectR :: Array String -> String -> MonadPerspectivesQuery String
selectR patterns value = try 
  (ArrayT case head patterns of
    Just regexString -> case REGEX.regex regexString noFlags of
      Left e -> pure []
      Right r -> case getFirstMatch r value of
        Nothing -> pure []
        Just s -> pure [s]
    Nothing -> pure [])
  >>= handleExternalFunctionError "model://perspectives.domains#Utilities$SelectR"

-- | See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat for the values of Locale and 
-- | the shape that the options can have.

-- | E.g. "En-US" or "nl-NL"
type Locale = String
-- | E.g. {weekday: "long", year: "numeric", month: "long", day: "numeric"}. See: https://tc39.es/ecma402/#sec-createdatetimeformat.
type FormatOptions = String

-- | The timeZone is always UTC. This is in line with the time values that we obtain from the SmartFieldControl.
formatDateTime_ :: Array String -> Array String -> Array String -> RoleInstance -> MonadPerspectivesQuery String
formatDateTime_ dts locales optionss _ = try 
  (ArrayT $ case head dts, head locales, head optionss of
    Just dt, Just locale, Just options -> do
      epoch <- parseNumber dt
      runArrayT $ formatDateTime epoch locale options
    _, _, _ -> pure [])
  >>= handleExternalFunctionError "model://perspectives.domains#Utilities$FormatDateTime"

formatDateTime :: Number -> Locale -> FormatOptions -> MonadPerspectivesQuery String
formatDateTime dt locale options = liftEffect $ runEffectFn3 formatDateTimeImpl dt locale options

foreign import formatDateTimeImpl :: EffectFn3 Number Locale FormatOptions String

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
          lift $ lift $ void $ ensureModel parseTree'
          (t :: Either MultiplePerspectivesErrors QueryFunctionDescription) <- lift $ lift $ evalPhaseTwo' 
            (compileExpression (RDOM $ UET $ RoleInContext {context: ct, role: rt}) parseTree')
          case t of 
            Left errs -> pure $ show (PSPE errs)
            Right qfd -> do 
              calculator <- lift $ lift $ compileFunction qfd
              result <- calculator id
              pure $ "result#" <> result

evalExpression_ :: Array String -> RoleInstance -> MonadPerspectivesQuery String
evalExpression_ exprArray roleId = try
  (ArrayT case head exprArray of 
    Just expr -> runArrayT $ evalExpression expr roleId
    _ -> pure []
  )
  >>= handleExternalFunctionError "model://perspectives.domains#Utilities$EvalExpression"

-- | Return various values from PerspectivesState or even other state.
systemParameter_ :: Array String -> ContextInstance -> MonadPerspectivesQuery String
systemParameter_ parArray _ = try 
  (ArrayT $ lift case head parArray of 
    Just par -> case par of
      "IsFirstInstallation" -> gets (singleton <<< show <<< _.isFirstInstallation <<< _.runtimeOptions)
      "PublicKey" -> getMyPublicKey >>= case _ of 
        Just publicKey -> pure [publicKey]
        Nothing -> pure []
      "MyContextsVersion" -> gets (singleton <<< _.myContextsVersion <<< _.runtimeOptions)
      "PDRVersion" -> pure [pdrVersion]
      _ -> pure []
    _ -> pure [])
  >>= handleExternalFunctionError "model://perspectives.domains#Utilities$SystemParameter"

-- | The package version of the Perspectives Distributed Runtime.
foreign import pdrVersion :: String
foreign import mycontextsUrl :: String

-- | Given a serialised transaction and the string representation of a ConfirmationCode, creates a serialised Invitation
createInvitation_ :: Array String -> Array String -> Array String -> RoleInstance -> MonadPerspectivesQuery String
createInvitation_ messageA transactionA confirmationA _ = try 
  (ArrayT $ lift case head messageA, head transactionA, head confirmationA of
    Just message, Just transaction, Just confirmation -> pure [writeJSON { message, transaction, confirmation }]
    _, _, _ -> pure [])
  >>= handleExternalFunctionError "model://perspectives.domains#Utilities$CreateInvitation"

getSharedFileServerKey :: Array String -> RoleInstance -> MonadPerspectivesQuery String
getSharedFileServerKey keyArray _ = try
  (ArrayT $ case head keyArray of
    Just sharedFileServerKey -> do
        rq <- pure 
          { method: Left POST
          , url: mycontextsUrl <> "ppsfs/getsharedfileserverkey"
          , headers: []
          , content: Just $ RequestBody.string $ writeJSON ({sharedfileserverkey: sharedFileServerKey } :: { sharedfileserverkey :: String})
          , responseFormat: ResponseFormat.string
          , timeout: Nothing
          , password: Nothing
          , username: Nothing
          , withCredentials: false
          }
        res <- liftAff $ request rq
        case res of 
          Left e -> do 
            log $ printError e
            pure []
          Right (response :: Response String) -> case (readJSON response.body) of
            Left e -> throwError $ error ("model://perspectives.domains#Utilities$GetSharedFileServerKey: " <> show e)
            Right (r :: {error :: Maybe Int, message :: Maybe String, newKey :: Maybe String}) -> case r.error, r.message, r.newKey of
              Just error_, Just message_, _ -> log message_ *> pure []
              Nothing, Nothing, Just newKey_ -> pure [newKey_]
              _, _, _ -> pure []
    Nothing -> pure []
    )
  >>= handleExternalFunctionError "model://perspectives.domains#Utilities$GetSharedFileServerKey"

idbSet :: Array String -> Array String -> RoleInstance -> MonadPerspectivesTransaction Unit
idbSet keys values _ = case head keys, head values of
  Just key, Just value -> liftAff $ IDBKeyVal.idbSet key (write value)
  _, _ -> pure unit

setCurrentLanguage :: Array String -> Array String -> RoleInstance -> MonadPerspectivesTransaction Unit
setCurrentLanguage keys values _ = case head keys, head values of
  Just key, Just value -> do
    liftAff $ IDBKeyVal.idbSet key (write value)
    lift $ PState.setCurrentLanguage value
  _, _ -> pure unit


-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "model://perspectives.domains#Utilities$GenSym" {func: unsafeCoerce genSym, nArgs: 0, isFunctional: True, isEffect: false}
  , Tuple "model://perspectives.domains#Utilities$RoleIdentifier" {func: unsafeCoerce roleIdentifier, nArgs: 0, isFunctional: True, isEffect: false}
  , Tuple "model://perspectives.domains#Utilities$ContextIdentifier" {func: unsafeCoerce contextIdentifier, nArgs: 0, isFunctional: True, isEffect: false}
  , Tuple "model://perspectives.domains#Utilities$SystemIdentifier" {func: unsafeCoerce systemIdentifier, nArgs: 0, isFunctional: True, isEffect: false}
  , Tuple "model://perspectives.domains#Utilities$BottomIdentifier" {func: unsafeCoerce bottomIdentifier, nArgs: 0, isFunctional: True, isEffect: false}
  , Tuple "model://perspectives.domains#Utilities$Replace" {func: unsafeCoerce replace, nArgs: 2, isFunctional: True, isEffect: false}
  , Tuple "model://perspectives.domains#Utilities$ReplaceR" {func: unsafeCoerce replaceR, nArgs: 2, isFunctional: True, isEffect: false}
  , Tuple "model://perspectives.domains#Utilities$SelectR" {func: unsafeCoerce selectR, nArgs: 1, isFunctional: True, isEffect: false}
  , Tuple "model://perspectives.domains#Utilities$Random" {func: unsafeCoerce random, nArgs: 2, isFunctional: True, isEffect: false}
  , Tuple "model://perspectives.domains#Utilities$FormatDateTime" {func: unsafeCoerce formatDateTime_, nArgs: 3, isFunctional: True, isEffect: false}
  , Tuple "model://perspectives.domains#Utilities$EvalExpression" {func: unsafeCoerce evalExpression_, nArgs: 1, isFunctional: Unknown, isEffect: false}
  , Tuple "model://perspectives.domains#Utilities$SystemParameter" {func: unsafeCoerce systemParameter_, nArgs: 1, isFunctional: True, isEffect: false}
  , Tuple "model://perspectives.domains#Utilities$CreateInvitation" {func: unsafeCoerce createInvitation_, nArgs: 3, isFunctional: True, isEffect: false}
  , Tuple "model://perspectives.domains#Utilities$GetSharedFileServerKey" {func: unsafeCoerce getSharedFileServerKey, nArgs: 1, isFunctional: True, isEffect: false}
  , Tuple "model://perspectives.domains#Utilities$IdbSet" {func: unsafeCoerce idbSet, nArgs: 2, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#Utilities$SetCurrentLanguage" {func: unsafeCoerce setCurrentLanguage, nArgs: 2, isFunctional: True, isEffect: true}
  ]
