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

module Perspectives.Instances.Values
  ( MIME
  , PerspectivesFile
  , bool2Value
  , date2Value
  , decodeDate
  , defaultDateTime
  , number2Value
  , parseBool
  , parseDate
  , parseNumber
  , parseNumber__
  , parsePerspectivesFile
  , value2Bool
  , value2Bool'
  , value2Date
  , value2Date'
  , value2Number
  , value2Number'
  , writePerspectivesFile
  )
  where


-- | Parse a date. See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse#Date_Time_String_Format for the supported string format of the date.

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (runExcept)
import Data.DateTime (DateTime(..), Time(..), canonicalDate)
import Data.DateTime.Instant (fromDateTime, unInstant)
import Data.Either (Either(..), either, fromRight)
import Data.Enum (toEnum)
import Data.JSDate (JSDate, parse, toDateTime)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error, try)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (ForeignError, MultipleErrors, readNumber, unsafeToForeign)
import Partial.Unsafe (unsafePartial)
import Perspectives.Representation.InstanceIdentifiers (Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType)
import Perspectives.Sync.DateTime (SerializableDateTime(..))
import Prelude (bind, pure, show, ($), (<$>), (<*>), (<<<), (<>))
import Simple.JSON (read, readJSON, writeJSON)

-- TODO. We gebruiken hier Error, het javascript error type. Liever zou ik een
-- PerspectivesRuntimeError type gebruiken. Maar dan moeten we MonadPerspectives aanpassen.

-- | Parse a date. See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse#Date_Time_String_Format for the supported string format of the date.
parseDate :: forall m. MonadError Error m => String -> m DateTime
parseDate s = do
  (d :: JSDate) <- pure $ unsafePerformEffect $ parse s
  case toDateTime d of
    Nothing -> throwError $ error "Not a date"
    (Just (dt :: DateTime)) -> pure dt

-- | Decode a date from the Epoch format in which it is stored in Couchdb.
decodeDate :: forall m. MonadError Error m => String -> m DateTime
decodeDate s = 
  case (read $ unsafeToForeign s) of
    Left e -> throwError $ error "Not a date"
    Right (SerializableDateTime dt) -> pure dt

foreign import parseNumber__ :: EffectFn1 String Number

parseNumber_ :: String -> Effect Number
parseNumber_ = runEffectFn1 parseNumber__

parseNumber :: forall m. MonadError Error m => MonadEffect m => String -> m Number
parseNumber s = do
  r <- liftEffect $ try (parseNumber_ s)
  case r of
    Left e -> throwError (error $ "Cannot parse a number from '" <> s <> "' (" <> show e <> ")")
    Right i -> pure i

parseBool :: forall m. MonadError Error m => MonadEffect m => String -> m Boolean
parseBool "true" = pure true
parseBool "false" = pure false
parseBool s = throwError (error $ "Cannot parse a bool from '" <> s <> "'.")

-------------------------------------------------------------------------------
-- VALUE TO VARIOUS TYPES
-------------------------------------------------------------------------------
value2Number' :: Value -> Either Error Number
value2Number' (Value is) = either
  (Left <<< error <<< show)
  Right 
  ((runExcept $ readNumber $ unsafeToForeign is) :: Either (NonEmptyList ForeignError) Number)

-- | An UNSAFE operation!
value2Number :: Value -> Number
value2Number (Value is) = unsafePerformEffect $ parseNumber_ is

number2Value :: Number -> Value
number2Value = Value <<< show

value2Bool' :: Value -> Either Error Boolean
value2Bool' (Value "true") = Right true
value2Bool' (Value "false") = Right false
value2Bool' x = Left $ error $ "value2Bool: not a boolean: " <> show x

-- | An safe operation, but with subtly different semantics than value2Bool'.
-- | Only (Value "true") is mapped to `true`; all other values are mapped to `false`.
-- | However, value2Bool' will signal an error condition if the string is
-- | is not the representation of a Boolean value.
value2Bool :: Value -> Boolean
value2Bool (Value "true") = true
value2Bool _ = false

bool2Value :: Boolean -> Value
bool2Value = Value <<< show

value2Date' :: Value -> Either Error SerializableDateTime
value2Date' (Value dt) = case read $ unsafeToForeign dt of
  Left el -> Left $ error ("value2Date: not a date: " <> show el)
  Right d -> Right d

-- | An UNSAFE operation!
value2Date :: Value -> SerializableDateTime
value2Date (Value dt) = fromRight (SerializableDateTime defaultDateTime) $ read $ unsafeToForeign dt


date2Value :: SerializableDateTime -> Value
date2Value (SerializableDateTime sdt) = Value $ show (unwrap $ unInstant (fromDateTime sdt))

defaultDateTime :: DateTime
defaultDateTime = unsafePartial fromJust $ DateTime <$> (canonicalDate <$> (toEnum 2022) <*> (toEnum 1) <*> (toEnum 1)) <*>
  (Time <$> (toEnum 0) <*> (toEnum 0) <*> (toEnum 0) <*> (toEnum 0))

-----------------------------------------------------------
-- PERSPECTIVESFILE
-----------------------------------------------------------
type MIME = String

-- Use database and roleFileName to retrieve the role instance; 
-- use the local name of the PFile property to retrieve the attachment.
type PerspectivesFile = 
  { fileName :: String                      -- The name associated with the file on creating or uploading it. Use only client side.
  , propertyType :: EnumeratedPropertyType  -- The identifier of the attachment of the role instance.
  , mimeType :: MIME
  , database :: Maybe String                -- The database where the role instance is stored. 
                                            -- (is Nothing for IndexedDB)
  , roleFileName :: String                  -- The name of the role instance document. 
  }

parsePerspectivesFile :: String -> Either MultipleErrors PerspectivesFile
parsePerspectivesFile = readJSON

writePerspectivesFile :: PerspectivesFile -> String
writePerspectivesFile = writeJSON