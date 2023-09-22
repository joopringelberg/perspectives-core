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

module Perspectives.Authenticate where

import Prelude

import Crypto.Subtle.Constants.AES (aesCBC, l256)
import Crypto.Subtle.Hash (sha1)
import Crypto.Subtle.Key.Derive (deriveBits, pbkdf2)
import Crypto.Subtle.Key.Generate (aes, generateKey)
import Crypto.Subtle.Key.Import (ImportAlgorithm, importKey)
import Crypto.Subtle.Key.Types as CryptoTypes
import Data.Array (foldMap)
import Data.ArrayBuffer.ArrayBuffer (byteLength)
import Data.ArrayBuffer.Builder (PutM, putArrayBuffer, execPut, putUint32be)
import Data.ArrayBuffer.Typed (buffer, toArray, whole)
import Data.ArrayBuffer.Types (ArrayBuffer, Int8Array)
import Data.Int (hexadecimal, toStringAs)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.UInt (fromInt)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.Persistence.Types (Password)
import Unsafe.Coerce (unsafeCoerce)
import Web.Encoding.TextEncoder (encode, new)


-- | Top level entry to message authentication. Sign a message. Under the hood this requires fetching
-- | the users private key and encrypting the message.
sign :: String -> String
sign message = message

-- | Top level entry to message authentication. Given a role instance of model:System$PerspectivesSystem$User,
-- | (the author) ensure that the message was, indeed, sent by the author. Under the hood this involves fetching
-- | the authors' public key and decrypting the message.
authenticate :: RoleInstance -> String -> Maybe String
authenticate author message = Just message

-- | Use the current authors key to decrypt the message. This must succeed; otherwise we have a logical error.
selfAuthenticate :: String -> String
selfAuthenticate message = message

pbkdf2_import :: ImportAlgorithm
pbkdf2_import = unsafeCoerce "PBKDF2"

-- Create a password 
createPassword :: Password -> Aff String
createPassword password = do
  -- Generate a random salt
  (key :: CryptoTypes.CryptoKey) <- generateKey (aes aesCBC l256) true [CryptoTypes.encrypt, CryptoTypes.decrypt]
  (salt :: ArrayBuffer) <- CryptoTypes.exportKey CryptoTypes.raw key
  -- Convert the password to an ArrayBuffer and import it into a CryptoKey.
  (passwordData :: ArrayBuffer) <- liftEffect $ execPut $ putStringUtf8 password
  (baseKey :: CryptoTypes.CryptoKey) <- importKey CryptoTypes.raw passwordData pbkdf2_import false [CryptoTypes.deriveBits]
  -- Now derive a key from the password and the salt.
  (derivedKey :: ArrayBuffer) <- deriveBits (pbkdf2 sha1 salt 10000) baseKey 8
  -- Finally, convert to a hexadecimal string representation.
  liftEffect $ arrayBufferToHexString derivedKey

  where 

    putStringUtf8 :: forall m. MonadEffect m => String -> PutM m Unit
    putStringUtf8 s = do
      textEncoder <- liftEffect new
      let (stringbuf :: ArrayBuffer) = buffer $ encode s textEncoder
      -- Put a 32-bit big-endian length for the utf8 string, in bytes.
      putUint32be $ fromInt $ byteLength stringbuf
      putArrayBuffer stringbuf

  -- Function to convert an ArrayBuffer to a hexadecimal string
    arrayBufferToHexString :: ArrayBuffer -> Effect String
    arrayBufferToHexString buffer = do
      (int8array :: Int8Array) <- whole buffer
      arr <- toArray int8array
      pure $ foldMap toHex arr 
      where
        toHex :: Int -> String
        toHex int = case toStringAs hexadecimal int of
          s | length s == 1 -> "0" <> s
          s -> s
