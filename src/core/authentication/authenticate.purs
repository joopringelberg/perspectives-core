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

module Perspectives.Authenticate
  ( getMyPublicKey
  , getPrivateKey
  , signDelta
  , verifyDelta
  )
  where

import Prelude

import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, toAffE)
import Crypto.Subtle.Constants.AES (aesCBC, l256)
import Crypto.Subtle.Constants.EC as ECConstants
import Crypto.Subtle.Hash (sha1, sha256)
import Crypto.Subtle.Key.Derive (deriveBits, pbkdf2)
import Crypto.Subtle.Key.Generate (aes, generateKey)
import Crypto.Subtle.Key.Import (ImportAlgorithm, ec, importKey)
import Crypto.Subtle.Key.Types (CryptoKey)
import Crypto.Subtle.Key.Types as CryptoTypes
import Crypto.Subtle.Sign (ecdsa, sign, verify)
import Data.Array (foldMap)
import Data.ArrayBuffer.ArrayBuffer (byteLength)
import Data.ArrayBuffer.Builder (PutM, putArrayBuffer, execPut, putUint32be)
import Data.ArrayBuffer.Typed (buffer, toArray, whole)
import Data.ArrayBuffer.Types (ArrayBuffer, Int8Array)
import Data.Int (hexadecimal, toStringAs)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.String (length)
import Data.Traversable (for)
import Data.UInt (fromInt)
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Perspectives.CoreTypes (MonadPerspectives, (##>))
import Perspectives.Instances.ObjectGetters (getProperty)
import Perspectives.ModelDependencies (perspectivesUsersPublicKey)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistence.Types (Password)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..))
import Perspectives.ResourceIdentifiers (stripNonPublicIdentifiers)
import Perspectives.ResourceIdentifiers.Parser (ResourceIdentifier)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Simple.JSON (unsafeStringify)
import Unsafe.Coerce (unsafeCoerce)
import Web.Encoding.TextDecoder (decode, new) as Decoder
import Web.Encoding.TextEncoder (encode, new) as Encoder
import Web.Encoding.UtfLabel (utf8)

-----------------------------------------------------------
-- SIGNING AND VERIFYING
-----------------------------------------------------------
-- | Top level entry to message authentication. Sign a message. Under the hood this requires fetching
-- | the users private key and signing the message.
signDelta :: ResourceIdentifier -> String -> MonadPerspectives SignedDelta
signDelta author encryptedDelta = do
  deltaBuff :: ArrayBuffer <- liftEffect $ string2buff encryptedDelta
  mcryptoKey <- gets (_.privateKey <<< _.runtimeOptions)
  case mcryptoKey of
    Nothing ->  pure $ SignedDelta 
      { author: stripNonPublicIdentifiers author
      , encryptedDelta
      , signature: Nothing
      }
    Just cryptoKey -> do 
      signatureBuff <- lift $ sign (ecdsa sha256) (unsafeCoerce cryptoKey) deltaBuff
      signature <- liftEffect $ buff2string signatureBuff
      pure $ SignedDelta 
        { author: stripNonPublicIdentifiers author
        , encryptedDelta
        , signature: Just signature
        }

verifyDelta :: SignedDelta -> MonadPerspectives (Maybe String)
verifyDelta (SignedDelta{author, encryptedDelta, signature}) = do
  mcryptoKey <- getPublicKey author
  case signature, mcryptoKey of 
    Nothing, Nothing -> pure $ Just encryptedDelta
    Nothing, Just _ ->  pure $ Just encryptedDelta
    Just s, Nothing -> throwError (error $ "No public key found for " <> author)
    Just signature', Just cryptoKey -> do 
      signatureBuff <- liftAff $ liftEffect $ string2buff signature'
      deltaBuff <- liftAff $ liftEffect $ string2buff encryptedDelta
      trusted <- liftAff $ verify (ecdsa sha256) cryptoKey signatureBuff deltaBuff
      if trusted
        then pure $ Just encryptedDelta
        else pure Nothing

-- | Get the public key of a peer.
getPublicKey :: String -> MonadPerspectives (Maybe CryptoTypes.CryptoKey)
getPublicKey author = do
  mrawKey <- (RoleInstance author) ##> getProperty (EnumeratedPropertyType perspectivesUsersPublicKey)
  case mrawKey of 
    Nothing -> pure Nothing 
    Just (Value rawKey) -> do
      keyBuff <- liftAff $ liftEffect $ string2buff rawKey
      key <- lift $ importKey CryptoTypes.jwk keyBuff (ec ECConstants.ecdsa ECConstants.p256) true [CryptoTypes.verify]
      pure $ Just key

-- | Get my private key (to sign a delta).
-- | This is used on system startup. It takes the private key from IndexedDB 
-- | in order to put it in Perspectives State.
getPrivateKey :: MonadPerspectives (Maybe CryptoTypes.CryptoKey)
getPrivateKey = do
  sysId <- getSystemIdentifier
  privateKey <- toMaybe <$> (lift $ getCryptoKey $ sysId <> privateKeyString)
  pure privateKey

-- | Get my public key. This will only be used on setting up an installation.
-- | It is taken from IndexedDB.
getMyPublicKey :: MonadPerspectives (Maybe String)
getMyPublicKey = do
  sysId <- getSystemIdentifier
  publicKey <- toMaybe <$> (lift $ getCryptoKey $ sysId <> publicKeyString)
  for publicKey
    \key -> lift $ do 
      jwk <- CryptoTypes.exportKey CryptoTypes.jwk key
      -- NOTE. The Purescript library Crypto.Subtle contains an error here. `exportKey` always returns an ArrayBuffer,
      -- but https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/exportKey#return_value clearly states that in case
      -- export format JWK a JSON object is returned. This is what we observe. We therefore use unsafeStringify rather then writeJSON
      pure $ unsafeStringify jwk

publicKeyString :: String
publicKeyString = "_publicKey"

privateKeyString :: String
privateKeyString = "_privateKey"

foreign import getCryptoKeyImpl :: EffectFn1 String (Promise (Nullable CryptoKey))
getCryptoKey :: String -> Aff (Nullable CryptoKey)
getCryptoKey = getCryptoKeyImpl_ >>> toAffE

getCryptoKeyImpl_ :: String -> Effect (Promise (Nullable CryptoKey))
getCryptoKeyImpl_ = runEffectFn1 getCryptoKeyImpl

buff2string :: ArrayBuffer -> Effect String
buff2string buff = do
  buffDecoder <- Decoder.new utf8
  (int8array :: Int8Array) <- whole buff
  Decoder.decode int8array buffDecoder

string2buff :: String -> Effect ArrayBuffer
string2buff s = do
  textEncoder <- liftEffect Encoder.new
  pure $ buffer $ Encoder.encode s textEncoder

-----------------------------------------------------------
-- CREATEPASSWORD
-----------------------------------------------------------

pbkdf2_import :: ImportAlgorithm
pbkdf2_import = unsafeCoerce "PBKDF2"

-- Create a password 
createPassword :: Password -> Aff String
createPassword password = do
  -- Generate a random salt
  (key :: CryptoTypes.CryptoKey) <- generateKey (aes aesCBC l256) true [CryptoTypes.encrypt, CryptoTypes.decrypt]
  (salt :: ArrayBuffer) <- CryptoTypes.exportKey CryptoTypes.raw key
  -- Convert the password to an ArrayBuffer and import it into a CryptoTypes.CryptoKey.
  (passwordData :: ArrayBuffer) <- liftEffect $ execPut $ putStringUtf8 password
  (baseKey :: CryptoTypes.CryptoKey) <- importKey CryptoTypes.raw passwordData pbkdf2_import false [CryptoTypes.deriveBits]
  -- Now derive a key from the password and the salt.
  (derivedKey :: ArrayBuffer) <- deriveBits (pbkdf2 sha1 salt 10000) baseKey 8
  -- Finally, convert to a hexadecimal string representation.
  liftEffect $ arrayBufferToHexString derivedKey

  where 

    putStringUtf8 :: forall m. MonadEffect m => String -> PutM m Unit
    putStringUtf8 s = do
      textEncoder <- liftEffect Encoder.new
      let (stringbuf :: ArrayBuffer) = buffer $ Encoder.encode s textEncoder
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
