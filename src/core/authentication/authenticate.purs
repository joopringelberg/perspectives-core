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
  ( deserializeJWK
  , getMyPublicKey
  , tryGetPublicKey
  , getPrivateKey
  , signDelta
  , verifyDelta
  , verifyDelta'
  )
  where

import Prelude

import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Except (runExcept)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, toAffE)
import Crypto.Subtle.Constants.AES (aesCBC, l256)
import Crypto.Subtle.Constants.EC as ECConstants 
import Crypto.Subtle.Hash (sha1, sha384)
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
import Data.ArrayBuffer.Types (ArrayBuffer, Int8Array, Uint8Array)
import Data.Either (Either(..))
import Data.Int (hexadecimal, toStringAs)
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Data.String (length)
import Data.Traversable (for)
import Data.UInt (fromInt)
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import IDBKeyVal (idbGet)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, (##>))
import Perspectives.Instances.ObjectGetters (deltaAuthor2ResourceIdentifier, getProperty)
import Perspectives.ModelDependencies (perspectivesUsersPublicKey)
import Perspectives.Persistence.Types (Password)
import Perspectives.Persistent (entityExists)
import Perspectives.PerspectivesState (getPerspectivesUser)
import Perspectives.Representation.InstanceIdentifiers (PerspectivesUser(..), Value(..), perspectivesUser2RoleInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..))
import Perspectives.ResourceIdentifiers (stripNonPublicIdentifiers, takeGuid)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Simple.JSON (parseJSON, unsafeStringify)
import Unsafe.Coerce (unsafeCoerce)
import Web.Encoding.TextEncoder (encode, new) as Encoder

-----------------------------------------------------------
-- SIGNING AND VERIFYING
-----------------------------------------------------------
-- | Top level entry to message authentication. Sign a message. Under the hood this requires fetching
-- | the users private key and signing the message.
signDelta :: String -> MonadPerspectivesTransaction SignedDelta
signDelta encryptedDelta = do
  author <- lift getPerspectivesUser
  deltaBuff :: ArrayBuffer <- liftEffect $ string2buff encryptedDelta
  mcryptoKey <- lift $ gets (_.privateKey <<< _.runtimeOptions)
  case mcryptoKey of
    Nothing ->  pure $ SignedDelta 
      { author: over PerspectivesUser stripNonPublicIdentifiers author
      , encryptedDelta
      , signature: Nothing
      }
    Just cryptoKey -> do 
      signatureBuff <- lift $ lift $ sign (ecdsa sha384) (unsafeCoerce cryptoKey) deltaBuff
      (int8array :: Uint8Array) <- liftEffect $ whole signatureBuff
      (signature :: String) <- liftAff $ bytesToBase64DataUrl int8array
      sd <- pure $ SignedDelta 
        { author: over PerspectivesUser stripNonPublicIdentifiers author
        , encryptedDelta
        , signature: Just signature
        }
      pure sd

-- | Returns Nothing if the delta cannot be verified; a string representation of the Delta in the SignedDelta otherwise.
verifyDelta :: SignedDelta -> MonadPerspectives (Maybe String)
verifyDelta d@(SignedDelta{author, encryptedDelta, signature}) = getPublicKey author >>= verifyDelta' d

verifyDelta' :: SignedDelta -> Maybe CryptoTypes.CryptoKey -> MonadPerspectives (Maybe String)
verifyDelta' (SignedDelta{author, encryptedDelta, signature}) mcryptoKey = do
  case signature, mcryptoKey of 
    Nothing, Nothing -> pure $ Just encryptedDelta
    Nothing, Just _ ->  pure $ Just encryptedDelta
    Just s, Nothing -> throwError (error $ "No public key found for " <> show author)
    Just signature', Just cryptoKey -> do 
      signatureBuff <- buffer <$> (liftAff $ dataUrlToBytes signature')
      deltaBuff <- liftAff $ liftEffect $ string2buff encryptedDelta
      trusted <- liftAff $ verify (ecdsa sha384) cryptoKey signatureBuff deltaBuff
      if trusted
        then pure $ Just encryptedDelta
        else pure Nothing

-- | Get the public key of a peer.
getPublicKey :: PerspectivesUser -> MonadPerspectives (Maybe CryptoTypes.CryptoKey)
getPublicKey author = do
  mrawKey <- (perspectivesUser2RoleInstance $ deltaAuthor2ResourceIdentifier author) ##> getProperty (EnumeratedPropertyType perspectivesUsersPublicKey)
  case mrawKey of 
    Nothing -> pure Nothing 
    Just (Value rawKey) -> lift (Just <$> deserializeJWK rawKey)

tryGetPublicKey  :: PerspectivesUser -> MonadPerspectives (Maybe CryptoTypes.CryptoKey)
tryGetPublicKey author = entityExists (perspectivesUser2RoleInstance $ deltaAuthor2ResourceIdentifier author) >>= if _ 
  then getPublicKey author
  else pure Nothing

deserializeJWK :: String -> Aff CryptoTypes.CryptoKey
deserializeJWK rawKey = do
  -- keyBuff <- liftAff $ liftEffect $ string2buff rawKey
  -- Even though `importKey` specifies a `buffer` as second argument, the Crypto.subtle API specifies a JSON object.
  case runExcept (parseJSON rawKey) of
    Left e -> throwError (error $ "Cannot parse JWK json: " <> show e)
    Right jwk -> importKey CryptoTypes.jwk (unsafeCoerce jwk) (ec ECConstants.ecdsa ECConstants.p384) true [CryptoTypes.verify]

-- | Get my private key (to sign a delta).
-- | This is used on system startup. It takes the private key from IndexedDB 
-- | in order to put it in Perspectives State.
getPrivateKey :: MonadPerspectives (Maybe CryptoTypes.CryptoKey)
getPrivateKey = do
  PerspectivesUser id <- getPerspectivesUser
  privateKey <- lift $ getCryptoKey $ (takeGuid id) <> privateKeyString
  pure privateKey

-- | Get my public key. This will only be used on setting up an installation.
-- | It is taken from IndexedDB.
getMyPublicKey :: MonadPerspectives (Maybe String)
getMyPublicKey = do
  PerspectivesUser id <- getPerspectivesUser
  publicKey <- lift $ getCryptoKey $ (takeGuid id) <> publicKeyString
  for publicKey
    \key -> lift $ do 
      jwk <- CryptoTypes.exportKey CryptoTypes.jwk key
      -- NOTE. The Purescript library Crypto.Subtle contains an error here. `exportKey` always returns an ArrayBuffer,
      -- but https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/exportKey#return_value clearly states that in case
      -- export format JWK a JSON object is returned. This is what we observe. We therefore use unsafeStringify rather then writeJSON
      pure $ unsafeStringify jwk

getCryptoKey :: String -> Aff (Maybe CryptoKey)
getCryptoKey = unsafeCoerce idbGet

publicKeyString :: String
publicKeyString = "_publicKey"

privateKeyString :: String
privateKeyString = "_privateKey"

string2buff :: String -> Effect ArrayBuffer
string2buff s = do
  textEncoder <- liftEffect Encoder.new
  pure $ buffer $ Encoder.encode s textEncoder

foreign import dataUrlToBytesImpl :: EffectFn1 String (Promise Uint8Array)

dataUrlToBytesImpl_ :: String -> Effect (Promise Uint8Array)
dataUrlToBytesImpl_ = runEffectFn1 dataUrlToBytesImpl

dataUrlToBytes :: String -> Aff Uint8Array
dataUrlToBytes = dataUrlToBytesImpl_ >>> toAffE

foreign import bytesToBase64DataUrlImpl :: EffectFn1 Uint8Array (Promise String)

bytesToBase64DataUrlImpl_ :: Uint8Array -> Effect (Promise String)
bytesToBase64DataUrlImpl_ = runEffectFn1 bytesToBase64DataUrlImpl

bytesToBase64DataUrl :: Uint8Array -> Aff String
bytesToBase64DataUrl = bytesToBase64DataUrlImpl_ >>> toAffE

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
