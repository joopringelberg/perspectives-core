-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

module Perspectives.SerializableNonEmptyArray where

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Array.NonEmpty (NonEmptyArray, toArray, fromArray, singleton) as NER
import Data.Either (Either(..))
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Foreign (ForeignError(..), readString)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Prelude (class Eq, class Functor, class Ord, class Semigroup, class Show, pure, show, ($), (<<<), (<>), bind)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON', write)

newtype SerializableNonEmptyArray a = SerializableNonEmptyArray (NER.NonEmptyArray a)

instance showSerializableNonEmptyArray :: Show a => Show (SerializableNonEmptyArray a) where
  show (SerializableNonEmptyArray arr) = "SerializableNonEmptyArray" <> show arr

derive newtype instance eqSerializableNonEmptyArray :: Eq a => Eq (SerializableNonEmptyArray a)

derive newtype instance ordSerializableNonEmptyArray :: Ord a => Ord (SerializableNonEmptyArray a)

derive newtype instance semigroupSerializableNonEmptyArray :: Semigroup (SerializableNonEmptyArray a)

derive newtype instance functorSerializableNonEmptyArray :: Functor SerializableNonEmptyArray
derive newtype instance functorWithIndexSerializableNonEmptyArray :: FunctorWithIndex Int SerializableNonEmptyArray

derive instance newtypeSerializableNonEmptyArray :: Newtype (SerializableNonEmptyArray a) _

instance encodeSerializableNonEmptyArray :: Encode a => Encode (SerializableNonEmptyArray a) where
  encode (SerializableNonEmptyArray arr) = encode (NER.toArray arr)

instance decodeSerializableNonEmptyArray :: Decode a => Decode (SerializableNonEmptyArray a) where
  decode f = case (runExcept (decode f)) of
    Left e -> throwError e
    Right (arr :: Array a) -> case NER.fromArray arr of
      Nothing -> throwError (NEL.singleton (ForeignError "SerializableNonEmptyArray cannot be empty"))
      Just narr -> pure (SerializableNonEmptyArray narr)

instance readForeignSerializableNonEmptyArray :: ReadForeign a => ReadForeign (SerializableNonEmptyArray a) where
  readImpl f = do
    s <- readString f
    arr <- readJSON' s
    case fromArray arr of
      Nothing -> throwError (NEL.singleton $ ForeignError "SerializableNonEmptyArray expects an array with elements.")
      Just a -> pure a

instance writeForeignSerializableNonEmptyArray :: WriteForeign a => WriteForeign (SerializableNonEmptyArray a) where
  writeImpl (SerializableNonEmptyArray a) = write $ NER.toArray a

fromArray :: forall a. Array a -> Maybe (SerializableNonEmptyArray a)
fromArray arr = case NER.fromArray arr of
  Nothing -> Nothing
  Just narr -> Just $ SerializableNonEmptyArray narr

singleton :: forall a. a -> SerializableNonEmptyArray a
singleton a = SerializableNonEmptyArray $ NER.singleton a

toNonEmptyArray :: forall a. SerializableNonEmptyArray a -> NER.NonEmptyArray a
toNonEmptyArray = unwrap

toArray :: forall a. SerializableNonEmptyArray a -> Array a
toArray = NER.toArray <<< unwrap
