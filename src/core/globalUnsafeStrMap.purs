-- | A totally unsafe StrMap, tracked by an effect.
-- |

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

module Perspectives.GlobalUnsafeStrMap
  ( GLStrMap
  , newMap
  , peek
  , poke
  , delete
  , clear
  , ensure
  , modify
  , keys
  , values
  , filterKeys
  , delete'
  ) where

import Data.Maybe (Maybe(..))
import Foreign (Foreign, isUndefined, unsafeFromForeign)
import Foreign.Object (Object)
import Prelude (class Show, Unit, show)
import Unsafe.Coerce (unsafeCoerce)

-- | A reference to a mutable map
-- |
-- | The type parameter defines the type of elements of the mutable array.
-- |
foreign import data GLStrMap :: Type -> Type

-- | Create a new, empty mutable map
foreign import newMap :: forall a. Unit -> GLStrMap a

-- | Get the value for a key in a global map
peek :: forall a. GLStrMap a -> String -> (Maybe a)
peek map key = let
  x = peekImpl map key in
  if isUndefined x then Nothing else (Just (unsafeFromForeign x))

-- | Look up a key. Returns the value found or the default value and then puts the default under that key in the map.
ensure :: forall a. GLStrMap a -> String -> a -> a
ensure map key default = let
  ma = peek map key in
  case ma of
    Nothing -> let ignore = poke map key default in default
    (Just a) -> a

-- | Modify the value at the key with a function.
modify :: forall a. GLStrMap a -> String -> (a -> a) -> a -> GLStrMap a
modify map key f default = poke map key (f (ensure map key default) )

foreign import peekImpl :: forall a. GLStrMap a -> String -> Foreign

-- | Update the value for a key in a global map
foreign import poke :: forall a. GLStrMap a -> String -> a -> (GLStrMap a)

-- | Remove a key and the corresponding value from a global map
foreign import delete_ :: forall a. GLStrMap a -> String -> Foreign

delete :: forall a. GLStrMap a -> String -> Maybe a
delete map key = let
  x = delete_ map key in
  if isUndefined x then Nothing else (Just (unsafeFromForeign x))

instance showGLStrMap :: Show a => Show (GLStrMap a) where
  show s = show (unsafeCoerce s :: Object a)

-- | Remove all keys and values.
foreign import clear :: forall a. GLStrMap a -> (GLStrMap a)

foreign import keys :: forall a. GLStrMap a -> Array String

foreign import values :: forall a. GLStrMap a -> Array a

-- instance indexForeignObject :: Index (GLStrMap v) String v where
--   ix k =
--     wander \coalg m ->
--       peek m k #
--         maybe
--           (pure m)
--           (coalg >>> map \v -> poke m k v)

-- instance atForeignGLStrMap :: At (GLStrMap a) String a where
--   at k =
--     lens ((flip peek) k) \m ->
--       maybe (delete' k m) \ v -> poke m k v

delete' :: forall a. String -> GLStrMap a -> GLStrMap a
delete' k m = let
  x = delete_ m k
  in m

-- | Remove all key-value pairs that do not satisfy a predicate.
foreign import filterKeys :: forall a. (String -> Boolean) -> GLStrMap a -> GLStrMap a
