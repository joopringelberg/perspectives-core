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

-- | The type HiddenFunction represents functions of varying signature that we want to hide from the
-- | Purescript compiler. We will unsafely coerce that type to the function we like
-- | when we feel we can do it.

module Perspectives.HiddenFunction where 

import Prelude



import Simple.JSON (class ReadForeign, class WriteForeign)
import Unsafe.Coerce (unsafeCoerce)

foreign import data HiddenFunction :: Type

instance showHiddenFunction :: Show HiddenFunction where
  show _ = "HiddenFunction"

instance eqHiddenFunction :: Eq HiddenFunction where
  eq _ _ = true




-- As we never read a HiddenFunction, this does not matter.


instance WriteForeign HiddenFunction where
-- The javascript null value, typed as Foreign
  writeImpl _ = unsafeCoerce ""

-- As we never read a HiddenFunction, this does not matter.
instance ReadForeign HiddenFunction where
  readImpl _ = unsafeCoerce unit
