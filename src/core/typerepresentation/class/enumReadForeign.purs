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

module Perspectives.Representation.Class.EnumReadForeign where

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), to)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign (Foreign, F, ForeignError(..))
import Prelude ((<<<), (==), (<$>), bind, ($), pure, (<>))
import Simple.JSON (readImpl) 
import Type.Proxy (Proxy(..))

enumReadForeign :: forall a rep
   . Generic a rep
  => EnumReadForeign rep
  => Foreign
  -> F a
enumReadForeign f =
  to <$> enumReadForeignImpl f

-- type class for "enums", or nullary sum types
class EnumReadForeign rep where
  enumReadForeignImpl :: Foreign -> F rep

instance sumEnumReadForeign ::
  ( EnumReadForeign a
  , EnumReadForeign b
  ) => EnumReadForeign (Sum a b) where
  enumReadForeignImpl f
      = Inl <$> enumReadForeignImpl f
    <|> Inr <$> enumReadForeignImpl f

instance constructorEnumReadForeign ::
  ( IsSymbol name
  ) => EnumReadForeign (Constructor name NoArguments) where
  enumReadForeignImpl f = do
    s <- readImpl f
    if s == name
       then pure $ Constructor NoArguments
       else throwError <<< pure <<< ForeignError $
            "Enum string " <> s <> " did not match expected string " <> name
    where
      name = reflectSymbol (Proxy :: Proxy name)
