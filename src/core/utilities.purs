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

module Perspectives.Utilities where

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), maybe)
import Foreign.Object (Object, foldMap)
import Prelude (class Monad, class Show, type (~>), bind, pure, show, (<$>), (<<<), (<>), (>>=))

onNothing :: forall m a e. MonadThrow e m => e -> m (Maybe a) -> m a
onNothing s ma = ma >>= (maybe (throwError s) pure)

onNothing' :: forall m e. MonadThrow e m => e -> Maybe ~> m
onNothing' s = maybe (throwError s) pure

maybeM :: forall a b m. Monad m => m a -> (b -> m a) -> m (Maybe b) -> m a
maybeM default fromJust monadicValue = do
  mv <- monadicValue
  case mv of
    Nothing -> default
    (Just v) -> fromJust v

ifNothing :: forall a b m. Monad m => m (Maybe b) -> m a -> (b -> m a) -> m a
ifNothing monadicValue default fromJust = maybeM default fromJust monadicValue

prettyPrint :: forall a. PrettyPrint a => a -> String
prettyPrint a = prettyPrint' "  " a

class PrettyPrint d where
  prettyPrint' :: String -> d -> String

instance intPrettyPrint :: PrettyPrint Int where
  prettyPrint' indent = (<>) indent <<< show

instance stringPrettyPrint :: PrettyPrint String where
  prettyPrint' indent = (<>) indent

instance boolPrettyPrint :: PrettyPrint Boolean where
  prettyPrint' indent = (<>) indent <<< show

instance objectPrettyPrint :: PrettyPrint v => PrettyPrint (Object v) where
  prettyPrint' tab o = "{ " <> (foldMap (\(s :: String) (v :: v) -> newline <> tab <> s <> ": " <> (prettyPrint' (tab <> "  ") v)) o) <> " }"

instance arrayPrettyPrint :: (Show v, PrettyPrint v) => PrettyPrint (Array v) where
  prettyPrint' tab a = "[" <> fold (((<>) newline <<< (prettyPrint' (tab <> "  "))) <$> a) <> newline <> "]"

newline :: String
newline = "\n"
