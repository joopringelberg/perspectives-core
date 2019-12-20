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

module Perspectives.Instances.Values where


-- | Parse a date. See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse#Date_Time_String_Format for the supported string format of the date.
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.JSDate (JSDate, parse, toDateTime)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error, try)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Prelude (bind, ($), pure, (<>), show)

-- TODO. We gebruiken hier Error, het javascript error type. Liever zou ik een
-- PerspectivesRuntimeError type gebruiken. Maar dan moeten we MonadPerspectives aanpassen.

-- | Parse a date. See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse#Date_Time_String_Format for the supported string format of the date.
parseDate :: forall m. MonadError Error m => String -> m DateTime
parseDate s = do
  (d :: JSDate) <- pure $ unsafePerformEffect $ parse s
  case toDateTime d of
    Nothing -> throwError $ error "Not a date"
    (Just (dt :: DateTime)) -> pure dt

foreign import parseInt__ :: EffectFn1 String Int

parseInt_ :: String -> Effect Int
parseInt_ = runEffectFn1 parseInt__

parseInt :: forall m. MonadError Error m => MonadEffect m => String -> m Int
parseInt s = do
  r <- liftEffect $ try (parseInt_ s)
  case r of
    Left e -> throwError (error $ "Cannot parse an integer from '" <> s <> "' (" <> show e <> ")")
    Right i -> pure i
