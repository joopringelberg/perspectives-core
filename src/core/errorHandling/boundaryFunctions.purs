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

module Perspectives.Error.Boundaries where

import Data.Either (Either(..))
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Prelude (Unit, show, ($), pure, unit, bind, (*>))

handlePerspectRolError :: forall a m r. MonadEffect m => String -> (r -> m a) -> Either Error r -> m Unit
handlePerspectRolError boundaryName f erole = case erole of
  Left err -> logPerspectivesError $ RolErrorBoundary boundaryName (show err)
  Right role -> do
    _ <- f role
    pure unit

handlePerspectRolError' :: forall a m r. MonadEffect m => String -> a -> (r -> m a) -> Either Error r -> m a
handlePerspectRolError' boundaryName default f erole = case erole of
  Left err -> (logPerspectivesError $ RolErrorBoundary boundaryName (show err)) *> pure default
  Right role -> f role

handlePerspectContextError :: forall a m r. MonadEffect m => String -> (r -> m a) -> Either Error r -> m Unit
handlePerspectContextError boundaryName f econtext = case econtext of
  Left err -> logPerspectivesError $ ContextErrorBoundary boundaryName (show err)
  Right ctxt -> do
    _ <- f ctxt
    pure unit

handlePerspectContextError' :: forall a m r. MonadEffect m => String -> a -> (r -> m a) -> Either Error r -> m a
handlePerspectContextError' boundaryName default f econtext = case econtext of
  Left err -> (logPerspectivesError $ ContextErrorBoundary boundaryName (show err)) *> pure default
  Right ctxt -> f ctxt

handleDomeinFileError :: forall a m r. MonadEffect m => String -> (r -> m a) -> Either Error r -> m Unit
handleDomeinFileError boundaryName f dfile = case dfile of
  Left err -> logPerspectivesError $ DomeinFileErrorBoundary boundaryName (show err)
  Right ctxt -> do
    _ <- f ctxt
    pure unit

handleDomeinFileError' :: forall a m r. MonadEffect m => String -> a -> (r -> m a) -> Either Error r -> m a
handleDomeinFileError' boundaryName default f dfile = case dfile of
  Left err -> (logPerspectivesError $ DomeinFileErrorBoundary boundaryName (show err)) *> pure default
  Right ctxt -> f ctxt
