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

-- | The Repository module has functions to
-- |  * retrieve a model from a repository.
-- |  * add a model to the user's personal collection of models. This function also adds model instances.

module Perspectives.TypePersistence.Repository where

import Affjax (Request, request)
import Control.Monad.Error.Class (catchError, throwError)
import Data.FoldableWithIndex (forWithIndex_)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Couchdb (onAccepted, onCorrectCallAndResponse, version)
import Perspectives.Couchdb.Databases (defaultPerspectRequest)
import Perspectives.DomeinCache (storeDomeinFileInCouchdb)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Persistent (saveEntiteit_)
import Perspectives.Representation.Class.Revision (changeRevision)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Prelude (Unit, bind, discard, pure, show, unit, ($), (<>))

type URL = String

-- For now, we assume no authentication is necessary to retrieve a model from a repository.
-- This will change in the future.
-- | Fetches the DomeinFile located at the URL. Fails if the result is not well-formed.
fetchModelFromRepository :: URL -> MonadPerspectives DomeinFile
fetchModelFromRepository url = catchError
  do
    (rq :: (Request String)) <- defaultPerspectRequest
    res <- liftAff $ request $ rq {url = url}
    m <- liftAff $ onAccepted res.status [200, 304] "fetchModelFromRepository"
      (onCorrectCallAndResponse "fetchEntiteit" res.body \(m :: DomeinFile) -> pure unit)
    rev <- version res.headers
    pure $ changeRevision rev m
  \e -> throwError $ error ("fetchModelFromRepository: failed to retrieve model from this url: " <> url <> "." <> show e)

-- | Put the DomeinFile in the user's personal Models database.
-- | Adds the Model instances to the user's entities.
startUsingNewModel :: DomeinFile -> MonadPerspectives Unit
startUsingNewModel df@(DomeinFile{contextInstances, roleInstances}) = do
  storeDomeinFileInCouchdb df
  forWithIndex_ contextInstances \s e -> saveEntiteit_ (ContextInstance s) e
  forWithIndex_ roleInstances \s e -> saveEntiteit_ (RoleInstance s) e
