module Test.CacheEntiteit where

import Prelude

import Control.Monad.Aff.Console (log, logShow)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe)
import Perspectives.CoreTypes (MonadPerspectives, runMonadPerspectivesQueryCompiler)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.QueryFunctionDescriptionCompiler (createDataTypeGetterDescription)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.ResourceRetrieval (fetchEntiteit, fetchPerspectEntiteitFromCouchdb)
import Perspectives.Syntax (PerspectContext, PerspectRol(..))

test :: forall e. MonadPerspectives (AjaxAvarCache (console :: CONSOLE | e)) Unit
test = do
  lift $ log "=========================Test.CacheEntiteit================================"
  -- _ <- runMonadPerspectivesQueryCompiler "model:User" (createDataTypeGetterDescription "model:User$testGetter1" "binding")
  -- _ <- lift $ logShow "Jaja"
  -- (e :: Maybe PerspectContext) <- getPerspectEntiteit "model:User$testGetter1"
  (e :: PerspectRol) <- (fetchEntiteit "model:User$testRol1")
  lift $ logShow e
