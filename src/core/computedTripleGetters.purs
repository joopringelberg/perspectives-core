module Perspectives.ComputedTripleGetters where

import Prelude

import Control.Monad.Error.Class (catchError)
import Control.Monad.Trans.Class (lift)
import Network.HTTP.Affjax (AJAX)
import Perspectives.CoreTypes (TypedTripleGetter, MonadPerspectivesQuery)
import Perspectives.DomeinCache (documentNamesInDatabase)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.TripleGetterConstructors (constructRolPropertyGetter, constructTripleGetterWithArbitrarySupport)

-- | This TypedTripleGetter computes a list of the IDs of all models that are available to this system.
modelsM :: forall e1 e2. TypedTripleGetter e1
modelsM = constructTripleGetterWithArbitrarySupport
  "model:Systeem$Systeem$models" getListOfModels (constructRolPropertyGetter "model:Systeem$Systeem$buitenRolBeschrijving$modelRetrievalCount")

getListOfModels :: forall e. (ID -> MonadPerspectivesQuery (ajax :: AJAX | e) (Array String))
getListOfModels id = lift $ lift $ catchError (documentNamesInDatabase "perspect_models") \_ -> pure []
