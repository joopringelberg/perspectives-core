module Perspectives.ComputedTripleGetters where

import Prelude

import Control.Monad.Error.Class (catchError)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (for_)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX)
import Perspectives.CoreTypes (MonadPerspectivesQuery, TypedTripleGetter, MonadPerspectives)
import Perspectives.DomeinCache (documentNamesInDatabase)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Perspectives.QueryCache (queryCacheInsert)
import Perspectives.TripleGetterConstructors (constructExternalPropertyGetter, constructTripleGetterWithArbitrarySupport)

-- | This TypedTripleGetter computes a list of the IDs of all models that are available to this system.
modellenM :: forall e1 e2. TypedTripleGetter e1
modellenM = constructTripleGetterWithArbitrarySupport
  "model:Systeem$Systeem$modellen" getListOfModels (constructExternalPropertyGetter "model:Systeem$TrustedCluster$buitenRolBeschrijving$modelOphaalTeller")

getListOfModels :: forall e. (ID -> MonadPerspectivesQuery (ajax :: AJAX | e) (Array String))
getListOfModels id = lift $ lift $ catchError (documentNamesInDatabase "perspect_models") \_ -> pure []

addComputedTripleGetters :: forall e. MonadPerspectives (gm :: GLOBALMAP | e) Unit
addComputedTripleGetters = for_ [Tuple "modellenM" modellenM] \(Tuple n f) -> queryCacheInsert n f
