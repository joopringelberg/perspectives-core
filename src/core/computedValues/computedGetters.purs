module Perspectives.Instances.ComputedGetters where

import Control.Monad.Error.Class (catchError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (tell)
import Data.Foldable (for_)
import Data.Tuple (Tuple(..))
import Perspectives.CoreTypes (type (~~>), MP, RoleGetter, assumption)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinCache (documentNamesInDatabase)
import Perspectives.ObjectGetterLookup (roleGetterCacheInsert)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance(..))
import Prelude (Unit, discard, map, pure, ($), (<<<), (<>), (>>=))

modellenM :: ContextInstance ~~> RoleInstance
modellenM c = ArrayT do
  tell [assumption "model:User$MijnSysteem" ophaalTellerName]
  lift $ getListOfModels

  where
    getListOfModels :: MP (Array RoleInstance)
    getListOfModels = lift $ catchError ((documentNamesInDatabase "perspect_models") >>= pure <<< map (RoleInstance <<< (_ <> "$External"))) \_ -> pure []

    ophaalTellerName :: String
    ophaalTellerName = "model:System$PerspectivesSystem$External$ModelOphaalTeller"

-- | An Array of RoleGetters. Each RoleGetter is inserted into the RoleGetterCache and can be retrieved
-- | with `Perspectives.ObjectGetterLookup.lookupRoleGetterByName`.
computedRoleGetters :: Array (Tuple String RoleGetter)
computedRoleGetters = [
  Tuple "modellenM" modellenM
]

addComputedTripleGetters :: MP Unit
addComputedTripleGetters = for_ computedRoleGetters \(Tuple n f) -> pure $ roleGetterCacheInsert n f
