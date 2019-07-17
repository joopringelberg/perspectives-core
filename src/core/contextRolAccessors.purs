module Perspectives.ContextRolAccessors where

import Control.Monad.Error.Class (catchError)
import Data.Maybe (Maybe(..))
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Instances (getPerspectEntiteit)
import Perspectives.Instances.Aliases (AnyContext, RoleInstance)
import Prelude (pure, (>=>), (<<<), (>>=))

-- Can we safely assume that the PerspectContext will exist? If it does not because there is no model holding it,
-- this function will break.
getContextMember :: forall a. (PerspectContext -> a) -> (AnyContext -> MonadPerspectives a)
getContextMember f = getPerspectEntiteit >=> pure <<< f

-- Even though members of a context will always be present, the context itself may not. Hence we return a Maybe value.
getContextMember' :: forall a. (PerspectContext -> a) -> (ID -> MonadPerspectives (Maybe a))
getContextMember' f c = catchError (getPerspectEntiteit c >>= pure <<< Just <<< f) \_ -> pure Nothing

getRolMember :: forall a. (PerspectRol -> a) -> (RoleInstance -> MonadPerspectives a)
getRolMember f = getPerspectEntiteit >=> pure <<< f
