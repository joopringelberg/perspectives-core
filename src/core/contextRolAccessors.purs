module Perspectives.ContextRolAccessors where

import Control.Monad.Error.Class (catchError)
import Data.Maybe (Maybe(..))
import Perspectives.CoreTypes (MonadPerspectives, ObjectsGetter)

import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.Syntax (PerspectContext, PerspectRol)
import Prelude (pure, (>=>), (<<<), (>>=))

-- Can we safely assume that the PerspectContext will exist? If it does not because there is no model holding it,
-- this function will break.
getContextMember :: (PerspectContext -> Array String) -> ObjectsGetter
getContextMember f = getPerspectEntiteit >=> pure <<< f

-- Even though members of a context will always be present, the context itself may not. Hence we return a Maybe value.
getContextMember' :: forall a. (PerspectContext -> a) -> (ID -> MonadPerspectives (Maybe a))
getContextMember' f c = catchError (getPerspectEntiteit c >>= pure <<< Just <<< f) \_ -> pure Nothing

getRolMember :: (PerspectRol -> Array String) -> ObjectsGetter
getRolMember f = getPerspectEntiteit >=> pure <<< f
