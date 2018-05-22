module Perspectives.ContextRolAccessors where

import Control.Monad.Eff.Exception (error)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Perspectives.CoreTypes (MonadPerspectives, ObjectsGetter, ObjectGetter)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.Syntax (PerspectContext, PerspectRol)
import Perspectives.Utilities (onNothing)
import Prelude (bind, pure, ($), (<<<), (<>), (>=>))

getContextMember :: forall e. (PerspectContext -> Array String) -> ObjectsGetter e
getContextMember f c = do
  maybeContext <- getPerspectEntiteit c
  case maybeContext of
    (Just perspectContext) -> pure $ f perspectContext
    otherwise -> pure []

-- Even though members of a context will always be present, the context itself may not. Hence we return a Maybe value.
getContextMember' :: forall a e. (PerspectContext -> a) -> (ID -> MonadPerspectives (AjaxAvarCache e) (Maybe a))
getContextMember' f c = do
  maybeContext <- getPerspectEntiteit c
  case maybeContext of
    (Just perspectContext) -> pure $ Just $ f perspectContext
    otherwise -> pure Nothing

getRolMember :: forall e. (PerspectRol -> Array String) -> ObjectsGetter e
getRolMember f c = do
  maybeRol <- getPerspectEntiteit c
  case maybeRol of
    (Just perspectRol) -> pure $ f perspectRol
    otherwise -> pure []

makeFunction :: forall e. String -> ObjectsGetter e -> ObjectGetter e
makeFunction name og = og >=> (\ta -> onNothing (error $ "Function yields no value: " <> name) (pure (head ta)))

firstOnly :: forall e. ObjectsGetter e -> (ID -> MonadPerspectives (AjaxAvarCache e) (Maybe String))
firstOnly g = g >=> (pure <<< head)
