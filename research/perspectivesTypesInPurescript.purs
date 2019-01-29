module Perspectives.PerspectivesTypesInPurescript where

import Prelude

import Control.Monad.Aff.AVar (readVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.AVar (AVar)
import Data.Maybe (Maybe(..))
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.PerspectivesState (contextDefinitionsLookup)
import Perspectives.ResourceRetrieval (fetchPerspectEntiteitFromCouchdb)
import Perspectives.Syntax (PerspectContext(..))


newtype Context = Context String
newtype Rol = Rol String
newtype Val = Val String
newtype PBool = PBool String -- type level strings?
newtype PString = PString String

newtype ContextDef = ContextDef String
newtype RolDef = RolDef String
newtype PropertyDef = PropertyDef String

type OGetter s o e = s -> MonadPerspectives (AjaxAvarCache e) (Array o)

infixr 4 type OGetter as ~~>


contextType :: forall e. (Context ~~> ContextDef) e
contextType = getContextMember \context -> [context_pspType context]

context_pspType :: PerspectContext -> ContextDef
context_pspType (PerspectContext{pspType})= ContextDef pspType

getContextMember :: forall a e. (PerspectContext -> Array a) -> (Context -> MonadPerspectives (AjaxAvarCache e) (Array a))
getContextMember f = getPerspectContext >=> pure <<< f

getPerspectContext :: forall e a. Context -> MonadPerspectives (AjaxAvarCache e) PerspectContext
getPerspectContext (Context id) =
  do
    (av :: Maybe (AVar PerspectContext)) <- contextDefinitionsLookup id
    case av of
      (Just avar) -> do
        pe <- liftAff $ readVar avar
        pure pe
      Nothing -> do
        (ent :: PerspectContext) <- fetchPerspectEntiteitFromCouchdb id
        pure ent

-- newtype PerspectContext = PerspectContext ContextRecord
--
-- type ContextRecord =
--   { _id :: Context
--   , _rev :: Revision
--   , displayName :: String
--   , pspType :: ContextDef
--   , binnenRol :: Rol
--   , buitenRol :: Rol
--   , rolInContext :: StrMap (Array Rol)
--   , comments :: Comments
--   }
