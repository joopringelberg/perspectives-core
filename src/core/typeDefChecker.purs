module Perspectives.TypeDefChecker where

import Control.Monad.Writer (WriterT, execWriterT, lift, tell)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Perspectives.CoreTypes (MP, TypeID, TypedTripleGetter, UserMessage(..), MonadPerspectivesQuery, runMonadPerspectivesQuery, tripleGetter2function, tripleObjects, (##))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, RolID)
import Perspectives.PropertyComposition ((>->))
import Perspectives.QueryAST (ElementaryQueryStep(..))
import Perspectives.QueryCombinators (toBoolean)
import Perspectives.QueryCompiler (constructQueryFunction)
import Perspectives.QueryFunctionDescriptionCompiler (compileElementaryQueryStep)
import Perspectives.SystemQueries (contextExternePropertyTypes, contextInternePropertyTypes, contextRolTypes, contextType, isVerplicht, rolPropertyTypes)
import Perspectives.Utilities (ifNothing)
import Prelude (Unit, bind, discard, ifM, pure, unit, void, ($), (*>), (<<<), (<>), (>>=))

type TDChecker e = WriterT (Array UserMessage) (MonadPerspectivesQuery e)

checkContext :: forall e. ContextID -> MP e (Array UserMessage)
checkContext cid = execWriterT $ checkContext' cid

checkContext' :: forall e. ContextID -> TDChecker e Unit
checkContext' cid = do
  ifNothing (lift $ runMonadPerspectivesQuery cid (tripleGetter2function contextType))
    (tell [MissingType cid])
    \tp -> checkProperties tp cid *> checkRoles tp cid

checkProperties :: forall e. TypeID -> ContextID -> TDChecker e Unit
checkProperties typeId cid = do
  -- TODO. Neem ook properties uit Aspecten mee! De mogelijkeBinding valt buiten deze check, dat komt als de binding zelf gecheckt wordt.
  void $ (get typeId contextInternePropertyTypes) >>= (traverse (checkInternalProperty cid))
  -- TODO: Each internal property must be defined.
  -- TODO. Neem ook properties uit Aspecten mee!
  void $ (get typeId contextExternePropertyTypes) >>= (traverse (checkProperty (cid <> "_buitenRol")))
  -- TODO: Each external property must be defined.

get :: forall e. TypeID -> TypedTripleGetter e -> TDChecker e (Array ID)
get typeId tg = lift $ (typeId ## tg) >>= pure <<< tripleObjects

-- TODO: each role must be defined. This involves a check the other way round.
-- TODO: neem rollen uit aspecten mee.
checkRoles :: forall e. TypeID -> ContextID -> TDChecker e Unit
checkRoles typeId cid = void $ (get typeId contextRolTypes) >>= (traverse (checkRol cid))

-- To check:
--  * if the property is mandatory, is it present?
--  * guess the type of the property value of the given name. Does it have the range as Aspect?
--  * if the property is functional, not more than one value may be present.
checkInternalProperty :: forall e. ContextID -> TypeID -> TDChecker e Unit
checkInternalProperty cid propertyType = pure unit

-- Note: properties can be mandatory.
checkProperty :: forall e. RolID -> TypeID -> TDChecker e Unit
checkProperty rid propertyType = pure unit

-- Note: roles can be mandatory.
-- To check:
--  * the properties of the role
--  * the binding of the role.
checkRol :: forall e. ContextID -> TypeID -> TDChecker e Unit
checkRol cid rolType = do
  -- retrieve this type of rol from the context.
  -- TODO: haal de getter op via de cache.
  rolGetter <- lift $ (constructQueryFunction rolType)
  mrolId <- lift $ (runMonadPerspectivesQuery cid (tripleGetter2function rolGetter))
  case mrolId of
    Nothing -> ifM (lift (runMonadPerspectivesQuery rolType rolIsMandatory) )
      (tell [MissingRolInstance rolType cid])
      (pure unit)
    (Just rolId) -> do
      rolPropertyTypes <- lift $ (rolType ## rolPropertyTypes) >>= pure <<< tripleObjects
      void $ (traverse (checkProperty rolId)) rolPropertyTypes
      pure unit

rolIsMandatory :: forall e. RolID -> MonadPerspectivesQuery (AjaxAvarCache e) Boolean
rolIsMandatory = toBoolean isVerplicht
