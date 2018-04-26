module Perspectives.TypeDefChecker (checkContext)

where

import Control.Monad.Writer (WriterT, execWriterT, lift, tell)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Perpectives.TypeChecker (rolIsInstanceOfType)
import Perspectives.CoreTypes (MP, MonadPerspectivesQuery, Triple(..), TypeID, TypedTripleGetter, UserMessage(..), MonadPerspectives, runMonadPerspectivesQuery, tripleGetter2function, tripleObjects, (@@))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, RolID)
import Perspectives.QueryCache (queryCacheLookup)
import Perspectives.QueryCombinators (toBoolean)
import Perspectives.QueryCompiler (constructQueryFunction)
import Perspectives.SystemQueries (binding, contextExternePropertyTypes, contextInternePropertyTypes, contextRolTypes, contextType, isVerplicht, mogelijkeBinding, rolPropertyTypes)
import Perspectives.Utilities (ifNothing)
import Prelude (Unit, bind, discard, ifM, pure, unit, void, ($), (*>), (<<<), (<>), (>>=), id, (<*>), (<$>))

type TDChecker e = WriterT (Array UserMessage) (MonadPerspectivesQuery e)

checkContext :: forall e. ContextID -> MP (AjaxAvarCache e) (Array UserMessage)
checkContext cid = runMonadPerspectivesQuery cid \x -> execWriterT $ checkContext' x

checkContext' :: forall e. ContextID -> TDChecker (AjaxAvarCache e) Unit
checkContext' cid = do
  ifNothing (lift $ tripleGetter2function contextType cid)
    (tell [MissingType cid])
    \tp -> checkProperties tp cid *> checkRoles tp cid

checkProperties :: forall e. TypeID -> ContextID -> TDChecker (AjaxAvarCache e) Unit
checkProperties typeId cid = do
  -- TODO. Neem ook properties uit Aspecten mee! De mogelijkeBinding valt buiten deze check, dat komt als de binding zelf gecheckt wordt.
  void $ (typeId ~> contextInternePropertyTypes) >>= (traverse (checkInternalProperty cid))
  -- TODO: Each internal property must be defined.
  -- TODO. Neem ook properties uit Aspecten mee!
  void $ (typeId ~> contextExternePropertyTypes) >>= (traverse (checkProperty (cid <> "_buitenRol")))
  -- TODO: Each external property must be defined.

get :: forall e. TypeID -> TypedTripleGetter e -> TDChecker (AjaxAvarCache e) (Array ID)
get typeId tg = lift $ (typeId @@ tg) >>= pure <<< tripleObjects

infix 0 get as ~>

-- TODO: each role must be defined. This involves a check the other way round.
-- TODO: neem rollen uit aspecten mee.
checkRoles :: forall e. TypeID -> ContextID -> TDChecker (AjaxAvarCache e) Unit
checkRoles typeId cid = void $ (typeId ~> contextRolTypes) >>= (traverse (checkRol cid))

-- To check:
--  * if the property is mandatory, is it present?
--  * guess the type of the property value of the given name. Does it have the range as Aspect?
--  * if the property is functional, not more than one value may be present.
checkInternalProperty :: forall e. ContextID -> TypeID -> TDChecker e Unit
checkInternalProperty cid propertyType = pure unit

-- Note: properties can be mandatory.
checkProperty :: forall e. RolID -> TypeID -> TDChecker e Unit
checkProperty rid propertyType = pure unit

-- TODO: the binding of the role.
-- | If the role is mandatory and missing, adds a message. Checks each defined property with the instance of the rol.
checkRol :: forall e. ContextID -> TypeID -> TDChecker (AjaxAvarCache e) Unit
checkRol cid rolType = do
  rolGetter <- lift $ lift $ getQueryFunction rolType
  (Triple {object}) <- lift (cid @@ rolGetter)
  case head object of
    Nothing -> ifM (lift (rolIsMandatory rolType))
      (tell [MissingRolInstance rolType cid])
      (pure unit)
    (Just rolId) -> do
      rolPropertyTypes <- lift $ (rolType @@ rolPropertyTypes) >>= pure <<< tripleObjects
      void $ (traverse (checkProperty rolId)) rolPropertyTypes
      -- check the binding. Does the binding have the type given by mogelijkeBinding, or has its type that Aspect?
      -- b <- lift (rolId @@ binding) >>= pure <<< tripleObjects
      -- mb <- lift (rolType @@ mogelijkeBinding) >>= pure <<< tripleObjects
      -- ifM (lift $ lift $ ([rolIsInstanceOfType] <$> b <*> mb))
      --   (pure unit)
      --   (tell [IncorrectBinding rolId rolType])

getQueryFunction :: forall e. TypeID -> MonadPerspectives (AjaxAvarCache e) (TypedTripleGetter e)
getQueryFunction tp = ifNothing (queryCacheLookup tp)
  (constructQueryFunction tp)
  (pure <<< id)

rolIsMandatory :: forall e. RolID -> MonadPerspectivesQuery (AjaxAvarCache e) Boolean
rolIsMandatory = toBoolean isVerplicht
