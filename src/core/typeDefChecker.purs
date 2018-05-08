module Perspectives.TypeDefChecker (checkContext, propertyIsFunctional)

where

import Control.Monad.Writer (WriterT, execWriterT, lift, tell)
import Data.Argonaut.Core (fromString)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (elemIndex, head, length)
import Data.DateTime.ISO (ISO)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Number (fromString) as Nmb
import Data.Traversable (for_, traverse)
import Perpectives.TypeChecker (typeIsInstanceOfType)
import Perspectives.CoreTypes (MP, MonadPerspectivesQuery, Triple(..), TypeID, TypedTripleGetter, UserMessage(..), tripleGetter2function, tripleObject, tripleObjects, (@@))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, RolID, RolName)
import Perspectives.PropertyComposition ((>->))
import Perspectives.QueryCombinators (toBoolean)
import Perspectives.QueryCompiler (propertyQuery, rolQuery)
import Perspectives.RunMonadPerspectivesQuery ((##), runMonadPerspectivesQuery)
import Perspectives.SystemQueries (binding, contextExternePropertyTypes, contextInternePropertyTypes, contextRolTypes, contextType, isFunctionalProperty, mogelijkeBinding, propertyIsVerplicht, range, rolContext, rolIsVerplicht, rolPropertyTypes, rolTypen)
import Perspectives.Utilities (ifNothing)
import Prelude (Unit, bind, const, discard, ifM, pure, unit, void, ($), (*>), (<<<), (<>), (>>=), (<))

type TDChecker e = WriterT (Array UserMessage) (MonadPerspectivesQuery e)

checkContext :: forall e. ContextID -> MP e (Array UserMessage)
checkContext cid = runMonadPerspectivesQuery cid \x -> execWriterT $ checkContext' x

-- TODO. CONTROLEER RECURSIEF DE AAN ROLLEN GEBONDEN CONTEXTEN.
checkContext' :: forall e. ContextID -> TDChecker (AjaxAvarCache e) Unit
checkContext' cid = do
  ifNothing (lift $ tripleGetter2function contextType cid)
    (tell [MissingType cid])
    \tp -> checkProperties tp cid *> checkDefinedRoles tp cid *> checkAvailableRoles tp cid

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

checkDefinedRoles :: forall e. TypeID -> ContextID -> TDChecker (AjaxAvarCache e) Unit
checkDefinedRoles typeId cid = do
  (Triple{object: definedRollen}) <- lift $ lift $ (typeId ## contextRolTypes)
  void $ traverse (checkRol cid) definedRollen

-- | Does the type hold a definition for all roles given to the ContextInstantie?
checkAvailableRoles :: forall e. TypeID -> ContextID -> TDChecker (AjaxAvarCache e) Unit
checkAvailableRoles typeId cid = do
  (Triple{object: availableRoles}) <- lift (cid @@ rolTypen)
  for_ availableRoles (isDefined typeId)
  where
    isDefined :: RolName -> RolName -> TDChecker (AjaxAvarCache e) Unit
    isDefined contextType rolType = do
      (Triple{object: definedRollen}) <- lift $ lift $ (typeId ## contextRolTypes)
      case elemIndex rolType definedRollen of
        Nothing -> tell [RolNotDefined rolType cid contextType]
        otherwise -> pure unit

-- To check:
--  * if the property is mandatory, is it present?
--  * guess the type of the property value of the given name. Does it have the range as Aspect?
--  * if the property is functional, not more than one value may be present.
checkInternalProperty :: forall e. ContextID -> TypeID -> TDChecker e Unit
checkInternalProperty cid propertyType = pure unit

-- | If the Property is mandatory and missing, adds a message.
-- | Checks the value of the Property with the range that has been defined.

--  * if the property is functional, not more than one value may be present.
checkProperty :: forall e. RolID -> TypeID -> TDChecker (AjaxAvarCache e) Unit
checkProperty rid propertyType = do
  propertyGetter <- lift $ propertyQuery propertyType rid
  (Triple {object}) <- lift (rid @@ propertyGetter)
  case head object of
    Nothing -> ifM (lift (propertyIsMandatory propertyType))
      (tell [MissingPropertyValue propertyType rid])
      (pure unit)
    (Just propertyValue) -> do
      mrange <- lift (propertyType @@ range)
      case head (tripleObjects mrange) of
        Nothing -> pure unit -- There should be a range, however, we protect this function from failing on it.
        (Just sv) -> ifM (tryParseSimpleValue sv propertyValue)
          (pure unit)
          (tell [IncorrectPropertyValue propertyType sv propertyValue])
      if length object < 2
        then pure unit
        else ifM (lift $ propertyIsFunctional propertyType)
          (tell [TooManyPropertyValues propertyType])
          (pure unit)

  where
    tryParseSimpleValue :: TypeID -> String -> TDChecker (AjaxAvarCache e) Boolean
    tryParseSimpleValue sv propertyValue = case sv of
      "Number" -> pure $ maybe false (const true) (Nmb.fromString propertyValue)
      "Boolean" -> case propertyValue of
        "true" -> pure true
        "false" -> pure true
        otherwise -> pure false
      "Date" -> -- Dates *should* be represented as ISO strings.
        case decodeJson $ fromString propertyValue of
          (Left err :: Either String ISO) -> pure false
          (Right iso :: Either String ISO) -> pure true
      otherwise -> pure true

-- | If the Role is mandatory and missing, adds a message. Checks each defined property with the instance of the rol.
-- | Checks the type of the binding, if given.
checkRol :: forall e. ContextID -> TypeID -> TDChecker (AjaxAvarCache e) Unit
checkRol cid rolType' = do
  rolGetter <- lift $ rolQuery rolType' cid
  (Triple {object}) <- lift (cid @@ rolGetter)
  case head object of
    Nothing -> ifM (lift (rolIsMandatory rolType'))
      (tell [MissingRolInstance rolType' cid])
      (pure unit)
    (Just rolId) -> do
      rolPropertyTypes <- lift $ (rolType' @@ rolPropertyTypes)
      void $ (traverse (checkProperty rolId)) (tripleObjects rolPropertyTypes)
      -- check the binding. Does the binding have the type given by mogelijkeBinding, or has its type that Aspect?
      typeOfTheBinding <- lift (rolId @@ (binding >-> rolContext))
      mmb <- lift (rolType' @@ mogelijkeBinding)
      case head (tripleObjects mmb) of
        Nothing -> pure unit
        (Just toegestaneBinding) -> do
          ifM (lift $ lift $ typeIsInstanceOfType (tripleObject typeOfTheBinding) toegestaneBinding)
            (pure unit) -- TODO. Controleer hier de binding? Denk aan wederzijdse recursie.
            (tell [IncorrectBinding rolId (tripleObject typeOfTheBinding) toegestaneBinding])

rolIsMandatory :: forall e. RolID -> MonadPerspectivesQuery (AjaxAvarCache e) Boolean
rolIsMandatory = toBoolean rolIsVerplicht

propertyIsMandatory :: forall e. RolID -> MonadPerspectivesQuery (AjaxAvarCache e) Boolean
propertyIsMandatory = toBoolean propertyIsVerplicht

propertyIsFunctional :: forall e. RolID -> MonadPerspectivesQuery (AjaxAvarCache e) Boolean
propertyIsFunctional = toBoolean isFunctionalProperty
