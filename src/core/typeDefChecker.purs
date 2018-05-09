module Perspectives.TypeDefChecker (checkContext, propertyIsFunctional)

where

import Control.Monad.Writer (WriterT, execWriterT, lift, tell)
import Data.Argonaut.Core (fromString)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (elemIndex, head, length)
import Data.DateTime.ISO (ISO)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Number (fromString) as Nmb
import Data.Traversable (for_, traverse)
import Perpectives.TypeChecker (typeIsOrHasAspect)
import Perspectives.CoreTypes (MP, MonadPerspectivesQuery, Triple(..), TypeID, TypedTripleGetter, UserMessage(..), tripleGetter2function, tripleObject, tripleObjects, (@@))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, RolID, RolName, PropertyName)
import Perspectives.Property (getContextTypeF, getInternePropertyTypen, getPropertyTypen)
import Perspectives.PropertyComposition ((>->))
import Perspectives.QueryCombinators (toBoolean)
import Perspectives.QueryCompiler (propertyQuery, rolQuery)
import Perspectives.RunMonadPerspectivesQuery ((##), runMonadPerspectivesQuery)
import Perspectives.SystemQueries (aspect, aspectRol, aspecten, binding, contextExternePropertyTypes, contextInternePropertyTypes, contextOwnExternePropertyTypes, contextOwnInternePropertyTypes, contextRolTypes, contextType, isFunctionalProperty, mogelijkeBinding, propertyIsVerplicht, range, rolContext, rolIsVerplicht, rolPropertyTypes, rolTypen)
import Perspectives.Utilities (ifNothing)
import Prelude (Unit, bind, const, discard, ifM, pure, unit, void, ($), (<<<), (<>), (>>=), (<))

type TDChecker e = WriterT (Array UserMessage) (MonadPerspectivesQuery e)

checkContext :: forall e. ContextID -> MP e (Array UserMessage)
checkContext cid = runMonadPerspectivesQuery cid \x -> execWriterT $ checkContext' x

-- TODO. CONTROLEER RECURSIEF DE AAN ROLLEN GEBONDEN CONTEXTEN.
-- TODO. Controleer of de binding van aspectRol een rol is van een Aspect.
-- | `psp:ContextInstance -> psp:ElkType`
checkContext' :: forall e. ContextID -> TDChecker (AjaxAvarCache e) Unit
checkContext' cid = do
  ifNothing (lift $ tripleGetter2function contextType cid)
    (tell [MissingType cid])
    \tp -> do
      checkProperties tp cid
      checkDefinedRoles tp cid
      checkAvailableRoles tp cid
      checkAspectOfRolType cid
      checkCyclicAspects cid

-- | `psp:Context -> psp:ContextInstance -> psp:ElkType`
checkProperties :: forall e. TypeID -> ContextID -> TDChecker (AjaxAvarCache e) Unit
checkProperties typeId cid = do
  void $ (typeId ~> contextOwnInternePropertyTypes) >>= (traverse (checkInternalProperty cid))

  void $ (typeId ~> contextOwnExternePropertyTypes) >>= (traverse (checkProperty (cid <> "_buitenRol")))

  (Triple{object: definedExternalProperties}) <- lift $ lift $ (typeId ## contextExternePropertyTypes)
  availableProperties <- lift $ lift $ getPropertyTypen (cid <> "_buitenRol")
  checkAvailableProperties (cid <> "_buitenRol") typeId availableProperties definedExternalProperties

  (Triple{object: definedInternalProperties}) <- lift $ lift $ (typeId ## contextInternePropertyTypes)
  availableProperties <- lift $ lift $ getInternePropertyTypen cid
  checkAvailableProperties (cid <> "_binnenRol") typeId availableProperties definedExternalProperties

get :: forall e. TypeID -> TypedTripleGetter e -> TDChecker (AjaxAvarCache e) (Array ID)
get typeId tg = lift $ (typeId @@ tg) >>= pure <<< tripleObjects

infix 0 get as ~>

-- | `psp:Context -> psp:ContextInstance -> psp:ElkType`
checkDefinedRoles :: forall e. TypeID -> ContextID -> TDChecker (AjaxAvarCache e) Unit
checkDefinedRoles typeId cid = do
  (Triple{object: definedRollen}) <- lift $ lift $ (typeId ## contextRolTypes)
  void $ traverse (checkRol cid) definedRollen

-- | Does the type hold a definition for all roles given to the ContextInstantie?
-- | `psp:Context -> psp:ContextInstance -> psp:ElkType`
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

-- | Does the type hold a definition for all properties given to the RolInstantie?
-- | `psp:BinnenRolInstance -> psp:Context -> Array psp:Property -> Array psp:Property -> psp:ElkType`
checkAvailableProperties :: forall e. RolID -> TypeID -> Array PropertyName -> Array PropertyName -> TDChecker (AjaxAvarCache e) Unit
checkAvailableProperties rolId contextId availableProperties definedProperties = do
  for_ availableProperties isDefined
  where
    isDefined :: PropertyName -> TDChecker (AjaxAvarCache e) Unit
    isDefined propertyName =
      case elemIndex propertyName definedProperties of
        Nothing -> tell [PropertyNotDefined propertyName rolId contextId]
        otherwise -> pure unit

-- To check:
--  * if the property is mandatory, is it present?
--  * guess the type of the property value of the given name. Does it have the range as Aspect?
--  * if the property is functional, not more than one value may be present.
-- `psp:ContextInstance -> psp:Property -> psp:ElkType`
checkInternalProperty :: forall e. ContextID -> TypeID -> TDChecker e Unit
checkInternalProperty cid propertyType = pure unit

-- | If the Property is mandatory and missing, adds a message.
-- | Checks the value of the Property with the range that has been defined.
-- | If the Property is functional and more than one value has been given, adds a message.
-- | `psp:BuitenRolInstance -> psp:Property -> psp:ElkType`
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
      "model:Perspectives$Number" -> pure $ maybe false (const true) (Nmb.fromString propertyValue)
      "model:Perspectives$Boolean" -> case propertyValue of
        "true" -> pure true
        "false" -> pure true
        otherwise -> pure false
      "model:Perspectives$Date" -> -- Dates *should* be represented as ISO strings.
        case decodeJson $ fromString propertyValue of
          (Left err :: Either String ISO) -> pure false
          (Right iso :: Either String ISO) -> pure true
      otherwise -> pure true

-- | If the Role is mandatory and missing, adds a message. Checks each defined property with the instance of the rol.
-- | Checks the type of the binding, if given.
-- | `psp:ContextInstance -> psp:Rol -> psp:ElkType`
checkRol :: forall e. ContextID -> TypeID -> TDChecker (AjaxAvarCache e) Unit
checkRol cid rolType' = do
  rolGetter <- lift $ rolQuery rolType' cid
  (Triple {object}) <- lift (cid @@ rolGetter)
  case head object of
    Nothing -> ifM (lift (rolIsMandatory rolType'))
      (tell [MissingRolInstance rolType' cid])
      (pure unit)
    (Just rolId) -> do
      -- Check the properties.
      rolPropertyTypes' <- lift $ (rolType' @@ rolPropertyTypes)
      void $ (traverse (checkProperty rolId)) (tripleObjects rolPropertyTypes')

      -- Detect used but undefined properties.
      (Triple{object: definedRolProperties}) <- lift $ lift $ (rolType' ## rolPropertyTypes)
      availableProperties <- lift $ lift $ getPropertyTypen rolId
      checkAvailableProperties rolId rolType' availableProperties definedRolProperties

      -- check the binding. Does the binding have the type given by mogelijkeBinding, or has its type that Aspect?
      typeOfTheBinding <- lift (rolId @@ (binding >-> rolContext))
      t <- lift $ lift $ getContextTypeF (tripleObject typeOfTheBinding)
      mmb <- lift (rolType' @@ mogelijkeBinding)
      case head (tripleObjects mmb) of
        Nothing -> pure unit
        (Just toegestaneBinding) -> do
          ifM (lift $ lift $ typeIsOrHasAspect t toegestaneBinding)
            (pure unit)
            (tell [IncorrectBinding rolId (tripleObject typeOfTheBinding) toegestaneBinding])

-- Check the aspectRol, if any. Is it bound to a Rol of an Aspect?
-- | psp:Rol -> psp:ElkType`
checkAspectOfRolType :: forall e. ContextID -> TDChecker (AjaxAvarCache e) Unit
checkAspectOfRolType cid = do
  mar <- lift $ lift (cid ## aspectRol)
  case head (tripleObjects mar) of
    Nothing -> pure unit
    (Just aspectRol) -> do
      (Triple{object:aspectRollen}) <- lift $ lift $ (cid ## contextType >-> aspect >-> contextRolTypes)
      if isJust $ elemIndex aspectRol aspectRollen
        then pure unit
        else tell [AspectRolNotFromAspect cid aspectRol]

-- | `psp:ContextInstance -> psp:ElkType`
checkCyclicAspects :: forall e. ContextID -> TDChecker (AjaxAvarCache e) Unit
checkCyclicAspects cid = do
  (Triple {object: aspects}) <- lift $ lift (cid ## aspecten)
  case elemIndex cid aspects of
    Nothing -> pure unit
    otherwise -> tell [CycleInAspects cid aspects]

rolIsMandatory :: forall e. RolID -> MonadPerspectivesQuery (AjaxAvarCache e) Boolean
rolIsMandatory = toBoolean rolIsVerplicht

propertyIsMandatory :: forall e. RolID -> MonadPerspectivesQuery (AjaxAvarCache e) Boolean
propertyIsMandatory = toBoolean propertyIsVerplicht

propertyIsFunctional :: forall e. RolID -> MonadPerspectivesQuery (AjaxAvarCache e) Boolean
propertyIsFunctional = toBoolean isFunctionalProperty
