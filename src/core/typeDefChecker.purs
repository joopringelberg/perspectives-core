module Perspectives.TypeDefChecker (checkContext, checkModel)

where

import Control.Monad.Writer (WriterT, execWriterT, lift, tell)
import Data.Argonaut.Core (fromString)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (elemIndex, head, length)
import Data.DateTime.ISO (ISO)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Number (fromString) as Nmb
import Data.StrMap (keys)
import Data.Traversable (for_, traverse)
import Perpectives.TypeChecker (importsAspect, hasType)
import Perspectives.CoreTypes (MP, MonadPerspectivesQuery, Triple(..), TypeID, TypedTripleGetter, UserMessage(..), tripleGetter2function, tripleObject, tripleObjects, (@@))
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, RolID, RolName, PropertyName)
import Perspectives.Identifiers (binnenRol, buitenRol)
import Perspectives.Property (getInternePropertyTypen, getPropertyTypen, getRolUsingAspects)
import Perspectives.TripleGetterComposition ((>->))
import Perspectives.QueryCombinators (toBoolean)
import Perspectives.QueryCompiler (propertyQuery)
import Perspectives.RunMonadPerspectivesQuery ((##), runMonadPerspectivesQuery)
import Perspectives.ModelBasedTripleGetters (aspect, aspectRol, aspecten, contextExternePropertyTypes, contextInternePropertyTypes, contextOwnExternePropertyTypes, contextOwnInternePropertyTypes, contextRolTypes, contextTypeOfRolType, isFunctionalProperty, mogelijkeBinding, propertyIsVerplicht, range, rolIsVerplicht, rolPropertyTypes)
import Perspectives.DataTypeTripleGetters (binding, contextType, rolContext, rolTypen)
import Perspectives.Utilities (ifNothing)
import Prelude (Unit, bind, const, discard, ifM, pure, unit, void, ($), (<<<), (>>=), (<))

type TDChecker e = WriterT (Array UserMessage) (MonadPerspectivesQuery e)

checkModel :: forall e. ContextID -> MP e (Array UserMessage)
checkModel modelId = runMonadPerspectivesQuery modelId checkAllContexts
  where
    checkAllContexts :: ContextID -> MonadPerspectivesQuery (AjaxAvarCache e) (Array UserMessage)
    checkAllContexts x = execWriterT $ do
      (DomeinFile{contexts}) <- lift $ lift $ retrieveDomeinFile modelId
      for_ (keys contexts) checkContext'


checkContext :: forall e. ContextID -> MP e (Array UserMessage)
checkContext cid = runMonadPerspectivesQuery cid \x -> execWriterT $ checkContext' x

-- TODO. CONTROLEER RECURSIEF DE AAN ROLLEN GEBONDEN CONTEXTEN.
-- | `psp:ContextInstance -> psp:ElkType`
checkContext' :: forall e. ContextID -> TDChecker (AjaxAvarCache e) Unit
checkContext' cid = do
  ifNothing (lift $ tripleGetter2function contextType cid)
    (tell [MissingType cid])
    -- tp is psp:Context
    \tp -> do
      checkProperties tp cid
      checkDefinedRoles tp cid
      checkAvailableRoles tp cid
      -- if this psp:ContextInstance represents a psp:Rol, and if it has an instance
      -- of $aspectRol, check whether its namespace giving context has that Aspect.
      ifM (lift $ lift $ (cid `importsAspect` "model:Perspectives$Rol"))
        (checkAspectOfRolType cid)
        (pure unit)
      checkCyclicAspects cid

-- | `psp:Context -> psp:ContextInstance -> psp:ElkType`
checkProperties :: forall e. TypeID -> ContextID -> TDChecker (AjaxAvarCache e) Unit
checkProperties typeId cid = do
  void $ (typeId ~> contextOwnInternePropertyTypes) >>= (traverse (checkInternalProperty cid))

  void $ (typeId ~> contextOwnExternePropertyTypes) >>= (traverse (comparePropertyInstanceToDefinition cid (buitenRol cid)))

  (Triple{object: definedExternalProperties}) <- lift $ lift $ (typeId ## contextExternePropertyTypes)
  availableExternalProperties <- lift $ lift $ getPropertyTypen (buitenRol cid)
  checkAvailableProperties (buitenRol cid) typeId availableExternalProperties definedExternalProperties

  (Triple{object: definedInternalProperties}) <- lift $ lift $ (typeId ## contextInternePropertyTypes)
  availableInternalProperties <- lift $ lift $ getInternePropertyTypen cid
  checkAvailableProperties (binnenRol cid) typeId availableInternalProperties definedExternalProperties

get :: forall e. TypeID -> TypedTripleGetter e -> TDChecker (AjaxAvarCache e) (Array ID)
get typeId tg = lift $ (typeId @@ tg) >>= pure <<< tripleObjects

infix 0 get as ~>

-- | For each Rol that is defined for this type of Context, is the instance of that Rol
-- | in accordance to its definition?
-- | `psp:Context -> psp:ContextInstance -> psp:ElkType`
checkDefinedRoles :: forall e. TypeID -> ContextID -> TDChecker (AjaxAvarCache e) Unit
checkDefinedRoles typeId cid = do
  (Triple{object: definedRollen}) <- lift $ lift $ (typeId ## contextRolTypes)
  void $ traverse (compareRolInstancesToDefinition cid) definedRollen

-- | For each RolInstance in the ContextInstance, is there a Rol defined with the Context?
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
        Nothing -> tell [PropertyNotDefined contextId propertyName rolId contextId]
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
-- | `psp:ContextInstance -> psp:BuitenRolInstance -> psp:Property -> psp:ElkType`
comparePropertyInstanceToDefinition :: forall e. ContextID -> RolID -> TypeID -> TDChecker (AjaxAvarCache e) Unit
comparePropertyInstanceToDefinition cid rid propertyType = do
  propertyGetter <- lift $ propertyQuery propertyType rid
  (Triple {object}) <- lift (rid @@ propertyGetter)
  case head object of
    Nothing -> ifM (lift (propertyIsMandatory propertyType))
      (tell [MissingPropertyValue cid propertyType rid])
      (pure unit)
    (Just propertyValue) -> do
      mrange <- lift (propertyType @@ range)
      case head (tripleObjects mrange) of
        Nothing -> pure unit -- There should be a range, however, we protect this function from failing on it.
        (Just sv) -> ifM (tryParseSimpleValue sv propertyValue)
          (pure unit)
          (tell [IncorrectPropertyValue cid propertyType sv propertyValue])
      if length object < 2
        then pure unit
        else ifM (lift $ propertyIsFunctional propertyType)
          (tell [TooManyPropertyValues cid propertyType])
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

-- | For this Rol (definition), looks for an instance of it in the ContextInstance.
-- | Compares that RolInstance to its definition:
-- |  1. Checks each defined property with the instance of the rol.
-- |  2. Checks the type of the binding, if given.
-- | Finally, ff the Rol is mandatory and missing, adds a message.
-- | `psp:ContextInstance -> psp:Rol -> psp:ElkType`
compareRolInstancesToDefinition :: forall e. ContextID -> TypeID -> TDChecker (AjaxAvarCache e) Unit
compareRolInstancesToDefinition cid rolType' = do
  rolInstances <- lift $ lift $ getRolUsingAspects rolType' cid
  -- (Triple {object:rolInstances}) <- lift (cid @@ rolGetter) -- TODO: kijk ook bij de aspectRollen!
  case head rolInstances of
    Nothing -> ifM (lift (rolIsMandatory rolType'))
      (tell [MissingRolInstance rolType' cid])
      (pure unit)
    otherwise -> void $ traverse compareRolInstancesToDefinition rolInstances
  where
    compareRolInstancesToDefinition :: RolID -> TDChecker (AjaxAvarCache e) Unit
    compareRolInstancesToDefinition rolId = do
      -- Check the properties.
      rolPropertyTypes' <- lift $ (rolType' @@ rolPropertyTypes)
      void $ (traverse (comparePropertyInstanceToDefinition cid rolId)) (tripleObjects rolPropertyTypes')

      -- Detect used but undefined properties.
      (Triple{object: definedRolProperties}) <- lift $ lift $ (rolType' ## rolPropertyTypes)
      availableProperties <- lift $ lift $ getPropertyTypen rolId
      checkAvailableProperties rolId rolType' availableProperties definedRolProperties

      -- check the binding. Does the binding have the type given by mogelijkeBinding, or has its type that Aspect?
      typeOfTheBinding <- lift (rolId @@ (binding >-> rolContext))
      mmb <- lift (rolType' @@ mogelijkeBinding)
      case head (tripleObjects mmb) of
        Nothing -> pure unit
        (Just toegestaneBinding) -> do
          ifM (lift $ lift $ hasType (tripleObject typeOfTheBinding) toegestaneBinding)
            (pure unit)
            (tell [IncorrectBinding cid rolId (tripleObject typeOfTheBinding) toegestaneBinding])

-- Check the aspectRol, if any. Is it bound to a Rol of an Aspect?
-- | The first parameter is bound to a psp:ContextInstance that represents a psp:Rol.
-- | psp:Rol -> psp:Context -> psp:ElkType`
checkAspectOfRolType :: forall e. ContextID -> TDChecker (AjaxAvarCache e) Unit
checkAspectOfRolType cid = do
  (Triple{object:ctypeArr}) <- lift $ lift (cid ## contextTypeOfRolType)
  case head ctypeArr of
    Nothing -> tell [RolWithoutContext cid]
    (Just ctype) -> do
      mar <- lift $ lift (cid ## aspectRol)
      case head (tripleObjects mar) of -- TODO: er kunnen er meer zijn!
        Nothing -> pure unit
        (Just aspectRol) -> do
          (Triple{object:aspectRollen}) <- lift $ lift $ (ctype ## aspect >-> contextRolTypes)
          if isJust $ elemIndex aspectRol aspectRollen
            then pure unit
            else tell [AspectRolNotFromAspect cid aspectRol ctype]

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
