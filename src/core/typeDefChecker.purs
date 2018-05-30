module Perspectives.TypeDefChecker (checkContext, checkModel, getPropertyFunction)

where

import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
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
import Perpectives.TypeChecker (contextHasType)
import Perspectives.CoreTypes (MP, MonadPerspectivesQuery, Triple(..), TypeID, TypedTripleGetter, UserMessage(..), MonadPerspectives, runMonadPerspectivesQueryCompiler, tripleGetter2function, tripleObject, tripleObjects, (@@))
import Perspectives.DataTypeTripleGetters (bindingM, contextM, contextTypeM, typeVanIedereRolInContextM)
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, RolID, RolName, PropertyName)
import Perspectives.Identifiers (binnenRol, buitenRol)
import Perspectives.ModelBasedTripleGetters (aspectDef, aspectRolDef, aspectDefClosure, externePropertyDef, internePropertyDef, ownExternePropertyDef, ownInternePropertyDef, rolDef, contextDef, propertyIsFunctioneel, bindingDef, propertyIsVerplicht, rangeDef, rolIsVerplicht, propertyDef)
import Perspectives.ObjectGetterConstructors (getRolUsingAspects)
import Perspectives.QueryAST (ElementaryQueryStep(..))
import Perspectives.QueryCombinators (toBoolean)
import Perspectives.QueryCompiler (constructQueryFunction)
import Perspectives.QueryFunctionDescriptionCompiler (compileElementaryQueryStep)
import Perspectives.RunMonadPerspectivesQuery ((##), runMonadPerspectivesQuery)
import Perspectives.DataTypeObjectGetters (internePropertyTypen, propertyTypen, getRolTypeF)
import Perspectives.TripleGetterComposition ((>->))
import Perspectives.Utilities (ifNothing)
import Prelude (Unit, bind, const, discard, ifM, pure, show, unit, void, ($), (<), (<<<), (<>), (>>=))

type TDChecker e = WriterT (Array UserMessage) (MonadPerspectivesQuery e)

checkModel :: forall e. ContextID -> MP e (Array UserMessage)
checkModel modelId = runMonadPerspectivesQuery modelId checkAllContexts
  where
    checkAllContexts :: ContextID -> MonadPerspectivesQuery (AjaxAvarCache e) (Array UserMessage)
    checkAllContexts x = execWriterT $ do
      (DomeinFile{contexts}) <- lift $ lift $ retrieveDomeinFile modelId
      for_ (keys contexts) checkContext'


-- | `psp:ContextInstance -> psp:ElkType`
checkContext :: forall e. ContextID -> MP e (Array UserMessage)
checkContext cid = runMonadPerspectivesQuery cid \x -> execWriterT $ checkContext' x

-- TODO. CONTROLEER RECURSIEF DE AAN ROLLEN GEBONDEN CONTEXTEN.
-- | `psp:ContextInstance -> psp:ElkType`
checkContext' :: forall e. ContextID -> TDChecker (AjaxAvarCache e) Unit
checkContext' cid = do
  ifNothing (lift $ tripleGetter2function contextTypeM cid)
    (tell [MissingType cid])
    -- tp is psp:Context
    \tp -> do
      checkProperties tp cid
      checkDefinedRoles tp cid
      checkAvailableRoles tp cid
      -- if this psp:ContextInstance represents a psp:Rol, and if it has an instance
      -- of $aspectRol, check whether its namespace giving context has that Aspect.
      ifM (lift $ lift $ (cid `contextHasType` "model:Perspectives$Rol"))
        (checkAspectOfRolType cid)
        (pure unit)
      checkCyclicAspects cid

-- | `psp:Context -> psp:ContextInstance -> psp:ElkType`
checkProperties :: forall e. TypeID -> ContextID -> TDChecker (AjaxAvarCache e) Unit
checkProperties typeId cid = do
  void $ (typeId ~> ownInternePropertyDef) >>= (traverse (checkInternalProperty cid))

  void $ (typeId ~> ownExternePropertyDef) >>= (traverse (comparePropertyInstanceToDefinition cid (buitenRol cid)))

  (Triple{object: definedExternalProperties}) <- lift $ lift $ (typeId ## externePropertyDef)
  availableExternalProperties <- lift $ lift $ propertyTypen (buitenRol cid)
  checkAvailableProperties (buitenRol cid) typeId availableExternalProperties definedExternalProperties cid

  (Triple{object: definedInternalProperties}) <- lift $ lift $ (typeId ## internePropertyDef)
  availableInternalProperties <- lift $ lift $ internePropertyTypen cid
  checkAvailableProperties (binnenRol cid) typeId availableInternalProperties definedExternalProperties cid

get :: forall e. TypeID -> TypedTripleGetter e -> TDChecker (AjaxAvarCache e) (Array ID)
get typeId tg = lift $ (typeId @@ tg) >>= pure <<< tripleObjects

infix 0 get as ~>

-- | For each Rol that is defined for this type of Context, is the instance of that Rol
-- | in accordance to its definition?
-- | `psp:Context -> psp:ContextInstance -> psp:ElkType`
checkDefinedRoles :: forall e. TypeID -> ContextID -> TDChecker (AjaxAvarCache e) Unit
checkDefinedRoles typeId cid = do
  (Triple{object: definedRollen}) <- lift $ lift $ (typeId ## rolDef)
  void $ traverse (compareRolInstancesToDefinition cid) definedRollen

-- | For each RolInstance in the ContextInstance, is there a Rol defined with the Context?
-- | `psp:Context -> psp:ContextInstance -> psp:ElkType`
checkAvailableRoles :: forall e. TypeID -> ContextID -> TDChecker (AjaxAvarCache e) Unit
checkAvailableRoles typeId cid = do
  (Triple{object: availableRoles}) <- lift (cid @@ typeVanIedereRolInContextM)
  for_ availableRoles isDefined
  where
    -- `psp:Context -> psp:Rol -> Unit`
    isDefined :: RolName -> TDChecker (AjaxAvarCache e) Unit
    isDefined rolType = do
      (Triple{object: definedRollen}) <- lift $ lift $ (typeId ## rolDef)
      case elemIndex rolType definedRollen of
        Nothing -> tell [RolNotDefined rolType cid typeId]
        otherwise -> pure unit

-- | Does the type hold a definition for all properties given to the RolInstantie?
-- | `psp:BinnenRolInstance -> psp:Context -> Array psp:Property -> Array psp:Property -> psp:ElkType`
checkAvailableProperties :: forall e. RolID -> TypeID -> Array PropertyName -> Array PropertyName -> ContextID -> TDChecker (AjaxAvarCache e) Unit
checkAvailableProperties rolId contextId availableProperties definedProperties cid = do
  for_ availableProperties isDefined
  where
    isDefined :: PropertyName -> TDChecker (AjaxAvarCache e) Unit
    isDefined propertyName =
      case elemIndex propertyName definedProperties of
        Nothing -> tell [PropertyNotDefined cid propertyName rolId contextId]
        otherwise -> pure unit

-- To check:
--  * if the property is mandatory, is it present?
--  * guess the type of the property value of the given name. Does it have the range as Aspect?
--  * if the property is functional, not more than one value may be present.
-- `psp:ContextInstance -> psp:Property -> psp:ElkType`
checkInternalProperty :: forall e. ContextID -> TypeID -> TDChecker e Unit
-- TODO: schrijf deze functie!
checkInternalProperty cid propertyType = pure unit

-- | If the Property is mandatory and missing, adds a message.
-- | Checks the value of the Property with the range that has been defined.
-- | If the Property is functional and more than one value has been given, adds a message.
-- | `psp:ContextInstance -> psp:RolInstance -> psp:Property -> psp:ElkType`
comparePropertyInstanceToDefinition :: forall e. ContextID -> RolID -> TypeID -> TDChecker (AjaxAvarCache e) Unit
comparePropertyInstanceToDefinition cid rid propertyType = do
  rolType <- lift $ lift $ getRolTypeF rid
  (propertyGetter :: TypedTripleGetter e) <- lift $ lift $ getPropertyFunction propertyType rolType
  (Triple {object}) <- lift (rid @@ propertyGetter)
  pure unit
  case head object of
    Nothing -> ifM (lift $ propertyIsMandatory propertyType)
      (tell [MissingPropertyValue cid propertyType rid])
      (pure unit)
    (Just propertyValue) -> do
      mrange <- lift (propertyType @@ rangeDef)
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

-- Returns a getter, lookup function or compiled query.
-- `Property -> Rol -> (RolInstance -> PropertyValue)`
getPropertyFunction :: forall e. PropertyName -> RolName -> MonadPerspectives (AjaxAvarCache e) (TypedTripleGetter e)
getPropertyFunction pn rn = do
  r <- runMonadPerspectivesQueryCompiler rn (compileElementaryQueryStep (QualifiedProperty pn) (pn <> "_getter"))
  case r of
    (Left m) -> throwError $ error $ show m
    (Right descriptionId) -> constructQueryFunction descriptionId

-- | For this Rol (definition), looks for an instance of it in the ContextInstance.
-- | Compares that RolInstance to its definition:
-- |  1. Checks each defined property with the instance of the rol.
-- |  2. Checks the type of the binding, if given.
-- | Finally, ff the Rol is mandatory and missing, adds a message.
-- | `psp:ContextInstance -> psp:Rol -> psp:ElkType`
compareRolInstancesToDefinition :: forall e. ContextID -> TypeID -> TDChecker (AjaxAvarCache e) Unit
compareRolInstancesToDefinition cid rolType' = do
  rolInstances <- lift $ lift $ getRolUsingAspects rolType' cid
  -- (Triple {object:rolInstances}) <- lift (cid @@ rolGetter) -- TODO: kijk ook bij de aspectRolDefClosure!
  case head rolInstances of
    Nothing -> ifM (lift (rolIsMandatory rolType'))
      (tell [MissingRolInstance rolType' cid])
      (pure unit)
    otherwise -> void $ traverse compareRolInstanceToDefinition rolInstances
  where
    -- `psp:RolInstance -> Unit`
    compareRolInstanceToDefinition :: RolID -> TDChecker (AjaxAvarCache e) Unit
    compareRolInstanceToDefinition rolId = do
      -- Check the properties.
      propertyDef' <- lift $ (rolType' @@ propertyDef)
      void $ (traverse (comparePropertyInstanceToDefinition cid rolId)) (tripleObjects propertyDef')

      -- Detect used but undefined properties.
      (Triple{object: definedRolProperties}) <- lift $ lift $ (rolType' ## propertyDef)
      availableProperties <- lift $ lift $ propertyTypen rolId
      checkAvailableProperties rolId rolType' availableProperties definedRolProperties cid

      -- check the binding. Does the binding have the type given by bindingDef, or has its type that Aspect?
      -- Note that we work on type level. So the theBinding is a Context describing a type of Rol.
      theBinding <- lift (rolId @@ bindingM >-> contextM)
      mmb <- lift (rolType' @@ bindingDef)
      case head (tripleObjects mmb) of
        Nothing -> pure unit
        (Just toegestaneBinding) -> do
          ifM (lift $ lift $ contextHasType (tripleObject theBinding) toegestaneBinding)
            (pure unit)
            (do
              typeOfTheBinding <- lift ((tripleObject theBinding) @@ contextTypeM)
              (tell [IncorrectBinding cid rolId (tripleObject theBinding) (tripleObject typeOfTheBinding) toegestaneBinding]))

-- Check the aspectRol, if any. Is it bound to a Rol of an Aspect?
-- | The first parameter is bound to a psp:ContextInstance that represents a psp:Rol.
-- | psp:Rol -> psp:Context -> psp:ElkType`
checkAspectOfRolType :: forall e. ContextID -> TDChecker (AjaxAvarCache e) Unit
checkAspectOfRolType cid = do
  (Triple{object:ctypeArr}) <- lift $ lift (cid ## contextDef)
  case head ctypeArr of
    Nothing -> tell [RolWithoutContext cid]
    (Just ctype) -> do
      mar <- lift $ lift (cid ## aspectRolDef)
      case head (tripleObjects mar) of -- TODO: er kunnen er meer zijn!
        Nothing -> pure unit
        (Just aspectRol) -> do
          (Triple{object:aspectRolDefClosure}) <- lift $ lift $ (ctype ## aspectDef >-> rolDef)
          if isJust $ elemIndex aspectRol aspectRolDefClosure
            then pure unit
            else tell [AspectRolNotFromAspect cid aspectRol ctype]

-- | `psp:ContextInstance -> psp:ElkType`
checkCyclicAspects :: forall e. ContextID -> TDChecker (AjaxAvarCache e) Unit
checkCyclicAspects cid = do
  (Triple {object: aspects}) <- lift $ lift (cid ## aspectDefClosure)
  case elemIndex cid aspects of
    Nothing -> pure unit
    otherwise -> tell [CycleInAspects cid aspects]

rolIsMandatory :: forall e. RolID -> MonadPerspectivesQuery (AjaxAvarCache e) Boolean
rolIsMandatory = toBoolean rolIsVerplicht

propertyIsMandatory :: forall e. RolID -> MonadPerspectivesQuery (AjaxAvarCache e) Boolean
propertyIsMandatory = toBoolean propertyIsVerplicht

propertyIsFunctional :: forall e. RolID -> MonadPerspectivesQuery (AjaxAvarCache e) Boolean
propertyIsFunctional = toBoolean propertyIsFunctioneel
