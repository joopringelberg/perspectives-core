module Perspectives.TypeDefChecker (checkContext, checkModel, checkDomeinFile)

where

import Control.Monad.Writer (WriterT, execWriterT, lift, tell)
import Data.Argonaut.Core (fromString)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (elemIndex, foldM, head, length, null)
import Data.DateTime.ISO (ISO)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)
import Data.Number (fromString) as Nmb
import Data.StrMap (keys)
import Data.Traversable (for_, traverse)
import Perspectives.ContextAndRole (context_binnenRol, rol_id, rol_pspType)
import Perspectives.CoreTypes (MP, MonadPerspectivesQuery, Triple(..), UserMessage(..), tripleObject, (@@), (@@=), (@@>), (##>))
import Perspectives.DataTypeObjectGetters (propertyTypen)
import Perspectives.DataTypeTripleGetters (propertyTypen, contextType, typeVanIedereRolInContext, internePropertyTypen) as DTG
import Perspectives.DataTypeTripleGetters (rolBindingDef)
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, PropertyName)
import Perspectives.Identifiers (buitenRol)
import Perspectives.ModelBasedObjectGetters (contextDef)
import Perspectives.ModelBasedTripleGetters (binnenRolBeschrijvingDef, buitenRolBeschrijvingDef, mandatoryProperties, mandatoryRollen, mogelijkeBinding, propertiesDef, propertyIsFunctioneel, propertyIsVerplicht, rangeDef, rolIsVerplicht, rollenDef)
import Perspectives.ObjectGetterConstructors (alternatives, searchContextRol)
import Perspectives.PerspectivesTypes (AnyContext, Context(..), ContextDef(..), ContextRol, PropertyDef(..), RolDef(..), SimpleValueDef)
import Perspectives.QueryCombinators (toBoolean)
import Perspectives.QueryCompiler (getPropertyFunction, getInternalPropertyFunction)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.RunMonadPerspectivesQuery (runMonadPerspectivesQuery, (##=))
import Perspectives.StringTripleGetterConstructors (StringTypedTripleGetter, closureOfAspect)
import Perspectives.Syntax (PerspectRol)
import Perspectives.TripleGetterComposition (followedBy, (>->))
import Perspectives.TripleGetterConstructors (directAspectRoles, directAspects)
import Perspectives.TypeChecker (contextHasType)
import Perspectives.Utilities (ifNothing)
import Prelude (Unit, bind, const, discard, ifM, map, pure, unit, void, ($), (<), (<<<), (>=>), (>>=), (>>>), (||))

type TDChecker e = WriterT (Array UserMessage) (MonadPerspectivesQuery e)

checkDomeinFile :: forall e. DomeinFile -> MonadPerspectivesQuery (AjaxAvarCache e) (Array UserMessage)
checkDomeinFile (DomeinFile{contexts}) = execWriterT $ for_ (keys contexts) (Context >>> checkContext')

checkModel :: forall e. ContextID -> MP e (Array UserMessage)
checkModel modelId = runMonadPerspectivesQuery modelId (lift <<< retrieveDomeinFile >=> checkDomeinFile)

checkContext :: forall e. ContextID -> MP e (Array UserMessage)
checkContext cid = runMonadPerspectivesQuery cid \x -> execWriterT $ checkContext' $ Context x

-- TODO. CONTROLEER RECURSIEF DE AAN ROLLEN GEBONDEN CONTEXTEN.
checkContext' :: forall e. Context -> TDChecker (AjaxAvarCache e) Unit
checkContext' cid = do
  ifNothing (lift (unwrap cid @@> DTG.contextType `followedBy` ContextDef))
    (tell [MissingType $ unwrap cid])
    -- tp is psp:Context
    \tp -> do
      checkContextProperties tp cid
      checkDefinedRoles tp cid
      checkAvailableRoles tp cid
      -- if this psp:ContextInstance represents a psp:Rol, and if it has an instance
      -- of $aspectRol, check whether its namespace giving context has that Aspect.
      -- TODO: move to checkDefinedRoles. Each instance of psp:Rol (that is, each RolDef) is always defined in context.
      b <- (lift $ lift $ (unwrap cid `contextHasType` ContextDef "model:Perspectives$Rol"))
      if b
        then (checkAspectOfRolType $ RolDef $ unwrap cid)
        else (pure unit)
      checkCyclicAspects cid

-- | Check the properties given on the instance of the Context to those defined on its type (the internal and external properties).
-- | That is:
-- |  * find all defined external properties and find all all properties on the instance
-- |  * make sure all given properties have a definition
-- | Do the same for the internal properties.
checkContextProperties :: forall e. ContextDef -> Context -> TDChecker (AjaxAvarCache e) Unit
checkContextProperties contextdef contextInstance = do
  (buitenrol :: PerspectRol) <- lift $ lift $ getPerspectEntiteit $ buitenRol $ unwrap contextInstance
  (binnenrol :: PerspectRol) <- lift $ lift $ getPerspectEntiteit (unwrap contextInstance) >>= pure <<< context_binnenRol

  definedInternalProperties <- lift (unwrap contextdef @@= binnenRolBeschrijvingDef >-> propertiesDef)
  void $ traverse (checkRange contextInstance binnenrol (getBinnenRolPropertyValues contextInstance binnenrol)) definedInternalProperties
  lift (unwrap contextdef @@= binnenRolBeschrijvingDef >-> mandatoryProperties) >>= void <<< traverse (checkPropertyAvailable contextInstance binnenrol (getBinnenRolPropertyValues contextInstance binnenrol))

  definedExternalProperties <- lift (unwrap contextdef @@= buitenRolBeschrijvingDef >-> propertiesDef)
  void $ traverse (checkRange contextInstance buitenrol (getPropertyValues contextInstance buitenrol)) definedExternalProperties
  lift (unwrap contextdef @@= buitenRolBeschrijvingDef >-> mandatoryProperties) >>= void <<< traverse (checkPropertyAvailable contextInstance buitenrol (getPropertyValues contextInstance buitenrol))

  availableExternalProperties <- lift (buitenRol $ unwrap contextInstance @@= DTG.propertyTypen)
  checkAvailableProperties buitenrol availableExternalProperties definedExternalProperties contextInstance

  availableInternalProperties <- lift (unwrap contextInstance @@= DTG.internePropertyTypen)
  checkAvailableProperties binnenrol availableInternalProperties definedInternalProperties contextInstance

-- | Does the rol type hold a definition for all properties given to the rol instance?
checkAvailableProperties :: forall e.
  PerspectRol ->
  Array PropertyName ->
  Array PropertyDef ->
  Context ->
  TDChecker (AjaxAvarCache e) Unit
checkAvailableProperties rolInstance availableProperties definedProperties cid = do
  roltype <- pure $ RolDef $ rol_pspType rolInstance
  for_ availableProperties (isDefined roltype)

    where

      -- | Check if the PropertyDef is a member of the defined properties.
      isDefined :: RolDef -> PropertyName -> TDChecker (AjaxAvarCache e) Unit
      isDefined roltype propertyName =
        case elemIndex (PropertyDef propertyName) definedProperties of
          Nothing -> tell [PropertyNotDefined (unwrap cid) propertyName (rol_id rolInstance) (unwrap roltype)]
          otherwise -> pure unit

-- | For each Rol that is defined for this type of Context, is the instance of that Rol
-- | in accordance to its definition?
checkDefinedRoles :: forall e. ContextDef -> Context -> TDChecker (AjaxAvarCache e) Unit
checkDefinedRoles typeId cid = do
  definedRollen <- lift $ lift (unwrap typeId ##= rollenDef)
  void $ traverse (compareRolInstancesToDefinition cid) definedRollen
  (lift $ lift (unwrap typeId ##= mandatoryRollen)) >>= void <<< traverse (checkRolAvailable cid)
  -- TODO: add the check checkAspectOfRolType

-- | For each RolInstance in the ContextInstance, is there a Rol defined with the Context?
-- | `psp:Context -> psp:ContextInstance -> psp:ElkType`
checkAvailableRoles :: forall e. ContextDef -> Context -> TDChecker (AjaxAvarCache e) Unit
checkAvailableRoles typeId cid = do
  availableRoles <- lift (unwrap cid @@= DTG.typeVanIedereRolInContext)
  for_ availableRoles (isDefined <<< RolDef)
  where

    definedRollen :: TDChecker (AjaxAvarCache e) (Array RolDef)
    definedRollen = lift $ lift (unwrap typeId ##= rollenDef)

    isDefined :: RolDef -> TDChecker (AjaxAvarCache e) Unit
    isDefined rolType = do
      rollen <- definedRollen
      case elemIndex rolType rollen of
        Nothing -> tell [RolNotDefined (unwrap rolType) (unwrap cid) (unwrap typeId)]
        otherwise -> pure unit

-- | Get the property values of a BuitenRol or a RolInContext.
getPropertyValues :: forall e. Context -> PerspectRol -> PropertyDef -> TDChecker (AjaxAvarCache e) (Array String)
getPropertyValues cid rol propertyType = do
  (propertyGetter :: StringTypedTripleGetter e) <- lift $ lift $ getPropertyFunction (unwrap propertyType)
  -- Read the property on the rol instance.
  (Triple {object}) <- lift (rol_id rol @@ propertyGetter) -- Dit gaat fout bij een BinnenRol!
  pure object

-- | Get the property values of a BinnenRol.
getBinnenRolPropertyValues :: forall e. Context -> PerspectRol -> PropertyDef -> TDChecker (AjaxAvarCache e) (Array String)
getBinnenRolPropertyValues cid rol propertyType = do
  (propertyGetter :: StringTypedTripleGetter e) <- lift $ lift $ getInternalPropertyFunction (unwrap propertyType)
  -- Read the property on the context instance.
  (Triple {object}) <- lift (unwrap cid @@ propertyGetter)
  pure object

checkPropertyAvailable :: forall e.
  Context ->
  PerspectRol ->
  (PropertyDef -> TDChecker (AjaxAvarCache e) (Array String)) ->
  PropertyDef ->
  TDChecker (AjaxAvarCache e) Unit
checkPropertyAvailable cid rol getter propertyType = ifM (getter propertyType >>= pure <<< null)
    (tell [MissingPropertyValue (unwrap cid) (unwrap propertyType) (rol_id rol)])
    (pure unit)

-- | Checks the value of the Property with the range that has been defined.
-- | If the Property is functional and more than one value has been given, adds a message.
-- | If the string-valued value of the property cannot be parsed into its declared type,
-- | adds a message.
checkRange :: forall e.
  Context ->
  PerspectRol ->
  (PropertyDef -> TDChecker (AjaxAvarCache e) (Array String)) ->
  PropertyDef ->
  TDChecker (AjaxAvarCache e) Unit
checkRange cid rol getter propertyType = do
  object <- getter propertyType
  case (head object) of
    Nothing ->(pure unit)
    (Just propertyValue) -> do
      mrange <- lift (propertyType @@> rangeDef)
      case mrange of
        Nothing -> pure unit -- There should be a range, however, we protect this function from failing on it.
        (Just (sv :: SimpleValueDef)) -> ifM (tryParseSimpleValue sv propertyValue)
          (pure unit)
          (tell [IncorrectPropertyValue (unwrap cid) (unwrap propertyType) (unwrap sv) propertyValue])
      if length object < 2
        then pure unit
        else ifM (lift $ propertyIsFunctional propertyType)
          (tell [TooManyPropertyValues (unwrap cid) (unwrap propertyType)])
          (pure unit)

  where

    tryParseSimpleValue :: SimpleValueDef -> String -> TDChecker (AjaxAvarCache e) Boolean
    tryParseSimpleValue sv propertyValue = case unwrap sv of
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

-- | For this Rol (definition), check if an instance is available on the context.
-- TODO: controleer of hier iets moet gebeuren voor berekende rollen!
checkRolAvailable :: forall e. Context -> RolDef -> TDChecker (AjaxAvarCache e) Unit
checkRolAvailable contextInstance rolType = ifM (lift $ lift $ searchContextRol rolType (unwrap contextInstance) >>= pure <<< null)
  (tell [MissingRolInstance (unwrap rolType) (unwrap contextInstance)])
  (pure unit)

-- | For this Rol (definition), looks for its instances in the ContextInstance.
-- | Compares that RolInstance to its definition:
-- |  1. Checks each defined property with the instance of the rol.
-- |  2. Checks the type of the binding, if given.
-- | Finally, if the Rol is mandatory and missing, adds a message.
-- TODO: controleer of hier iets moet gebeuren voor berekende rollen!
compareRolInstancesToDefinition :: forall e. Context -> RolDef -> TDChecker (AjaxAvarCache e) Unit
compareRolInstancesToDefinition contextInstance rolType =
  (lift $ lift $ searchContextRol rolType (unwrap contextInstance)) >>= void <<< traverse compareRolInstanceToDefinition

  where

    compareRolInstanceToDefinition :: ContextRol -> TDChecker (AjaxAvarCache e) Unit
    compareRolInstanceToDefinition rolInstance = do
      -- Check the properties.
      (definedRolProperties :: Array PropertyDef) <- lift (rolType @@= propertiesDef)
      (rol :: PerspectRol) <- lift $ lift $ getPerspectEntiteit (unwrap rolInstance)
      void $ traverse (checkRange contextInstance rol (getPropertyValues contextInstance rol)) definedRolProperties
      lift (rolType @@= mandatoryProperties) >>= void <<< traverse (checkPropertyAvailable contextInstance rol (getPropertyValues contextInstance rol))

      -- Detect used but undefined properties.
      (availableProperties :: Array String) <- lift $ lift $ propertyTypen $ unwrap rolInstance
      checkAvailableProperties rol availableProperties definedRolProperties contextInstance

      -- check the binding. Does the binding have the type given by mogelijkeBinding, or has its type that Aspect?
      -- Note that rolInstance is a ContextRol. `rolBindingDef` retrieves the context of the binding.
      maybeBinding <- lift (rolInstance @@> rolBindingDef)
      case maybeBinding of
        Nothing -> pure unit
        (Just (theBinding :: String)) -> do
          mmb <- lift (rolType @@> mogelijkeBinding)
          case mmb of
            Nothing -> pure unit
            (Just (toegestaneBinding :: String)) -> do
              (alts :: Array AnyContext) <- lift $ lift $ alternatives toegestaneBinding
              case head alts of
                -- Single type.
                Nothing -> ifM (lift $ lift $ contextHasType theBinding (ContextDef toegestaneBinding))
                  (pure unit)
                  (do
                    typeOfTheBinding <- lift (theBinding @@ DTG.contextType)
                    (tell [IncorrectBinding (unwrap contextInstance) (unwrap rolInstance) theBinding (tripleObject typeOfTheBinding) toegestaneBinding]))
                -- Sum type.
                otherwise -> ifM (foldM (\r alt -> (lift $ lift $ contextHasType theBinding alt) >>= pure <<< (||) r) false (map ContextDef alts))
                  (pure unit)
                  (do
                    typeOfTheBinding <- lift (theBinding @@ DTG.contextType)
                    (tell [IncorrectBinding (unwrap contextInstance) (unwrap rolInstance) theBinding (tripleObject typeOfTheBinding) toegestaneBinding]))

-- | Checks the aspectRollen of the RolDefinition.
-- | If such an aspectRol is not a Rol of one of the Aspecten of Context definition
-- | that holds the Rol definition, it returns a warning.
checkAspectOfRolType :: forall e. RolDef -> TDChecker (AjaxAvarCache e) Unit
checkAspectOfRolType roldef = do
  ifNothing (lift $ lift (roldef ##> contextDef))
    (tell [RolWithoutContext $ unwrap roldef])
    \(contextdef :: ContextDef) -> do
      rollenVanAspecten <- lift $ lift $ (unwrap contextdef ##= directAspects >-> rollenDef)
      (aspectrollen :: Array RolDef) <- lift $ lift (roldef ##= directAspectRoles)
      void $ traverse
        (\(aspectRol :: RolDef) -> do
          if isJust $ elemIndex aspectRol rollenVanAspecten
            then pure unit
            else tell [AspectRolNotFromAspect (unwrap roldef) (unwrap aspectRol) (unwrap contextdef)])
        aspectrollen

-- | Returns a warning if the Aspecten of the Context definition include the definition itself.
checkCyclicAspects :: forall e. Context -> TDChecker (AjaxAvarCache e) Unit
checkCyclicAspects cid = do
  aspects <- lift $ lift (unwrap cid ##= closureOfAspect)
  case elemIndex (unwrap cid) aspects of
    Nothing -> pure unit
    otherwise -> tell [CycleInAspects (unwrap cid) aspects]

rolIsMandatory :: forall e. RolDef -> MonadPerspectivesQuery (AjaxAvarCache e) Boolean
rolIsMandatory = toBoolean rolIsVerplicht

propertyIsMandatory :: forall e. PropertyDef -> MonadPerspectivesQuery (AjaxAvarCache e) Boolean
propertyIsMandatory = toBoolean propertyIsVerplicht

propertyIsFunctional :: forall e. PropertyDef -> MonadPerspectivesQuery (AjaxAvarCache e) Boolean
propertyIsFunctional = toBoolean propertyIsFunctioneel
