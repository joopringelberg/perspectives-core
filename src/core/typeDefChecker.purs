module Perspectives.TypeDefChecker (checkAContext, checkModel, checkDomeinFile)

where

import Control.Monad.Writer (WriterT, execWriterT, lift, tell)
import Data.Argonaut.Core (fromString)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (elemIndex, foldM, head, length, null)
import Data.DateTime.ISO (ISO)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Number (fromString) as Nmb
import Data.StrMap (keys)
import Data.Traversable (for_, traverse_)
import Perspectives.ContextAndRole (context_binnenRol, rol_id, rol_pspType)
import Perspectives.CoreTypes (MP, MonadPerspectivesQuery, Triple(..), UserMessage(..), tripleObject, (@@), (@@=), (@@>), type (**>))
import Perspectives.DataTypeObjectGetters (propertyTypen)
import Perspectives.DataTypeTripleGetters (getUnqualifiedProperty, rolBindingDef, buitenRol) as DTTG
import Perspectives.DataTypeTripleGetters (propertyTypen, contextType, typeVanIedereRolInContext, internePropertyTypen, buitenRol, binnenRol) as DTG
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, PropertyName)
import Perspectives.Identifiers (LocalName, buitenRol)
import Perspectives.ModelBasedTripleGetters (binnenRolBeschrijvingDef, buitenRolBeschrijvingDef, contextDef, mandatoryProperties, mandatoryRollen, mogelijkeBinding, nonQueryRollen, ownMogelijkeBinding, ownRangeDef, propertiesDef, propertyIsFunctioneel, rangeDef, rollenDef)
import Perspectives.ObjectGetterConstructors (alternatives, searchContextRol)
import Perspectives.PerspectivesTypes (AnyContext, Context(..), ContextDef(..), ContextRol, PBool(..), PropertyDef(..), RolDef(..), SimpleValueDef(..), Value(..))
import Perspectives.QueryCombinators (toBoolean)
import Perspectives.QueryCompiler (getPropertyFunction, getInternalPropertyFunction)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.RunMonadPerspectivesQuery (runMonadPerspectivesQuery, (##=), (##>))
import Perspectives.StringTripleGetterConstructors (StringTypedTripleGetter, closure, closureOfAspect, searchInRolTelescope, some)
import Perspectives.Syntax (PerspectRol)
import Perspectives.TripleGetterComposition (before, followedBy, (>->))
import Perspectives.TripleGetterConstructors (closureOfAspectProperty, closureOfAspectRol, directAspectProperties, directAspectRoles, getInternalProperty, searchExternalUnqualifiedProperty, searchInAspectRolesAndPrototypes, searchProperty)
import Perspectives.TypeChecker (contextHasType, isOrHasAspect)
import Perspectives.Utilities (ifNothing)
import Prelude (Unit, bind, const, discard, ifM, map, pure, unit, ($), (<), (<<<), (>=>), (>>=), (>>>), (||), (*>))

type TDChecker e = WriterT (Array UserMessage) (MonadPerspectivesQuery (AjaxAvarCache e))

checkDomeinFile :: forall e. DomeinFile -> MonadPerspectivesQuery (AjaxAvarCache e) (Array UserMessage)
checkDomeinFile (DomeinFile{contexts}) = execWriterT $ for_ (keys contexts) (ContextDef >>> checkDefinition)

checkModel :: forall e. ContextID -> MP e (Array UserMessage)
checkModel modelId = runMonadPerspectivesQuery modelId (lift <<< retrieveDomeinFile >=> checkDomeinFile)

checkAContext :: forall e. Context -> MP e (Array UserMessage)
checkAContext cid = runMonadPerspectivesQuery cid \x -> execWriterT $ checkContext' x
  where
    checkContext' :: Context -> TDChecker e Unit
    checkContext' def = ifNothing (lift (unwrap def @@> DTG.contextType `followedBy` ContextDef))
      (tell [MissingType $ unwrap def])
      -- \(tp :: ContextDef) -> checkContext def tp
      \(tp :: ContextDef) -> checksForEachContext def tp *> mandatoryRolesInstantiated def tp

-----------------------------------------------------------
-- DEFINITION
-----------------------------------------------------------
-- | Dispatches to specific checks for Context-, Rol- and PropertyDef, and all others.
checkDefinition :: forall e. ContextDef -> TDChecker e Unit
checkDefinition def = ifNothing (lift (unwrap def @@> DTG.contextType `followedBy` ContextDef))
  (tell [MissingType $ unwrap def])
  \(tp :: ContextDef) -> do
    ifM (tp `isa` (ContextDef "model:Perspectives$Context"))
      (checksForEachContext (Context (unwrap def)) tp *> checkContextDef def tp)
      (ifM (tp `isa` (ContextDef "model:Perspectives$Rol"))
        (checksForEachContext (Context (unwrap def)) tp *> checkRolDef (RolDef (unwrap def)) tp)
        (ifM (tp `isa` (ContextDef "model:Perspectives$Property"))
          (checksForEachContext (Context (unwrap def)) tp *> checkPropertyDef (PropertyDef (unwrap def)) tp)
          (checksForEachContext (Context (unwrap def)) tp) *> mandatoryRolesInstantiated (Context $ unwrap def) tp)
          )
  where
    isa :: ContextDef -> ContextDef -> TDChecker e Boolean
    isa a b = lift $ lift $ isOrHasAspect a b

-----------------------------------------------------------
-- CHECKCONTEXTDEF
-----------------------------------------------------------
-- | Is the Aspect relation not cyclic?
-- | Have all mandatory external properties a value?
-- | Have all mandatory internal properties a value?
-- | Have all mandatory roles been instantiated?
checkContextDef :: forall e. ContextDef -> ContextDef -> TDChecker e Unit
checkContextDef def deftype = do
  (buitenrol :: PerspectRol) <- lift $ lift $ getPerspectEntiteit $ buitenRol $ unwrap def
  (binnenrol :: PerspectRol) <- lift $ lift $ getPerspectEntiteit (unwrap def) >>= pure <<< context_binnenRol

  mandatoryInternalProperties <- lift (unwrap deftype @@= binnenRolBeschrijvingDef >-> mandatoryProperties)
  traverse_
    (\(propertyType :: PropertyDef) -> ifM ((getBinnenRolPropertyValues (Context (unwrap def)) binnenrol propertyType) >>= pure <<< null)
      (tell [MissingInternalPropertyValue (unwrap propertyType) (unwrap def)])
      (pure unit))
    mandatoryInternalProperties

  mandatoryExternalProperties <- (lift (unwrap deftype @@= buitenRolBeschrijvingDef >-> mandatoryProperties))
  traverse_
    (\(propertyType :: PropertyDef) -> ifM ((getPropertyValues (Context (unwrap def)) buitenrol propertyType) >>= pure <<< null)
      (tell [MissingExternalPropertyValue (unwrap propertyType) (unwrap def)])
      (pure unit))
    mandatoryExternalProperties

  checkCyclicAspects def

  mandatoryRolesInstantiated (Context $ unwrap def) deftype

-- | Get the property values of a BuitenRol or a RolInContext.
getPropertyValues :: forall e. Context -> PerspectRol -> PropertyDef -> TDChecker e (Array String)
getPropertyValues cid rol propertyType = do
  (propertyGetter :: StringTypedTripleGetter e) <- lift $ lift $ getPropertyFunction (unwrap propertyType)
  -- Read the property on the rol instance.
  (Triple {object}) <- lift (rol_id rol @@ propertyGetter) -- Dit gaat fout bij een BinnenRol!
  pure object

-- | Get the property values of a BinnenRol.
getBinnenRolPropertyValues :: forall e. Context -> PerspectRol -> PropertyDef -> TDChecker e (Array String)
getBinnenRolPropertyValues def rol propertyType = do
  (propertyGetter :: StringTypedTripleGetter e) <- lift $ lift $ getInternalPropertyFunction (unwrap propertyType)
  -- Read the property on the context instance.
  (Triple {object}) <- lift (unwrap def @@ propertyGetter)
  pure object

-----------------------------------------------------------
-- CHECKROLDEF
-----------------------------------------------------------
-- | Is each AspectRol a role of an Aspect of the defining ContextDef of this RolDef?
-- | Does the mogelijkeBinding have the mogelijkeBinding of each of its AspectRollen as an Aspect?
-- | Does the RolDef provide all mandatory internal and external properties with a value?
-- | Is there no cycle in AspectRol?
checkRolDef :: forall e. RolDef -> ContextDef -> TDChecker e Unit
checkRolDef def deftype = do
  checkAspectOfRolType def
  checkMogelijkeBinding def
  checkRolDefPropertyValues def deftype
  checkCyclicAspectRoles def

-- | Checks the aspectRollen of the RolDefinition.
-- | If such an aspectRol is not a Rol of one of the Aspecten of Context definition
-- | that holds the Rol definition, it returns a warning.
checkAspectOfRolType :: forall e. RolDef -> TDChecker e Unit
checkAspectOfRolType def = do
  ifNothing (lift $ lift (def ##> contextDef))
    (tell [RolWithoutContext $ unwrap def])
    \(contextdef :: ContextDef) -> do
      rollenVanAspecten <- lift $ lift $ (unwrap contextdef ##= closureOfAspect >-> rollenDef)
      (aspectrollen :: Array RolDef) <- lift $ lift (def ##= directAspectRoles)
      traverse_
        (\(aspectRol :: RolDef) -> do
          if isJust $ elemIndex aspectRol rollenVanAspecten
            then pure unit
            else tell [AspectRolNotFromAspect (unwrap def) (unwrap aspectRol) (unwrap contextdef)])
        aspectrollen

checkMogelijkeBinding :: forall e. RolDef -> TDChecker e Unit
checkMogelijkeBinding def = do
  mmbinding <- lift (def @@> mogelijkeBinding)
  case mmbinding of
    Nothing -> tell [MissingMogelijkeBinding $ unwrap def]
    (Just mbinding) -> do
      mlocalMbinding <- lift (def @@> ownMogelijkeBinding)
      case mlocalMbinding of
        Nothing -> pure unit
        (Just localMbinding) -> lift (def @@= closureOfAspectRol) >>= traverse_
          \aspectRol -> ifNothing (lift (aspectRol @@> ownMogelijkeBinding))
            (pure unit)
            (\aspectMbinding -> ifM (lift $ lift $ (ContextDef localMbinding) `isOrHasAspect` (ContextDef aspectMbinding))
              (pure unit)
              (tell [MogelijkeBindingNotSubsumed mbinding (unwrap aspectRol) aspectMbinding (unwrap def)]))

-- | Does the RolDef assign a value to the mandatory internal and external properties of the type of the
-- | RolDef? These values can be assigned to the RolDef itself (on its binnen- or buitenrol), or
-- | on its prototypes or AspectRollen.
checkRolDefPropertyValues :: forall e. RolDef -> ContextDef -> TDChecker e Unit
checkRolDefPropertyValues def deftype = do
  mandatoryExternalProperties <- lift (unwrap deftype @@= buitenRolBeschrijvingDef >-> mandatoryProperties)
  traverse_ checkExternalProperty mandatoryExternalProperties

  mandatoryInternalProperties <- lift (unwrap deftype @@= binnenRolBeschrijvingDef >-> mandatoryProperties)
  traverse_ checkExternalProperty mandatoryExternalProperties

  where
    findExternalValue :: PropertyDef -> (AnyContext **> Value) e
    findExternalValue pdef = searchInAspectRolesAndPrototypes (DTG.buitenRol >-> searchProperty pdef)

    checkExternalProperty :: PropertyDef -> TDChecker e Unit
    checkExternalProperty pdef = ifNothing (lift $ lift (unwrap def ##> findExternalValue pdef))
      (tell [MissingExternalPropertyValue (unwrap pdef) (unwrap def)])
      (\ignore -> pure unit)

    findInternalValue :: PropertyDef -> (AnyContext **> String) e
    findInternalValue pdef = searchInAspectRolesAndPrototypes (DTG.binnenRol >-> unwrap `before` (searchInRolTelescope ((getInternalProperty pdef) `followedBy` unwrap)))

    checkInternalProperty :: PropertyDef -> TDChecker e Unit
    checkInternalProperty pdef = ifNothing (lift $ lift (unwrap def ##> findInternalValue pdef))
      (tell [MissingInternalPropertyValue (unwrap pdef) (unwrap def)])
      (\ignore -> pure unit)

-- | Returns a warning if the AspectRollen of the Rol definition include the definition itself.
checkCyclicAspectRoles :: forall e. RolDef -> TDChecker e Unit
checkCyclicAspectRoles cid = do
  aspects <- lift $ lift (cid ##= closureOfAspectRol)
  case elemIndex cid aspects of
    Nothing -> pure unit
    otherwise -> tell [CycleInAspectRoles (unwrap cid) (map unwrap aspects)]


-----------------------------------------------------------
-- CHECKPROPERTYDEF
-----------------------------------------------------------
-- | The properties isFunctioneel and isVerplicht have default values (false),
-- | so we need not check whether there is a value.
-- | But when these properties have a local value equal to false, none of their
-- | AspectProperties may assign true to them!
-- | Is the range of the Property given with the PropertyDef, or one of its AspectProperties?
-- | Is the range of the Property subsumed by the ranges of its AspectProperties?
-- | Is there no cycle in AspectProperty?
checkPropertyDef :: forall e. PropertyDef -> ContextDef -> TDChecker e Unit
checkPropertyDef def deftype = do
  checkBooleanFacetOfProperty "isVerplicht"
  checkBooleanFacetOfProperty "isFunctioneel"
  checkRangeDef
  checkCyclicAspectProperties def

  where
    checkBooleanFacetOfProperty :: LocalName ->  TDChecker e Unit
    checkBooleanFacetOfProperty ln = do
      localvalueForIsVerplicht <- lift (unwrap def @@> (DTTG.buitenRol >-> (DTTG.getUnqualifiedProperty) ln))
      case localvalueForIsVerplicht of
        Just (Value "false") -> do
          b <- lift (def @@> aspectPropertiesSpecifyVerplicht)
          case b of
            Just (PBool "true") -> tell [CannotOverrideBooleanAspectProperty (unwrap def) ln]
            otherwise -> pure unit
        otherwise -> pure unit
      where
        aspectPropertiesSpecifyVerplicht :: (PropertyDef **> PBool) e
        aspectPropertiesSpecifyVerplicht = some ((closure directAspectProperties) >-> isVerplicht)
          where
            isVerplicht :: (PropertyDef **> PBool) e
            isVerplicht = (unwrap `before` (searchExternalUnqualifiedProperty ln)) `followedBy` (wrap <<< unwrap)
    -- Checks if a range has been defined, somewhere.
    -- If so, checks if that range is subsumed by the range of each AspectProperty.
    checkRangeDef :: TDChecker e Unit
    checkRangeDef = do
      mrange <- lift (def @@> rangeDef)
      case mrange of
        Nothing -> tell [MissingRange $ unwrap def]
        (Just range) -> do
          mlocalRange <- lift (def @@> ownRangeDef)
          case mlocalRange of
            Nothing -> pure unit
            (Just (SimpleValueDef localRangeDef)) -> lift (def @@= closureOfAspectProperty) >>= traverse_
              \aspectProp -> ifNothing (lift (aspectProp @@> ownRangeDef))
                (pure unit)
                (\(SimpleValueDef aspectRange) -> ifM (lift $ lift $ (ContextDef localRangeDef) `isOrHasAspect` (ContextDef aspectRange))
                  (pure unit)
                  (tell [RangeNotSubsumed localRangeDef (unwrap aspectProp) aspectRange (unwrap def)]))

-- | Returns a warning if the AspectProperties of the Rol definition include the definition itself.
checkCyclicAspectProperties :: forall e. PropertyDef -> TDChecker e Unit
checkCyclicAspectProperties cid = do
  aspects <- lift $ lift (cid ##= closureOfAspectProperty)
  case elemIndex cid aspects of
    Nothing -> pure unit
    otherwise -> tell [CycleInAspectProperties (unwrap cid) (map unwrap aspects)]

-----------------------------------------------------------
-- CHECKCONTEXT
-----------------------------------------------------------
-- | Is there an instance in Context for each Rol that is defined for ContextDef?
mandatoryRolesInstantiated :: forall e. Context -> ContextDef -> TDChecker e Unit
mandatoryRolesInstantiated def deftype =
  (lift $ lift (unwrap deftype ##= mandatoryRollen)) >>= traverse_ (checkRolAvailable def)

-----------------------------------------------------------
-- CHECKSFOREACHCONTEXT
-----------------------------------------------------------
-- | Is there a definition with the ContextDef for each role of Context?
-- | Is there a definition on the buitenRolBeschrijving of ContextDef for each external property that
-- | has been given a value?
-- | Have all external properties been assigned the right type of value?
-- | Is the number of values assigned to external properties in accordance with the cardinality of their
-- | definitions?
-- | The same checks for internal properties.

checksForEachContext :: forall e. Context -> ContextDef -> TDChecker e Unit
checksForEachContext def deftype = do
  (buitenrol :: PerspectRol) <- lift $ lift $ getPerspectEntiteit $ buitenRol $ unwrap def
  (binnenrol :: PerspectRol) <- lift $ lift $ getPerspectEntiteit (unwrap def) >>= pure <<< context_binnenRol

  definedInternalProperties <- lift (unwrap deftype @@= binnenRolBeschrijvingDef >-> propertiesDef)
  traverse_ (checkRange def binnenrol (getBinnenRolPropertyValues def binnenrol)) definedInternalProperties

  definedExternalProperties <- lift (unwrap deftype @@= buitenRolBeschrijvingDef >-> propertiesDef)
  traverse_ (checkRange def buitenrol (getPropertyValues def buitenrol)) definedExternalProperties

  availableExternalProperties <- lift (buitenRol $ unwrap def @@= DTG.propertyTypen)
  checkPropertyIsDefined buitenrol availableExternalProperties definedExternalProperties def

  availableInternalProperties <- lift (unwrap def @@= DTG.internePropertyTypen)
  checkPropertyIsDefined binnenrol availableInternalProperties definedInternalProperties def

  checkIfRolesHaveDefinition deftype def

  (lift $ lift (unwrap deftype ##= nonQueryRollen)) >>= traverse_ (compareRolInstancesToDefinition def)

-- | Does the rol type hold a definition for all properties given to the rol instance?
checkPropertyIsDefined :: forall e.
  PerspectRol ->
  Array PropertyName ->
  Array PropertyDef ->
  Context ->
  TDChecker e Unit
checkPropertyIsDefined rolInstance availableProperties definedProperties cid = do
  roltype <- pure $ RolDef $ rol_pspType rolInstance
  for_ availableProperties (isDefined roltype)

    where

      -- | Check if the PropertyDef is a member of the defined properties.
      isDefined :: RolDef -> PropertyName -> TDChecker e Unit
      isDefined roltype propertyName =
        case elemIndex (PropertyDef propertyName) definedProperties of
          Nothing -> tell [PropertyNotDefined (unwrap cid) propertyName (rol_id rolInstance) (unwrap roltype)]
          otherwise -> pure unit
-- | Checks the value of the Property with the range that has been defined.
-- | If the Property is functional and more than one value has been given, adds a message.
-- | If the string-valued value of the property cannot be parsed into its declared type,
-- | adds a message.
checkRange :: forall e.
  Context ->
  PerspectRol ->
  (PropertyDef -> TDChecker e (Array String)) ->
  PropertyDef ->
  TDChecker e Unit
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

    tryParseSimpleValue :: SimpleValueDef -> String -> TDChecker e Boolean
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

propertyIsFunctional :: forall e. PropertyDef -> MonadPerspectivesQuery (AjaxAvarCache e) Boolean
propertyIsFunctional = toBoolean propertyIsFunctioneel

-- | For each RolInstance in the ContextInstance, is there a Rol defined with the Context?
-- | `psp:Context -> psp:ContextInstance -> psp:ElkType`
checkIfRolesHaveDefinition :: forall e. ContextDef -> Context -> TDChecker e Unit
checkIfRolesHaveDefinition deftype def = do
  availableRoles <- lift (unwrap def @@= DTG.typeVanIedereRolInContext)
  for_ availableRoles (isDefined <<< RolDef)
  where

    definedRollen :: TDChecker e (Array RolDef)
    definedRollen = lift $ lift (unwrap deftype ##= rollenDef)

    isDefined :: RolDef -> TDChecker e Unit
    isDefined rolType = do
      rollen <- definedRollen
      case elemIndex rolType rollen of
        Nothing -> tell [RolNotDefined (unwrap rolType) (unwrap def) (unwrap deftype)]
        otherwise -> pure unit

-- | Returns a warning if the Aspecten of the Context definition include the definition itself.
checkCyclicAspects :: forall e. ContextDef -> TDChecker e Unit
checkCyclicAspects cid = do
  aspects <- lift $ lift (unwrap cid ##= closureOfAspect)
  case elemIndex (unwrap cid) aspects of
    Nothing -> pure unit
    otherwise -> tell [CycleInAspects (unwrap cid) aspects]

-- | For this Rol (definition), check if an instance is available on the context.
-- | As RolDef is mandatory, it cannot be defined as a Query (it must be
-- | represented locally or on a prototype of the context).
-- | A Query is defined with psp:Function, which is a psp:Context and not a
-- | psp:Rol. Hence it does not have $isVerplicht as a Property.
checkRolAvailable :: forall e. Context -> RolDef -> TDChecker e Unit
checkRolAvailable def rolType = ifM (lift $ lift $ searchContextRol rolType (unwrap def) >>= pure <<< null)
  (tell [MissingRolInstance (unwrap rolType) (unwrap def)])
  (pure unit)

-- | For this Rol (definition), looks for its instances in the ContextInstance.
-- | Compares that RolInstance to its definition:
-- |  1. Checks each defined property with the instance of the rol.
-- |  2. Checks the type of the binding, if given.
-- | Note that RolDef will never be a query, i.e. it does not have psp:Function
-- | as Aspect.
compareRolInstancesToDefinition :: forall e. Context -> RolDef -> TDChecker e Unit
compareRolInstancesToDefinition def rolType =
  (lift $ lift $ searchContextRol rolType (unwrap def)) >>= traverse_ compareRolInstanceToDefinition

  where

    compareRolInstanceToDefinition :: ContextRol -> TDChecker e Unit
    compareRolInstanceToDefinition rolInstance = do
      -- Check the properties.
      (definedRolProperties :: Array PropertyDef) <- lift (rolType @@= propertiesDef)
      (rol :: PerspectRol) <- lift $ lift $ getPerspectEntiteit (unwrap rolInstance)
      traverse_ (checkRange def rol (getPropertyValues def rol)) definedRolProperties
      lift (rolType @@= mandatoryProperties) >>= traverse_ (checkPropertyAvailable (ContextDef $ unwrap def) rol (getPropertyValues def rol))

      -- Detect used but undefined properties.
      (availableProperties :: Array String) <- lift $ lift $ propertyTypen $ unwrap rolInstance
      checkPropertyIsDefined rol availableProperties definedRolProperties def

      -- check the binding. Does the binding have the type given by mogelijkeBinding, or has its type that Aspect?
      -- Note that rolInstance is a ContextRol. `rolBindingDef` retrieves the context of the binding.
      maybeBinding <- lift (rolInstance @@> DTTG.rolBindingDef)
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
                    (tell [IncorrectBinding (unwrap def) (unwrap rolInstance) theBinding (tripleObject typeOfTheBinding) toegestaneBinding]))
                -- Sum type.
                otherwise -> ifM (foldM (\r alt -> (lift $ lift $ contextHasType theBinding alt) >>= pure <<< (||) r) false (map ContextDef alts))
                  (pure unit)
                  (do
                    typeOfTheBinding <- lift (theBinding @@ DTG.contextType)
                    (tell [IncorrectBinding (unwrap def) (unwrap rolInstance) theBinding (tripleObject typeOfTheBinding) toegestaneBinding]))

    checkPropertyAvailable ::
      ContextDef ->
      PerspectRol ->
      (PropertyDef -> TDChecker e (Array String)) ->
      PropertyDef ->
      TDChecker e Unit
    checkPropertyAvailable cid rol getter propertyType = ifM (getter propertyType >>= pure <<< null)
        (tell [MissingPropertyValue (unwrap cid) (unwrap propertyType) (rol_id rol)])
        (pure unit)
