module Perspectives.TypeDefChecker (checkAContext, checkModel, checkDomeinFile)

where

import Control.Monad.Writer (WriterT, execWriterT, lift, tell)
import Data.Argonaut.Core (fromString)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (elemIndex, foldMap, head, length, null)
import Data.DateTime.ISO (ISO)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (ala, unwrap, wrap)
import Data.Number (fromString) as Nmb
import Data.Traversable (for_, traverse, traverse_)
import Foreign.Object (keys)
import Perspectives.ContextAndRole (rol_id, rol_pspType)
import Perspectives.CoreTypes (type (**>), MP, MonadPerspectivesQuery, Triple(..), UserMessage(..), (@@), (@@=), (@@>), (@@>>))
import Perspectives.DataTypeObjectGetters (isBuitenRol, propertyTypen)
import Perspectives.DataTypeTripleGetters (getUnqualifiedProperty, rolBindingDef, propertyTypen, contextType, typeVanIedereRolInContext, buitenRol, binnenRol, rolType, binding) as DTG
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.EntiteitAndRDFAliases (ContextID, PropertyName)
import Perspectives.Identifiers (LocalName, buitenRol, binnenRol)
import Perspectives.ModelBasedStringTripleGetters (hasContextTypeOnEachRolTelescopeOf, hasRolTypeOnEachRolTelescopeOf, isSubsumedOnEachRolTelescopeOf)
import Perspectives.ModelBasedTripleGetters (bindingProperty, binnenRolBeschrijvingDef, buitenRolBeschrijvingDef, contextDef, enclosingDefinition, expressionType, hasContextType, mandatoryProperties, mandatoryRollen, mogelijkeBinding, nonQueryRollen, ownMogelijkeBinding, ownRangeDef, propertiesDef, propertyIsFunctioneel, rangeDef, rolDef, rollenDef, sumToSequence)
import Perspectives.ObjectGetterConstructors (searchContextRol)
import Perspectives.PerspectivesTypes (AnyContext, BuitenRol, Context(..), ContextDef(..), ContextRol, PBool(..), PropertyDef(..), RolDef(..), RolInContext(..), SimpleValueDef(..), Value(..))
import Perspectives.QueryCombinators (containedIn, toBoolean)
import Perspectives.QueryCompiler (getPropertyFunction, getInternalPropertyFunction)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.RunMonadPerspectivesQuery (runMonadPerspectivesQuery, (##=), (##>))
import Perspectives.StringTripleGetterConstructors (StringTypedTripleGetter, closure, closureOfAspect, searchInRolTelescope, some) as STGC
import Perspectives.StringTripleGetterConstructors (getPrototype)
import Perspectives.Syntax (PerspectRol)
import Perspectives.TripleGetterComposition (before, followedBy, (>->))
import Perspectives.TripleGetterConstructors (closureOfAspectProperty, closureOfAspectRol, directAspectProperties, directAspectRoles, getInternalProperty, searchExternalUnqualifiedProperty, searchInAspectRolesAndPrototypes, searchProperty)
import Perspectives.TypeChecker (isOrHasAspect) as TC
import Perspectives.Utilities (ifNothing)
import Prelude (Unit, bind, const, discard, identity, ifM, map, pure, show, unit, ($), (*>), (<), (<<<), (==), (>=>), (>>=), (>>>))

type TDChecker = WriterT (Array UserMessage) MonadPerspectivesQuery

runTDChecker :: forall a. TDChecker a -> MonadPerspectivesQuery (Array UserMessage)
runTDChecker = execWriterT

checkDomeinFile :: DomeinFile -> MonadPerspectivesQuery (Array UserMessage)
checkDomeinFile (DomeinFile{contexts}) = runTDChecker $ for_ (keys contexts) (ContextDef >>> checkDefinition)

checkModel :: ContextID -> MP (Array UserMessage)
checkModel modelId = runMonadPerspectivesQuery modelId (lift <<< retrieveDomeinFile >=> checkDomeinFile)

checkAContext :: Context -> MP (Array UserMessage)
checkAContext cid = runMonadPerspectivesQuery cid \x -> execWriterT $ checkContext' x
  where
    checkContext' :: Context -> TDChecker Unit
    checkContext' def = ifNothing (lift (unwrap def @@> DTG.contextType `followedBy` ContextDef))
      (tell [NoType $ unwrap def])
      -- \(tp :: ContextDef) -> checkContext def tp
      \(tp :: ContextDef) -> checksForEachContext def tp *> mandatoryRolesInstantiated def tp

-----------------------------------------------------------
-- DEFINITION
-----------------------------------------------------------
-- | Dispatches to specific checks for Context-, Rol- and PropertyDef, and all others,
-- | based on the **type** of the definition.
checkDefinition :: ContextDef -> TDChecker Unit
checkDefinition def = ifNothing (lift (unwrap def @@> DTG.contextType `followedBy` ContextDef))
  (tell [NoType $ unwrap def])
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
    isa :: ContextDef -> ContextDef -> TDChecker Boolean
    isa a b = lift $ lift $ TC.isOrHasAspect a b

-----------------------------------------------------------
-- CHECKCONTEXTDEF
-----------------------------------------------------------
-- | Is the Aspect relation not cyclic?
-- | Have all mandatory external properties a value?
-- | Have all mandatory internal properties a value?
-- | Have all mandatory roles been instantiated?
checkContextDef :: ContextDef -> ContextDef -> TDChecker Unit
checkContextDef def deftype = do
  ifM (checkCyclicAspects def)
    (pure unit)
    do
      (buitenrol :: PerspectRol) <- lift $ lift $ getPerspectEntiteit $ buitenRol $ unwrap def
      (binnenrol :: PerspectRol) <- lift $ lift $ getPerspectEntiteit $ binnenRol $ unwrap def

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

      mandatoryRolesInstantiated (Context $ unwrap def) deftype

-- | Get the property values of a BuitenRol or a RolInContext.
getPropertyValues :: Context -> PerspectRol -> PropertyDef -> TDChecker (Array String)
getPropertyValues cid rol propertyType = do
  (propertyGetter :: STGC.StringTypedTripleGetter) <- lift $ lift $ getPropertyFunction (unwrap propertyType)
  -- Read the property on the rol instance.
  (Triple {object}) <- lift (rol_id rol @@ propertyGetter) -- Dit gaat fout bij een BinnenRol!
  pure object

-- | Get the property values of a BinnenRol.
getBinnenRolPropertyValues :: Context -> PerspectRol -> PropertyDef -> TDChecker (Array String)
getBinnenRolPropertyValues def rol propertyType = do
  (propertyGetter :: STGC.StringTypedTripleGetter) <- lift $ lift $ getInternalPropertyFunction (unwrap propertyType)
  -- Read the property on the context instance.
  (Triple {object}) <- lift (unwrap def @@ propertyGetter)
  pure object

-----------------------------------------------------------
-- CHECKROLDEF
-----------------------------------------------------------
-- | Is each AspectRol a role of an Aspect of the defining ContextDef of this RolDef?
-- | Does the mogelijkeBinding have the mogelijkeBinding of each of its AspectRollen as an Aspect?
-- | Does the RolDef provide all mandatory internal and external properties with a value?
-- | Are the values given to the properties isFunctioneel and isVerplicht in accordance to values
-- | given to the Aspects? (see checkPropertyDef for an explanation)
-- | Is there no cycle in AspectRol? If there is such a cycle, the other
-- | tests will not be carried out.
checkRolDef :: RolDef -> ContextDef -> TDChecker Unit
checkRolDef def deftype = do
  ifM (checkCyclicAspectRoles def)
    (pure unit)
    do
      checkAspectOfRolType def
      checkBooleanFacetOfProperty "isVerplicht"
      checkBooleanFacetOfProperty "isFunctioneel"
      mogelijkeBindingSubsumedByAspect def
      checkRolDefPropertyValues def deftype

  where

    checkBooleanFacetOfProperty ::
      LocalName ->
      TDChecker Unit
    checkBooleanFacetOfProperty ln = do
      mlocalValue <- lift (unwrap def @@> (DTG.buitenRol >-> (DTG.getUnqualifiedProperty) ln))
      case mlocalValue of
        Just (Value "false") -> do
          b <- lift (def @@> aspectPropertiesValue)
          case b of
            Just (PBool "true") -> tell [CannotOverideBooleanRolProperty (unwrap def) ln]
            otherwise -> pure unit
        otherwise -> pure unit
      where
        aspectPropertiesValue :: (RolDef **> PBool)
        aspectPropertiesValue = STGC.some ((STGC.closure directAspectRoles) >-> getProp)
          where
            getProp :: (RolDef **> PBool)
            getProp = (unwrap `before` (searchExternalUnqualifiedProperty ln)) `followedBy` (wrap <<< unwrap)

-- | Checks the aspectRollen of the RolDefinition.
-- | If such an aspectRol is not a Rol of one of the Aspecten of Context definition
-- | that holds the Rol definition, it returns a warning.
checkAspectOfRolType :: RolDef -> TDChecker Unit
checkAspectOfRolType def = do
  ifNothing (lift $ lift (def ##> contextDef))
    (tell [RolWithoutContext $ unwrap def])
    \(contextdef :: ContextDef) -> do
      rollenVanAspecten <- lift $ lift $ (unwrap contextdef ##= STGC.closureOfAspect >-> rollenDef)
      (aspectrollen :: Array RolDef) <- lift $ lift (def ##= directAspectRoles)
      traverse_
        (\(aspectRol :: RolDef) -> do
          if isJust $ elemIndex aspectRol rollenVanAspecten
            then pure unit
            else tell [AspectRolNotFromAspect (unwrap def) (unwrap aspectRol) (unwrap contextdef)])
        aspectrollen

-- | Raises a warning if the Rol $mogelijkeBinding has no value (locally, on a prototype, or on an AspectRol).
-- | Then raises a warning if some AspectRol has a value for $mogelijkeBinding (without an alternative, in case of a sum type) that does not subsume the local value (if any).
mogelijkeBindingSubsumedByAspect :: RolDef -> TDChecker Unit
mogelijkeBindingSubsumedByAspect def = do
  mmbinding <- lift (def @@> mogelijkeBinding)
  case mmbinding of
    Nothing -> tell [MissingMogelijkeBinding $ unwrap def]
    (Just mbinding) -> do
      mlocalMbinding <- lift (def @@> ownMogelijkeBinding)
      case mlocalMbinding of
        Nothing -> pure unit
        (Just localMbinding) -> do
          aspectRollen <- lift (def @@= closureOfAspectRol)
          for_ aspectRollen
            \aspectRol -> do
              (bools :: Array Boolean) <- (lift (aspectRol @@= ownMogelijkeBinding >-> sumToSequence)) >>= (traverse
                \alternative -> lift (toBoolean (isSubsumedOnEachRolTelescopeOf localMbinding) alternative))
                -- read as: localMbinding ## (hasTypeOnEachRolTelescopeOf alternative)
                -- means: alternative is on each rolTelescope that starts with localMbinding.
                -- | On each path through the mogelijkeBinding graph of localMbinding there is a type x for which holds:
                -- | x hasContextType alternative, or:
                -- | alternative isContextTypeOf x
                -- maar het moet zijn:
                -- | x isOrHasAspect x
              if ala Disj foldMap bools
                then pure unit
                else do
                  aspectMbinding <- lift (aspectRol @@= ownMogelijkeBinding)
                  tell [MogelijkeBindingNotSubsumed localMbinding (unwrap aspectRol) (show aspectMbinding) (unwrap def)]

-- | Does the RolDef assign a value to the mandatory internal and external properties of the type of the
-- | RolDef? These values can be assigned to the RolDef itself (on its binnen- or buitenrol), or
-- | on its prototypes or AspectRollen.
checkRolDefPropertyValues :: RolDef -> ContextDef -> TDChecker Unit
checkRolDefPropertyValues def deftype = do
  mandatoryExternalProperties <- lift (unwrap deftype @@= buitenRolBeschrijvingDef >-> mandatoryProperties)
  traverse_ checkExternalProperty mandatoryExternalProperties

  mandatoryInternalProperties <- lift (unwrap deftype @@= binnenRolBeschrijvingDef >-> mandatoryProperties)
  traverse_ checkExternalProperty mandatoryExternalProperties

  where
    findExternalValue :: PropertyDef -> (AnyContext **> Value)
    findExternalValue pdef = searchInAspectRolesAndPrototypes (DTG.buitenRol >-> searchProperty pdef)

    checkExternalProperty :: PropertyDef -> TDChecker Unit
    checkExternalProperty pdef = ifNothing (lift $ lift (unwrap def ##> findExternalValue pdef))
      (tell [MissingExternalPropertyValue (unwrap pdef) (unwrap def)])
      (\ignore -> pure unit)

    findInternalValue :: PropertyDef -> (AnyContext **> String)
    findInternalValue pdef = searchInAspectRolesAndPrototypes (DTG.binnenRol >-> unwrap `before` (STGC.searchInRolTelescope ((getInternalProperty pdef) `followedBy` unwrap)))

    checkInternalProperty :: PropertyDef -> TDChecker Unit
    checkInternalProperty pdef = ifNothing (lift $ lift (unwrap def ##> findInternalValue pdef))
      (tell [MissingInternalPropertyValue (unwrap pdef) (unwrap def)])
      (\ignore -> pure unit)

-- | Returns a warning if the AspectRollen of the Rol definition include the definition itself.
checkCyclicAspectRoles :: RolDef -> TDChecker Boolean
checkCyclicAspectRoles cid = do
  aspects <- lift $ lift (cid ##= closureOfAspectRol)
  case elemIndex cid aspects of
    Nothing -> pure false
    otherwise -> tell [CycleInAspectRoles (unwrap cid) (map unwrap aspects)] *> pure true


-----------------------------------------------------------
-- CHECKPROPERTYDEF
-----------------------------------------------------------
-- | Is there no cycle in AspectProperty?
-- | The properties isFunctioneel and isVerplicht have default values (false),
-- | so we need not check whether there is a value.
-- | But when these properties have a local value equal to false, none of their
-- | AspectProperties may assign true to them!
-- | Is the range of the Property given with the PropertyDef, or one of its AspectProperties?
-- | Is the range of the Property subsumed by the ranges of its AspectProperties?
-- | Are all AspectProperties Properties of AspectRollen of the defining Rol?
-- | If a BindingProperty is specified, at least one AspectProperty should be given, too.
-- | The BindingProperty that is bound to an AspectProperty should have a range that is subsumed by
-- | the AspectProperties range. The same restrictions apply to the isFunctioneel and isVerplicht properties
-- | of the BindingProperty as to a locally defined Property with an AspectProperty.
-- | The BindingProperty should be a Property of the mogelijkeBinding (recursively).
checkPropertyDef :: PropertyDef -> ContextDef -> TDChecker Unit
checkPropertyDef def deftype = do
  ifM (checkCyclicAspectProperties def)
    (pure unit)
    do
      checkAspectsOfPropertyType
      mBindingproperty <- lift (def @@> bindingProperty)
      case mBindingproperty of
        Nothing -> do
          -- Below, the first occurrence of 'def' provides the value, the second provides the AspectProperties from
          -- which a value will be retrieved to compare it with.
          checkBooleanFacetOfProperty CannotOverrideBooleanAspectProperty def def "isVerplicht"
          checkBooleanFacetOfProperty CannotOverrideBooleanAspectProperty def def "isFunctioneel"
          checkRangeDef RangeNotSubsumed def def
        (Just (bindingproperty :: PropertyDef)) -> do

          (PBool b) <- lift (unwrap def @@>> containedIn bindingproperty (enclosingDefinition `followedBy` RolDef >-> propertiesDef))
          if (b == "true") then pure unit else (tell [BindingPropertyNotAvailable (unwrap def) (unwrap bindingproperty)])

          ifNothing (lift (def @@> directAspectProperties))
            (tell [MissingAspectPropertyForBindingProperty (unwrap def) (unwrap bindingproperty)])
            \ignore -> do
              checkBooleanFacetOfProperty (BindingPropertyCannotOverrideBooleanAspectProperty (unwrap def)) bindingproperty def "isVerplicht"
              checkBooleanFacetOfProperty (BindingPropertyCannotOverrideBooleanAspectProperty (unwrap def)) bindingproperty def "isFunctioneel"
              checkRangeDef (RangeNotSubsumedByBindingProperty (unwrap def)) bindingproperty def

  where

    -- | Checks the aspectRollen of the RolDefinition.
    -- | If such an aspectRol is not a Rol of one of the Aspecten of Context definition
    -- | that holds the Rol definition, it returns a warning.
    checkAspectsOfPropertyType :: TDChecker Unit
    checkAspectsOfPropertyType = do
      ifNothing (lift $ lift (def ##> rolDef))
        (tell [PropertyWithoutRol $ unwrap def])
        \(roldef :: RolDef) -> do
          propertiesVanAspecten <- lift $ lift $ (roldef ##= closureOfAspectRol >-> propertiesDef)
          (aspectproperties :: Array PropertyDef) <- lift $ lift (def ##= directAspectProperties)
          traverse_
            (\(aspectProperty :: PropertyDef) -> do
              if isJust $ elemIndex aspectProperty propertiesVanAspecten
                then pure unit
                else tell [AspectPropertyNotFromAspectRol (unwrap def) (unwrap aspectProperty) (unwrap roldef)])
            aspectproperties

    checkBooleanFacetOfProperty ::
      (String -> String -> UserMessage) ->
      PropertyDef ->
      PropertyDef ->
      LocalName ->
      TDChecker Unit
    checkBooleanFacetOfProperty userMessageConstructor defWithValues defWithAspects ln = do
      mlocalValue <- lift (unwrap defWithValues @@> (DTG.buitenRol >-> (DTG.getUnqualifiedProperty) ln))
      case mlocalValue of
        Just (Value "false") -> do
          b <- lift (defWithAspects @@> aspectPropertiesValue)
          case b of
            Just (PBool "true") -> tell [userMessageConstructor (unwrap defWithValues) ln]
            otherwise -> pure unit
        otherwise -> pure unit
      where
        aspectPropertiesValue :: (PropertyDef **> PBool)
        aspectPropertiesValue = STGC.some ((STGC.closure directAspectProperties) >-> getProp)
          where
            getProp :: (PropertyDef **> PBool)
            getProp = (unwrap `before` (searchExternalUnqualifiedProperty ln)) `followedBy` (wrap <<< unwrap)

    -- Checks if a range has been defined, somewhere.
    -- If so, checks if that range is subsumed by the range of each AspectProperty.
    checkRangeDef ::
      (String -> String -> String -> String -> UserMessage) ->
      PropertyDef ->
      PropertyDef ->
      TDChecker Unit
    checkRangeDef userMessageConstructor defWithValues defWithAspects = do
      mrange <- lift (defWithValues @@> rangeDef)
      case mrange of
        Nothing -> tell [MissingRange $ unwrap defWithValues]
        (Just range) -> do
          mlocalRange <- lift (defWithValues @@> ownRangeDef)
          case mlocalRange of
            Nothing -> pure unit
            (Just (SimpleValueDef localRangeDef)) -> lift (defWithAspects @@= closureOfAspectProperty) >>= traverse_
              \aspectProp -> ifNothing (lift (aspectProp @@> ownRangeDef))
                (pure unit)
                (\(SimpleValueDef aspectRange) -> ifM (lift $ lift $ (ContextDef localRangeDef) `TC.isOrHasAspect` (ContextDef aspectRange))
                  (pure unit)
                  (tell [userMessageConstructor localRangeDef (unwrap aspectProp) aspectRange (unwrap defWithValues)]))

-- | Returns a warning if the AspectProperties of the Rol definition include the definition itself.
checkCyclicAspectProperties :: PropertyDef -> TDChecker Boolean
checkCyclicAspectProperties cid = do
  aspects <- lift $ lift (cid ##= closureOfAspectProperty)
  case elemIndex cid aspects of
    Nothing -> pure false
    otherwise -> tell [CycleInAspectProperties (unwrap cid) (map unwrap aspects)] *> pure true

-----------------------------------------------------------
-- CHECKCONTEXT
-----------------------------------------------------------
-- | Is there an instance in Context for each Rol that is defined for ContextDef?
mandatoryRolesInstantiated :: Context -> ContextDef -> TDChecker Unit
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
-- | Is the prototype of the same type as the context?
checksForEachContext :: Context -> ContextDef -> TDChecker Unit
checksForEachContext def deftype = do
  (buitenrol :: PerspectRol) <- lift $ lift $ getPerspectEntiteit $ buitenRol $ unwrap def
  (binnenrol :: PerspectRol) <- lift $ lift $ getPerspectEntiteit $ binnenRol $ unwrap def

  definedInternalProperties <- lift (unwrap deftype @@= binnenRolBeschrijvingDef >-> propertiesDef)
  traverse_ (checkRange def binnenrol (getBinnenRolPropertyValues def binnenrol)) definedInternalProperties

  definedExternalProperties <- lift (unwrap deftype @@= buitenRolBeschrijvingDef >-> propertiesDef)
  traverse_ (checkRange def buitenrol (getPropertyValues def buitenrol)) definedExternalProperties

  availableExternalProperties <- lift ((buitenRol $ unwrap def) @@= DTG.propertyTypen)
  checkPropertyIsDefined buitenrol availableExternalProperties definedExternalProperties def

  availableInternalProperties <- lift ((binnenRol $ unwrap def)  @@= DTG.propertyTypen)
  checkPropertyIsDefined binnenrol availableInternalProperties definedInternalProperties def

  checkIfRolesHaveDefinition deftype def

  checkPrototype def deftype

  (lift $ lift (unwrap deftype ##= nonQueryRollen)) >>= traverse_ (compareRolInstancesToDefinition def)

checkPrototype :: Context -> ContextDef -> TDChecker Unit
checkPrototype def deftype = ifNothing (lift $ lift (unwrap def ##> getPrototype >-> DTG.contextType))
  (pure unit)
  (\ptType -> ifM (lift (toBoolean (hasContextType ptType) (unwrap def)))
    (pure unit)
    (tell [IncompatiblePrototype (unwrap def) (unwrap deftype) ptType]))

-- | Does the rol type hold a definition for all properties given to the rol instance?
checkPropertyIsDefined ::
  PerspectRol ->
  Array PropertyName ->
  Array PropertyDef ->
  Context ->
  TDChecker Unit
checkPropertyIsDefined rolInstance availableProperties definedProperties cid = do
  roltype <- pure $ RolDef $ rol_pspType rolInstance
  for_ availableProperties (isDefined roltype)

    where

      -- | Check if the PropertyDef is a member of the defined properties.
      isDefined :: RolDef -> PropertyName -> TDChecker Unit
      isDefined roltype propertyName =
        case elemIndex (PropertyDef propertyName) definedProperties of
          Nothing -> tell [PropertyNotDefined (unwrap cid) propertyName (rol_id rolInstance) (unwrap roltype)]
          otherwise -> pure unit
-- | Checks the value of the Property with the range that has been defined.
-- | If the Property is functional and more than one value has been given, adds a message.
-- | If the string-valued value of the property cannot be parsed into its declared type,
-- | adds a message.
checkRange ::
  Context ->
  PerspectRol ->
  (PropertyDef -> TDChecker (Array String)) ->
  PropertyDef ->
  TDChecker Unit
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

    tryParseSimpleValue :: SimpleValueDef -> String -> TDChecker Boolean
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

propertyIsFunctional :: PropertyDef -> MonadPerspectivesQuery Boolean
propertyIsFunctional = toBoolean propertyIsFunctioneel

-- | For each RolInstance in the ContextInstance, is there a Rol defined with the Context?
-- | `psp:Context -> psp:ContextInstance -> psp:ElkType`
checkIfRolesHaveDefinition :: ContextDef -> Context -> TDChecker Unit
checkIfRolesHaveDefinition deftype def = do
  availableRoles <- lift (unwrap def @@= DTG.typeVanIedereRolInContext)
  for_ availableRoles (isDefined <<< RolDef)
  where

    definedRollen :: TDChecker (Array RolDef)
    definedRollen = lift $ lift (unwrap deftype ##= rollenDef)

    isDefined :: RolDef -> TDChecker Unit
    isDefined rolType = do
      rollen <- definedRollen
      case elemIndex rolType rollen of
        Nothing -> tell [RolNotDefined (unwrap rolType) (unwrap def) (unwrap deftype)]
        otherwise -> pure unit

-- | Returns a warning if the Aspecten of the Context definition include the definition itself.
checkCyclicAspects :: ContextDef -> TDChecker Boolean
checkCyclicAspects cid = do
  aspects <- lift $ lift (unwrap cid ##= STGC.closureOfAspect)
  case elemIndex (unwrap cid) aspects of
    Nothing -> pure false
    otherwise -> tell [CycleInAspects (unwrap cid) aspects] *> pure true

-- | For this Rol (definition), check if an instance is available on the context.
-- | As RolDef is mandatory, it cannot be defined as a Query (it must be
-- | represented locally or on a prototype of the context).
-- | A Query is defined with psp:Function, which is a psp:Context and not a
-- | psp:Rol. Hence it does not have $isVerplicht as a Property.
checkRolAvailable :: Context -> RolDef -> TDChecker Unit
checkRolAvailable def rolType = ifM (lift $ lift $ searchContextRol rolType (unwrap def) >>= pure <<< null)
  (tell [MissingRolInstance (unwrap rolType) (unwrap def)])
  (pure unit)

-- | For this ContextRol (definition), looks for its instances in the ContextInstance.
-- | Compares that RolInstance to its definition:
-- |  1. Checks each defined property with the instance of the rol.
-- |  2. Checks the type of the binding, if given.
-- | Note that RolDef will never be a query, i.e. it does not have psp:Function
-- | as Aspect.
compareRolInstancesToDefinition :: Context -> RolDef -> TDChecker Unit
compareRolInstancesToDefinition def rolType =
  (lift $ lift $ searchContextRol rolType (unwrap def)) >>= traverse_ compareRolInstanceToDefinition

  where

    compareRolInstanceToDefinition :: ContextRol -> TDChecker Unit
    compareRolInstanceToDefinition rolInstance = do
      -- Check the properties.
      (definedRolProperties :: Array PropertyDef) <- lift (rolType @@= propertiesDef)
      (rol :: PerspectRol) <- lift $ lift $ getPerspectEntiteit (unwrap rolInstance)
      traverse_ (checkRange def rol (getPropertyValues def rol)) definedRolProperties
      lift (rolType @@= mandatoryProperties) >>= traverse_ (checkPropertyAvailable (ContextDef $ unwrap def) rol (getPropertyValues def rol))

      -- Detect used but undefined properties.
      (availableProperties :: Array String) <- lift $ lift $ propertyTypen $ unwrap rolInstance
      checkPropertyIsDefined rol availableProperties definedRolProperties def

      -- TODO. The implementation below is superfluous. The rolInstance is always
      -- a ContextRol.
      -- The rolInstance can be both a ContextRol and a RolInContext.
      -- We do not distinghuish the two as types. However, the former is, by definition, bound to a
      -- BuitenRol, while the latter - again by definition - is not.
      (mBnd :: Maybe BuitenRol) <- lift (rolInstance @@> DTG.binding)
      case mBnd of
        Nothing -> pure unit
        (Just bnd) -> ifM (lift $ lift $ isBuitenRol bnd)
          (checkBindingOfContextRol def rolType rolInstance)
          (checkBindingOfRolInContext def rolType (RolInContext $ unwrap rolInstance) (RolInContext $ unwrap bnd))

    checkPropertyAvailable ::
      ContextDef ->
      PerspectRol ->
      (PropertyDef -> TDChecker (Array String)) ->
      PropertyDef ->
      TDChecker Unit
    checkPropertyAvailable cid rol getter propertyType = ifM (getter propertyType >>= pure <<< null)
        (tell [MissingPropertyValue (unwrap cid) (unwrap propertyType) (rol_id rol)])
        (pure unit)

-- Check a ContextRol as follows. We assume that the bound value represents a definition of some kind.
-- Because its type can be a RolDef, we involve the mogelijkeBinding of that RolDef in the type checking.
--  - find the values of mogelijkeBinding of the type of the rolInstance: the possibleBindings;
--  - then there must be at least one possibleBinding for which holds: there must be a type x on each rolTelescope starting at that possibleBinding, for which (x `hasContextType` boundValue).
-- Note that because we include the head of the rolGraph in the check, if we do not have a RolDef, it will merely
-- check the bound value against each of the possibleBindings.
checkBindingOfContextRol :: Context -> RolDef -> ContextRol -> TDChecker Unit
checkBindingOfContextRol def rolType rolInstance = do
  mBoundValue <- lift (rolInstance @@> DTG.rolBindingDef)
  case mBoundValue of
    Nothing -> pure unit
    (Just boundValue) -> do
      (r :: Maybe PBool) <- lift (rolInstance @@> STGC.some (DTG.rolType >-> mogelijkeBinding >-> sumToSequence >-> (hasContextTypeOnEachRolTelescopeOf boundValue)))
      case r of
        (Just (PBool "false")) -> do
          typeOfTheBinding <- ifNothing (lift (boundValue @@> expressionType)) (pure "no type of binding") (pure <<< identity)
          possibleBindings <- (lift (rolType @@= mogelijkeBinding >-> sumToSequence))
          (tell [IncorrectContextRolBinding (unwrap def) (unwrap rolInstance) boundValue typeOfTheBinding (show possibleBindings)])
        otherwise -> pure unit

-- Check a RolInContext in the same way as a ContextRol, but compare the *type* of the bound value to the possibleBindings.
-- rolType is the type of rolInstance
checkBindingOfRolInContext :: Context -> RolDef -> RolInContext -> RolInContext -> TDChecker Unit
checkBindingOfRolInContext def rolType rolInstance boundValue = do
  -- refactor in the following line: replace rolInstance with rolType and remove DTG.rolType
  (r :: Maybe PBool) <- lift (rolInstance @@> STGC.some (DTG.rolType >-> mogelijkeBinding >-> sumToSequence >-> (hasRolTypeOnEachRolTelescopeOf boundValue)))
  case r of
    (Just (PBool "false")) -> do
      typeOfTheBinding <- ifNothing (lift (boundValue @@> DTG.rolType)) (pure "no type of binding") (pure <<< unwrap)
      toegestaneBindingen <- (lift (rolType @@= mogelijkeBinding >-> sumToSequence))
      (tell [IncorrectRolinContextBinding (unwrap def) (unwrap rolInstance) (unwrap boundValue) typeOfTheBinding (show toegestaneBindingen)])
    otherwise -> pure unit
