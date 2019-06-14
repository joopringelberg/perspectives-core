module Perspectives.ModelBasedTripleGetters where

import Data.Foldable (foldMap)
import Data.Maybe (maybe)
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (alaF, unwrap, wrap)
import Perspectives.CoreTypes (type (**>), TypedTripleGetter(..), TripleGetter, (@@))
import Perspectives.DataTypeObjectGetters (rolType)
import Perspectives.DataTypeTripleGetters (binding, iedereRolInContext, label, context, genericBinding, rolBindingDef, buitenRol) as DTG
import Perspectives.DataTypeTripleGetters (contextType, genericRolType, identity) as DTTG
import Perspectives.DataTypeTripleGetters (identity)
import Perspectives.Identifiers (LocalName) as ID
import Perspectives.Identifiers (deconstructLocalNameFromDomeinURI)
import Perspectives.Identifiers as Id
import Perspectives.ModelBasedObjectGetters (buitenRolBeschrijvingDef, binnenRolBeschrijvingDef, contextDef, rolDef) as MBOG
import Perspectives.ObjectsGetterComposition (composeMonoidal)
import Perspectives.PerspectivesTypes (class RolClass, ActieDef, AnyContext, AnyDefinition, ContextDef(..), ContextRol(..), PBool(..), PropertyDef(..), RolDef(..), RolInContext(..), SimpleValueDef(..), UserRolDef, ZaakDef, typeWithPerspectivesTypes)
import Perspectives.QueryCombinators (closure', filter, notEmpty, difference, conj, contains, cond, union) as QC
import Perspectives.StringTripleGetterConstructors (closure, directAspects, getPrototype, getRolInContext) as STGC
import Perspectives.TripleGetterComposition (before, followedBy, lazyIntersectionOfTripleObjects, lazyUnionOfTripleObjects, preferLeft, (>->), unlessFalse)
import Perspectives.TripleGetterConstructors (agreesWithType, all, closureOfAspectProperty, closureOfAspectRol, closure_, concat, directAspectProperties, directAspectRoles, getContextRol, getRolInContext, getRoleBinders, searchContextRol, searchExternalUnqualifiedProperty, searchInAspectPropertiesAndPrototypes, searchInAspectRolesAndPrototypes, searchRolInContext, searchUnqualifiedPropertyDefinition, searchUnqualifiedRol, searchUnqualifiedRolDefinition, some, alternatives)
import Perspectives.TripleGetterFromObjectGetter (constructInverseRolGetter, trackedAs)
import Prelude (const, show, ($), (<<<), (<>), (==), (>>>))

-----------------------------------------------------------
-- GETTERS BASED ON MODEL:PERSPECTIVES$
-- These getters are based on properties (defined for roles) and roles (defined for contexts)
-- as modelled in the definitions of CRL and ARC.
-----------------------------------------------------------

-- | True if the Rol has been defined as mandatory.
rolIsVerplicht :: (RolDef **> PBool)
rolIsVerplicht = some (concat isVerplicht (closureOfAspectRol >-> isVerplicht))
  where
    isVerplicht :: (RolDef **> PBool)
    isVerplicht = (unwrap `before` (searchExternalUnqualifiedProperty "isVerplicht")) `followedBy`(wrap <<< unwrap)

-- | True if the Rol has been defined as functional.
rolIsFunctioneel :: (RolDef **> PBool)
rolIsFunctioneel = some (concat isFunctioneel (closureOfAspectRol >-> isFunctioneel))
  where
    isFunctioneel :: (RolDef **> PBool)
    isFunctioneel = (unwrap `before` (searchExternalUnqualifiedProperty "isFunctioneel")) `followedBy` (wrap <<< unwrap)

-- | True if the Property has been defined as mandatory (possibly in an aspect).
propertyIsVerplicht :: (PropertyDef **> PBool)
propertyIsVerplicht = some ((closure_ directAspectProperties) >-> isVerplicht)
  where
    isVerplicht :: (PropertyDef **> PBool)
    isVerplicht = (unwrap `before` (searchExternalUnqualifiedProperty "isVerplicht")) `followedBy` (wrap <<< unwrap)

-- | True if the Property has been defined as functional.
propertyIsFunctioneel :: (PropertyDef **> PBool)
propertyIsFunctioneel = some (concat isFunctioneel (closureOfAspectProperty >-> isFunctioneel))
  where
    isFunctioneel :: (PropertyDef **> PBool)
    isFunctioneel = (unwrap `before` (searchExternalUnqualifiedProperty "isFunctioneel")) `followedBy` (wrap <<< unwrap)

ownRangeDef :: (PropertyDef **> SimpleValueDef)
ownRangeDef = (unwrap `before` (searchContextRol (RolDef "model:Perspectives$Property$range") >-> DTG.binding >-> DTG.context)) `followedBy` SimpleValueDef

-- | For a PropertyDef, find its range locally, on prototypes or on AspectProperties.
rangeDef :: (PropertyDef **> SimpleValueDef)
rangeDef = unwrap `before` searchInAspectPropertiesAndPrototypes (PropertyDef `before` ownRangeDef)

-- | True iff the context instance has a label.
hasLabel :: (AnyContext **> PBool)
hasLabel = QC.notEmpty DTG.label

-- | True if the rol instance has a binding.
hasBinding :: forall r. RolClass r => (r **> PBool)
hasBinding = QC.notEmpty (unwrap `before` DTG.genericBinding)

-- | The role instance at the bottom of the telescope of the rol instance (that role instance will have no binding).
rolUser :: (RolInContext **> RolInContext)
rolUser = QC.closure' DTG.binding

-- | The view that is needed for the object of the Actie. Notice that the typing is not precise: instead of Action we
-- use ContextDef, instead of View we use Rol.
-- | That is not correct, but we do not have a separate types for Action, nor View.
objectViewDef :: (ContextDef **> RolDef)
objectViewDef = searchUnqualifiedRolDefinition "objectView"

-- | The PropertyReferences of the View. Again, the typing is imprecise.
propertyReferenties :: (RolDef **> ContextRol)
propertyReferenties =  typeWithPerspectivesTypes searchUnqualifiedRol "propertyReferentie"

-- | Tests whether the type of the Rol has a specific local name. Used to test if a Rol is a BuitenRol type or a BinnenRol type.
rolHasTypeWithLocalName :: ID.LocalName -> TypedTripleGetter String PBool
rolHasTypeWithLocalName localName = ((RolInContext >>> rolType) `composeMonoidal` f) `trackedAs` ("rolHasTypeWithLocalName" <> "_" <> localName)
  where
    f :: Array RolDef -> PBool
    f = PBool <<< show <<< alaF Disj foldMap (maybe false ((==) localName) <<< deconstructLocalNameFromDomeinURI <<< unwrap)

-- | The context instances that are bound to a rol of the context instance.
boundContexts :: (AnyContext **> ContextDef)
boundContexts = (QC.filter (rolHasTypeWithLocalName "buitenRolBeschrijving") (DTG.iedereRolInContext >-> DTG.genericBinding)) >-> ContextRol `before` DTG.context `followedBy` ContextDef

-- | The Acties that the Rol is the subject (Actor) of.
actiesOfRol :: (RolDef **> ActieDef)
actiesOfRol = unwrap `before` (searchRolInContext (RolDef "model:Perspectives$Rol$subjectRol")) >-> DTG.rolBindingDef `followedBy` ContextDef

-- | Move to the enclosing definition of the definition by reversing over $rolInContext.
enclosingDefinition :: (AnyDefinition **> AnyDefinition)
enclosingDefinition = DTG.buitenRol >-> constructInverseRolGetter (RolDef "model:Perspectives$Context$rolInContext") >-> (DTG.context :: (RolInContext **> AnyDefinition))

-- | All acties defined in the Context.
actiesInContextDef :: (ZaakDef **> ActieDef)
actiesInContextDef = unwrap `before` searchRolInContext (RolDef "model:Perspectives$Zaak$actieInContext") >-> DTG.binding >-> DTG.context `followedBy` ContextDef

-- | The Acties that the Rol is the object of.
objectRollenDef :: (RolDef **> ActieDef)
objectRollenDef = unwrap `before` searchRolInContext (RolDef "model:Perspectives$Rol$objectRol") >-> DTG.binding >-> DTG.context `followedBy` ContextDef
-- objectRollenDef = (getRoleBinders (RolDef "model:Perspectives$Actie$object") :: ()) >-> DTG.context `followedBy` ContextDef

-- | The Rollen that have this Actie as subjectRol.
inverse_subjectRollenDef :: (ActieDef **> UserRolDef)
inverse_subjectRollenDef = unwrap `before` DTG.buitenRol >-> constructInverseRolGetter (RolDef "model:Perspectives$Rol$subjectRol") >-> (DTG.context :: (RolInContext **> AnyContext)) `followedBy` RolDef

-- | The type of Rol or Context that can be bound to the Rol.
ownMogelijkeBinding :: (RolDef **> AnyDefinition)
ownMogelijkeBinding = unwrap `before` searchRolInContext (RolDef "model:Perspectives$Rol$mogelijkeBinding")  >-> DTG.binding >-> DTG.context

-- | The type of Rol or Context that can be bound to the Rol, taken
-- | from the RolDef itself or any aspectRol or prototype.
mogelijkeBinding :: (RolDef **> AnyDefinition)
mogelijkeBinding = unwrap `before` mbinding
  where
    mbinding :: (String **> String)
    mbinding = searchInAspectRolesAndPrototypes f

    f :: (String **> AnyDefinition)
    f = searchRolInContext (RolDef "model:Perspectives$Rol$mogelijkeBinding") >-> DTG.binding >-> DTG.context

-- | All Rollen defined for a Context type, excluding Aspects.
ownRollenDef :: (AnyContext **> RolDef)
ownRollenDef = (QC.filter (isContextTypeOf "model:Perspectives$Rol") (DTG.iedereRolInContext `followedBy` ContextRol >-> DTG.binding >-> DTG.context)) `followedBy` RolDef

-- All Rollen defined for a Context type, locally or in Aspects.
rollenDef :: (AnyContext **> RolDef)
rollenDef = closure_ STGC.directAspects >-> (closure_ STGC.getPrototype) >-> ownRollenDef

-- | All mandatory roles defined for a Context in Aspects and prototypes, that are not used as AspectRol in one of the others.
mandatoryRollen :: (AnyContext **> RolDef)
mandatoryRollen = QC.difference f (f >-> directAspectRoles)
  where
    f :: (AnyContext **> RolDef)
    f = QC.filter rolIsVerplicht (closure_ STGC.directAspects >-> (closure_ STGC.getPrototype) >-> ownRollenDef)

nonQueryRollen :: (AnyContext **> RolDef)
nonQueryRollen = QC.filter (unwrap `before` DTTG.contextType >-> isNotAQuery) rollenDef where
  isNotAQuery :: (AnyDefinition **> PBool)
  isNotAQuery = QC.contains "model:Perspectives$Rol" (closure_ STGC.directAspects)

-- | All properties defined in the namespace of the Rol.
ownPropertiesDef :: (RolDef **> PropertyDef)
ownPropertiesDef = unwrap `before` (getContextRol $ RolDef "model:Perspectives$Rol$rolProperty") >-> DTG.binding >-> DTG.context `followedBy` PropertyDef

-- | All properties defined for the Rol, in the same namespace, or on aspects, or on the MogelijkeBinding. Note that some of these may be an AspectProperty of others.
-- Test.Perspectives.ModelBasedTripleGetters
propertiesDef :: (RolDef **> PropertyDef)
propertiesDef = concat defsInAspectsAndPrototypes defsInMogelijkeBinding
  where

  defsInAspectsAndPrototypes :: (RolDef **> PropertyDef)
  defsInAspectsAndPrototypes = closure_ directAspectRoles >-> unwrap `before` (closure_ STGC.getPrototype) `followedBy` RolDef >-> ownPropertiesDef

  defsInMogelijkeBinding :: (RolDef **> PropertyDef)
  defsInMogelijkeBinding = lazyUnionOfTripleObjects
    (mogelijkeBinding >-> sumToSequence `followedBy` RolDef)
    (\_ -> propertiesDef)
    "propertiesDef"

-- | All mandatory properties defined for a Rol in Aspects and prototypes, that are not used as AspectProperty in one of the others.
mandatoryProperties :: (RolDef **> PropertyDef)
mandatoryProperties = QC.difference f (f >-> directAspectProperties)
  where
    f :: (RolDef **> PropertyDef)
    f = QC.filter propertyIsVerplicht (closure_ directAspectRoles >-> (unwrap `before` closure_ STGC.getPrototype) >-> RolDef `before` ownPropertiesDef)

buitenRolBeschrijvingDef :: (AnyDefinition **> RolDef)
buitenRolBeschrijvingDef = MBOG.buitenRolBeschrijvingDef `trackedAs` "model:Perspectives$Context$buitenRolBeschrijving"

binnenRolBeschrijvingDef :: (AnyDefinition **> RolDef)
binnenRolBeschrijvingDef = MBOG.binnenRolBeschrijvingDef `trackedAs` "model:Perspectives$Context$binnenRolBeschrijving"

-- | From the description of a Context, return its contextBot.
contextBot :: (ContextDef **> RolInContext)
contextBot = unwrap `before` getRolInContext (RolDef "model:Perspectives$Context$contextBot")

-- | The Acties of a Context that its contextBot is the subject (Actor) of.
botActiesInContext :: (ContextDef **> ActieDef)
botActiesInContext = contextBot >-> (getRoleBinders (RolDef "model:Perspectives$Actie$subject") :: (RolInContext **> RolInContext)) >-> DTG.context `followedBy` ContextDef

contextDef :: (RolDef **> ContextDef)
contextDef = MBOG.contextDef `trackedAs` "contextDef"

rolDef :: (PropertyDef **> RolDef)
rolDef = MBOG.rolDef `trackedAs` "rolDef"

bindingProperty :: (PropertyDef **> PropertyDef)
bindingProperty = unwrap `before` getContextRol (RolDef "model:Perspectives$Property$bindingProperty") >-> DTG.binding >-> DTG.context `followedBy` PropertyDef

type Instance = String

-- | True iff AnyDefinition is a type of AnyContext.
-- | q ## (isContextTypeOf p) should be understood as:
-- | q is a type of p
-- | q `isContextTypeOf` p
-- | AnyDefinition `isContextTypeOf` AnyContext
hasContextType :: AnyContext -> (AnyDefinition **> PBool)
hasContextType p = TypedTripleGetter ("hasContextType_" <> p) f where
  -- q ## (isContextTypeOf p) should be understood as:
  -- some y in aspects (type p) `agreesWithType` q
  -- (type p) `isOrHasAspect` q
  -- q is a type of p.
  f :: TripleGetter AnyDefinition PBool
  f x = p @@ some (expressionType >-> closure_ STGC.directAspects >-> (agreesWithType x) )

-- | True iff AnyDefinition is a type of r.
-- | r `hasRolType` AnyDefinition
hasRolType :: forall r. RolClass r => r -> (AnyDefinition **> PBool)
hasRolType r = TypedTripleGetter ("hasRolType_" <> unwrap r) f where
  f :: TripleGetter AnyDefinition PBool
  f tp = unwrap r @@ some (DTTG.genericRolType >-> closure_ (RolDef `before` directAspectRoles `followedBy` unwrap) >-> (agreesWithType tp))

-- | p `hasContextType` q should be written:
-- | p ## (hasContextType q)
-- | True iff the **type** of AnyContext
-- |  - equals AnyDefinition, or if
-- |  - one of its Aspects is AnyDefinition.
-- | AnyContext `hasContextType` AnyDefinition
-- | p ## (hasContextType q)
-- | (type of p) ## (isOrHasAspect q)
-- | (type of p) `isOrHasAspect` q
-- | p `hasContextType` q
-- | AnyContext `hasContextType` AnyDefinition
isContextTypeOf :: AnyDefinition -> (AnyContext **> PBool)
isContextTypeOf q = DTTG.contextType >-> isOrHasAspect q

-- | p `isOrHasAspect` q should be written:
-- | p ## (isOrHasAspect q)
-- | p `isOrHasAspect` q means either:
-- |  * p `agreesWithType` q
-- |  * for some Aspect a of p: a `agreesWithType` q
-- TODO de argumenten zijn omgedraaid.
isOrHasAspect :: AnyDefinition -> (AnyDefinition **> PBool)
isOrHasAspect q = some (closure_ STGC.directAspects >-> agreesWithType q)
-- | p ## (isOrHasAspect q)
-- | for some a in aspects p: a `agreesWithType` q
-- | q equals or is a superType of p
-- | p `isOrHasAspect` q
-- | Because:
-- | q is a superType of p means:
-- | q is an Aspect of p, or, equivalently:
-- | p has q as Aspect.

-- | p `isOrHasSuperType` q should be written:
-- | p ## (isOrHasSuperType q)
isOrHasSuperType :: AnyDefinition -> (AnyDefinition **> PBool)
isOrHasSuperType = isOrHasAspect

-- TODO de argumenten zijn omgedraaid.
hasAspect :: AnyDefinition -> (AnyDefinition **> PBool)
hasAspect q = some (STGC.closure STGC.directAspects >-> agreesWithType q)

sumToSequence :: (AnyDefinition **> AnyDefinition)
sumToSequence = (alternatives `preferLeft` const identity) "sumToSequence"

-- | True iff t (the first parameter)ither agrees with the head of the graph, or if it is in the rol telescope
-- | for each of its mogelijkeBindingen
-- | (is in each rol telescope that starts with the head of the graph).
hasTypeOnEachRolTelescopeOf :: RolDef -> (RolDef **> PBool)
hasTypeOnEachRolTelescopeOf t = ((unwrap `before` hasContextType (unwrap t)) `unlessFalse`
  \_ ->
    (QC.conj
      (QC.notEmpty (mogelijkeBinding >-> sumToSequence))
      (all (mogelijkeBinding >-> sumToSequence `followedBy` RolDef >-> (hasTypeOnEachRolTelescopeOf t)))))
  "hasTypeOnEachRolTelescopeOf"

-- | Collect all definitions of a Property with the local name, in the RolDef and its Aspects
-- | and in all their prototypes and on the rolGraph of the RolDef. Notice there may be more than one!
collectUnqualifiedPropertyDefinitions :: Id.LocalName -> (RolDef **> PropertyDef)
collectUnqualifiedPropertyDefinitions ln = QC.union (searchUnqualifiedPropertyDefinition ln)
  (lazyIntersectionOfTripleObjects
    (mogelijkeBinding >-> sumToSequence `followedBy` RolDef)
    (\_ -> (collectUnqualifiedPropertyDefinitions ln))
   "collectUnqualifiedPropertyDefinitions")

-- Test.ModelBasedTripleGetters
getFunctionResultType :: (AnyContext **> AnyDefinition)
getFunctionResultType = getRolInContext (RolDef "model:Perspectives$Function$result") >-> DTG.rolBindingDef

-- | The argument should be an instance of psp:Context.
-- | If the argument is not a function, we return its type.
-- | If the argument is an instance of psp:Function, we do not return its type (that would be psp:Function).
-- | Instead, we return the type of the **result** of the function. The result is explicitly given as
-- | the value bound to the role psp:Function$result.
expressionType :: (AnyContext **> AnyDefinition)
expressionType = QC.cond (isContextTypeOf "model:Perspectives$Function") (getFunctionResultType >-> DTTG.contextType) DTTG.contextType

-- | Applied to a type of Rol, this function will return the same type if the *type of the roltype* is psp:Rol.
-- | However, if the type of the roltype is psp:Function, it will return the value of psp:Function$Result of the roltype.
effectiveRolType :: (AnyContext **> AnyDefinition)
effectiveRolType = QC.cond (isContextTypeOf "model:Perspectives$Function") ((STGC.getRolInContext "model:Perspectives$Function$result") >-> DTG.genericBinding >-> DTTG.genericRolType) DTTG.identity
