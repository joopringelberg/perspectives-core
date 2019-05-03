module Perspectives.ModelBasedObjectGetters where

import Control.Alt ((<|>))
import Data.Newtype (unwrap, wrap)
import Perspectives.CoreTypes (type (~~>))
import Perspectives.DataTypeObjectGetters (buitenRol, context, contextType, iedereRolInContext, rolBindingDef)
import Perspectives.ObjectGetterConstructors (agreesWithType, all, alternatives, closure, closureOfAspectProperty, closureOfAspectRol, closure_, concat, cond, conj, directAspectProperties, directAspects, filter, getContextRol, getRolInContext, getRoleBinders, getUnqualifiedRoleBinders, mogelijkeBinding, notEmpty, searchContextRol, searchExternalUnqualifiedProperty, searchInAspectsAndPrototypes, some, unlessFalse, unlessNull)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.PerspectivesTypes (AnyContext, AnyDefinition, BuitenRol, ContextDef, ContextRol(..), PBool, PropertyDef, RolDef(..), SimpleValueDef(..), binding, typeWithPerspectivesTypes)
import Prelude (($), (>=>), (<<<), pure, map, (>>>))


-- | True iff the RolDef or one of its AspectRollen has given property "isVerplicht" the value "true".
-- Test.Perspectives.ModelBasedObjectGetters
-- | Defaults to false. So if no value is given, this function returns "false".
rolIsVerplicht :: (RolDef ~~> PBool)
rolIsVerplicht = some (concat isVerplicht (closureOfAspectRol /-/ isVerplicht))
  where
    isVerplicht :: (RolDef ~~> PBool)
    isVerplicht = unwrap >>> searchExternalUnqualifiedProperty "isVerplicht" >=> pure <<< map wrap <<< map unwrap

-- | Equal to the 'own' $isVerplicht value; otherwise the logical or of the #aspectProperty values.
-- | Defaults to false. So if no value is given, this function returns "false".
rolIsFunctioneel :: (RolDef ~~> PBool)
rolIsFunctioneel = some (concat isFunctioneel (closureOfAspectRol /-/ isFunctioneel))
  where
    isFunctioneel :: (RolDef ~~> PBool)
    isFunctioneel = unwrap >>> searchExternalUnqualifiedProperty "isFunctioneel" >=> pure <<< map wrap <<< map unwrap

-- | Equal to the 'own' $isVerplicht value; otherwise the logical or of the #aspectProperty values.
-- | Defaults to false. So if no value is given, this function returns "false".
-- Test.Perspectives.ModelBasedObjectGetters
propertyIsVerplicht :: (PropertyDef ~~> PBool)
propertyIsVerplicht = some ((closure_ directAspectProperties) /-/ isVerplicht)
  where
    isVerplicht :: (PropertyDef ~~> PBool)
    isVerplicht = unwrap >>> searchExternalUnqualifiedProperty "isVerplicht" >=> pure <<< map wrap <<< map unwrap

-- | Equal to the 'own' $isFunctioneel value; otherwise the logical or of the #aspectProperty values.
-- | Defaults to false. So if no value is given, this function returns "false".
propertyIsFunctioneel :: (PropertyDef ~~> PBool)
propertyIsFunctioneel = some (concat isFunctioneel (closureOfAspectProperty /-/ isFunctioneel))
  where
    isFunctioneel :: (PropertyDef ~~> PBool)
    isFunctioneel = unwrap >>> searchExternalUnqualifiedProperty "isFunctioneel" >=> pure <<< map wrap <<< map unwrap

-- | The type of the range that has been defined for the Property.
rangeDef :: (PropertyDef ~~> SimpleValueDef)
rangeDef = unwrap >>> (searchContextRol (RolDef "model:Perspectives$Property$range") /-/ binding /-/ context) >=> pure <<< map SimpleValueDef

-- | Get the rol psp:Context$buitenRolBeschrijving.
buitenRolBeschrijving :: (AnyContext ~~> ContextRol)
buitenRolBeschrijving = getContextRol $ RolDef "model:Perspectives$Context$buitenRolBeschrijving"

-- | From a context that is a definition, get the definition of its BuitenRol.
-- Test.Perspectives.ObjectGetterConstructors
buitenRolBeschrijvingDef :: (AnyDefinition ~~> RolDef)
buitenRolBeschrijvingDef = searchInAspectsAndPrototypes f
  where
    f :: (AnyContext ~~> RolDef)
    f = typeWithPerspectivesTypes $ buitenRolBeschrijving /-/ binding /-/ context

-- | Get the rol psp:Context$binnenRolBeschrijving.
binnenRolBeschrijving :: (AnyContext ~~> ContextRol)
binnenRolBeschrijving = getContextRol $ RolDef "model:Perspectives$Context$binnenRolBeschrijving"

-- | From a context that is a definition, get the definition of its BinnenRol.
binnenRolBeschrijvingDef :: (AnyDefinition ~~> RolDef)
binnenRolBeschrijvingDef = searchInAspectsAndPrototypes f
  where
    f :: (AnyContext ~~> RolDef)
    f = typeWithPerspectivesTypes $ binnenRolBeschrijving /-/ binding /-/ context

-- | A RolDef must be embedded in a ContextDef. There are but a few roles to embed a RolDef.
contextDef :: (RolDef ~~> ContextDef)
contextDef rid =
  unlessNull rolInContextContextDef rid <|>
  unlessNull binnenRolContextDef rid <|>
  unlessNull buitenRolContextDef rid <|>
  -- unlessNull mogelijkeBinding rid <|>
  unlessNull gebruikerRolContextDef rid <|>
  unlessNull subjectContextDef rid <|>
  unlessNull objectContextDef rid <|>
  contextBotContextDef rid

-- | Get the ContextDef that holds the RolDef in the given Rol.
rolDef2ContextDef :: RolDef -> (RolDef ~~> ContextDef)
rolDef2ContextDef rd = unwrap >>> (buitenRol /-/ (getRoleBinders rd :: (BuitenRol ~~> ContextRol)) /-/ context) >=> pure <<< map wrap

localName2ContextDef ::String -> (RolDef ~~> ContextDef)
localName2ContextDef ln = unwrap >>> (buitenRol /-/ (getUnqualifiedRoleBinders ln :: (BuitenRol ~~> ContextRol)) /-/ context) >=> pure <<< map wrap

-- | The Context of the RolInContext.
rolInContextContextDef :: (RolDef ~~> ContextDef)
rolInContextContextDef = rolDef2ContextDef (RolDef "model:Perspectives$Context$rolInContext")

-- | The Context of the gebruikerRol.
gebruikerRolContextDef :: (RolDef ~~> ContextDef)
gebruikerRolContextDef = rolDef2ContextDef (RolDef "model:Perspectives$Context$gebruikerRol")

-- | The Context of the contextBot.
contextBotContextDef :: (RolDef ~~> ContextDef)
contextBotContextDef = rolDef2ContextDef (RolDef "model:Perspectives$Context$contextBot")

-- | The Context of the BinnenRol.
binnenRolContextDef :: (RolDef ~~> ContextDef)
binnenRolContextDef = localName2ContextDef "binnenRolBeschrijving"

-- | The Context of the buitenRolBeschrijving. I.e. starting from a Context that is a BuitenRolBeschrijving, returns
-- | the Context that describes the type that the BuitenRolBeschrijving belongs to.
buitenRolContextDef :: (RolDef ~~> ContextDef)
buitenRolContextDef = localName2ContextDef "buitenRolBeschrijving"

-- | The Context of the subjectRol.
subjectContextDef :: (RolDef ~~> ContextDef)
subjectContextDef = localName2ContextDef "subject"

-- | The Context of the objectRol.
objectContextDef :: (RolDef ~~> ContextDef)
objectContextDef =  localName2ContextDef "object"

-- | From the definition of a Property, find the enclosing definition of the Rol it is defined on.
rolDef :: (PropertyDef ~~> RolDef)
rolDef = unwrap >>> (buitenRol /-/ (getRoleBinders (RolDef "model:Perspectives$Rol$rolProperty") :: (BuitenRol ~~> ContextRol)) /-/ context) >=> pure <<< map RolDef

-- | All Rollen defined for a Context type, excluding Aspects.
ownRollenDef :: (AnyContext ~~> RolDef)
ownRollenDef = (filter (hasType "model:Perspectives$Rol") (iedereRolInContext >=> (pure <<< map ContextRol) /-/ binding /-/ context)) >=> pure <<< map RolDef


isContextTypeOf :: AnyContext -> (AnyDefinition ~~> PBool)
isContextTypeOf x = some (expressionType /-/ closure_ directAspects /-/ (agreesWithType x) )

hasType :: AnyDefinition -> (AnyContext ~~> PBool)
hasType q = contextType /-/ isOrHasAspect q

isOrHasAspect :: AnyDefinition -> (AnyDefinition ~~> PBool)
isOrHasAspect q = some (closure_ directAspects /-/ agreesWithType q)

hasAspect :: AnyDefinition -> (AnyDefinition ~~> PBool)
hasAspect q = some (closure directAspects /-/ agreesWithType q)

sumToSequence :: (AnyDefinition ~~> AnyDefinition)
sumToSequence t = unlessNull alternatives t <|> pure [t]

expressionType :: (AnyContext ~~> AnyDefinition)
expressionType = cond (hasType "model:Perspectives$Function") getFunctionResultType contextType

getFunctionResultType :: (AnyContext ~~> AnyDefinition)
getFunctionResultType = getRolInContext (RolDef "model:Perspectives$Function$result") /-/ rolBindingDef

-- | True iff t (the first parameter)ither agrees with the head of the graph, or if it is in the rol telescope
-- | for each of its mogelijkeBindingen.
hasOnEachRolTelescopeTheContextTypeOf :: RolDef -> (RolDef ~~> PBool)
hasOnEachRolTelescopeTheContextTypeOf t headOfGraph = unlessFalse (isContextTypeOf $ unwrap t) (unwrap headOfGraph)
  <|>
  ((conj
    (notEmpty (mogelijkeBinding /-/ sumToSequence))
    (all (unwrap >>> alternatives >=> (pure <<< map RolDef) /-/ (hasOnEachRolTelescopeTheContextTypeOf t))))
    headOfGraph)

getDefaultPrototype :: (AnyContext ~~> BuitenRol)
getDefaultPrototype = searchInAspectsAndPrototypes ((getContextRol (RolDef "model:Perspectives$Context$defaultPrototype")) /-/ binding)
