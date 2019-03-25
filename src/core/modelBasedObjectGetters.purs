module Perspectives.ModelBasedObjectGetters where

import Control.Alt ((<|>))
import Data.Newtype (unwrap, wrap)
import Perspectives.CoreTypes (type (~~>))
import Perspectives.DataTypeObjectGetters (buitenRol, context)
import Perspectives.ObjectGetterConstructors (closureOfAspectProperty, closureOfAspectRol, closure_, concat, directAspectProperties, getContextRol, getRoleBinders, searchContextRol, searchExternalUnqualifiedProperty, searchInAspectsAndPrototypes, some, unlessNull)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.PerspectivesTypes (AnyContext, AnyDefinition, BuitenRol, ContextDef, ContextRol, PBool, PropertyDef, RolDef(..), SimpleValueDef(..), binding, typeWithPerspectivesTypes)
import Prelude (($), (>=>), (<<<), pure, map, (>>>))


-- | True iff the RolDef or one of its AspectRollen has given property "isVerplicht" the value "true".
-- Test.Perspectives.ModelBasedObjectGetters
-- | Defaults to false. So if no value is given, this function returns "false".
rolIsVerplicht :: forall e. (RolDef ~~> PBool) e
rolIsVerplicht = some (concat isVerplicht (closureOfAspectRol /-/ isVerplicht))
  where
    isVerplicht :: (RolDef ~~> PBool) e
    isVerplicht = unwrap >>> searchExternalUnqualifiedProperty "isVerplicht" >=> pure <<< map wrap <<< map unwrap

-- | Equal to the 'own' $isVerplicht value; otherwise the logical or of the #aspectProperty values.
-- | Defaults to false. So if no value is given, this function returns "false".
rolIsFunctioneel :: forall e. (RolDef ~~> PBool) e
rolIsFunctioneel = some (concat isFunctioneel (closureOfAspectRol /-/ isFunctioneel))
  where
    isFunctioneel :: (RolDef ~~> PBool) e
    isFunctioneel = unwrap >>> searchExternalUnqualifiedProperty "isFunctioneel" >=> pure <<< map wrap <<< map unwrap

-- | Equal to the 'own' $isVerplicht value; otherwise the logical or of the #aspectProperty values.
-- | Defaults to false. So if no value is given, this function returns "false".
-- Test.Perspectives.ModelBasedObjectGetters
propertyIsVerplicht :: forall e. (PropertyDef ~~> PBool) e
propertyIsVerplicht = some ((closure_ directAspectProperties) /-/ isVerplicht)
  where
    isVerplicht :: (PropertyDef ~~> PBool) e
    isVerplicht = unwrap >>> searchExternalUnqualifiedProperty "isVerplicht" >=> pure <<< map wrap <<< map unwrap

-- | Equal to the 'own' $isFunctioneel value; otherwise the logical or of the #aspectProperty values.
-- | Defaults to false. So if no value is given, this function returns "false".
propertyIsFunctioneel :: forall e. (PropertyDef ~~> PBool) e
propertyIsFunctioneel = some (concat isFunctioneel (closureOfAspectProperty /-/ isFunctioneel))
  where
    isFunctioneel :: (PropertyDef ~~> PBool) e
    isFunctioneel = unwrap >>> searchExternalUnqualifiedProperty "isFunctioneel" >=> pure <<< map wrap <<< map unwrap

-- | The type of the range that has been defined for the Property.
rangeDef :: forall e. (PropertyDef ~~> SimpleValueDef) e
rangeDef = unwrap >>> searchContextRol (RolDef "model:Perspectives$Property$range") /-/ binding /-/ context >=> pure <<< map SimpleValueDef

-- | Get the rol psp:Context$buitenRolBeschrijving.
buitenRolBeschrijving :: forall e. (AnyContext ~~> ContextRol) e
buitenRolBeschrijving = getContextRol $ RolDef "model:Perspectives$Context$buitenRolBeschrijving"

-- | From a context that is a definition, get the definition of its BuitenRol.
-- Test.Perspectives.ObjectGetterConstructors
buitenRolBeschrijvingDef :: forall e. (AnyDefinition ~~> RolDef) e
buitenRolBeschrijvingDef = searchInAspectsAndPrototypes f
  where
    f :: (AnyContext ~~> RolDef) e
    f = typeWithPerspectivesTypes $ buitenRolBeschrijving /-/ binding /-/ context

-- | Get the rol psp:Context$binnenRolBeschrijving.
binnenRolBeschrijving :: forall e. (AnyContext ~~> ContextRol) e
binnenRolBeschrijving = getContextRol $ RolDef "model:Perspectives$Context$binnenRolBeschrijving"

-- | From a context that is a definition, get the definition of its BinnenRol.
binnenRolBeschrijvingDef :: forall e. (AnyDefinition ~~> RolDef) e
binnenRolBeschrijvingDef = searchInAspectsAndPrototypes f
  where
    f :: (AnyContext ~~> RolDef) e
    f = typeWithPerspectivesTypes $ binnenRolBeschrijving /-/ binding /-/ context

-- | `psp:Rol -> psp:Context`
contextDef :: forall e. (RolDef ~~> ContextDef) e
contextDef rid =
  unlessNull rolInContextContextDef rid <|>
  unlessNull binnenRolContextDef rid <|>
  unlessNull buitenRolContextDef rid <|>
  -- unlessNull mogelijkeBinding rid <|>
  unlessNull gebruikerRolContextDef rid <|>
  contextBotContextDef rid

-- | Get the ContextDef that holds the RolDef in the given Rol.
rolDef2ContextDef :: forall e. RolDef -> (RolDef ~~> ContextDef) e
rolDef2ContextDef rd = unwrap >>> buitenRol /-/ (getRoleBinders rd :: (BuitenRol ~~> ContextRol) e) /-/ context >=> pure <<< map wrap

-- | The Context of the RolInContext.
-- | `psp:Rol -> psp:Context`
rolInContextContextDef :: forall e. (RolDef ~~> ContextDef) e
rolInContextContextDef = rolDef2ContextDef (RolDef "model:Perspectives$Context$rolInContext")
-- rolInContextContextDef = unwrap >>> buitenRol /-/ (getRoleBinders (RolDef "model:Perspectives$Context$rolInContext") :: (BuitenRol ~~> ContextRol) e) /-/ context >=> pure <<< map wrap

-- | The Context of the gebruikerRol.
-- | `psp:Rol -> psp:Context`
gebruikerRolContextDef :: forall e. (RolDef ~~> ContextDef) e
gebruikerRolContextDef = rolDef2ContextDef (RolDef "model:Perspectives$Context$gebruikerRol")
-- gebruikerRolContextDef = buitenRol /-/ getRoleBinders "model:Perspectives$Context$gebruikerRol" /-/ context

-- | The Context of the contextBot.
-- | `psp:Rol -> psp:Context`
contextBotContextDef :: forall e. (RolDef ~~> ContextDef) e
contextBotContextDef = rolDef2ContextDef (RolDef "model:Perspectives$Context$contextBot")
-- contextBotContextDef = buitenRol /-/ getRoleBinders "model:Perspectives$Context$contextBot" /-/ context

-- | The Context of the BinnenRol.
-- | `psp:Rol -> psp:Context`
binnenRolContextDef :: forall e. (RolDef ~~> ContextDef) e
binnenRolContextDef = rolDef2ContextDef (RolDef "model:Perspectives$Context$binnenRolBeschrijving")
-- binnenRolContextDef = buitenRol /-/ getRoleBinders "model:Perspectives$Context$binnenRolBeschrijving" /-/ context

-- | The Context of the buitenRolBeschrijving. I.e. starting from a Context that is a BuitenRolBeschrijving, returns
-- | the Context that describes the type that the BuitenRolBeschrijving belongs to.
-- | `psp:Context -> psp:Context`
buitenRolContextDef :: forall e. (RolDef ~~> ContextDef) e
buitenRolContextDef = rolDef2ContextDef (RolDef "model:Perspectives$Context$buitenRolBeschrijving")
-- buitenRolContextDef = buitenRol /-/ getRoleBinders "model:Perspectives$Context$buitenRolBeschrijving" /-/ context

-- | From the definition of a Property, find the enclosing definition of the Rol it is defined on.
rolDef :: forall e. (PropertyDef ~~> RolDef) e
rolDef = unwrap >>> buitenRol /-/ (getRoleBinders (RolDef "model:Perspectives$Rol$rolProperty") :: (BuitenRol ~~> ContextRol) e) /-/ context >=> pure <<< map RolDef

-- | All Rollen defined for a Context type, excluding Aspects.
-- | `psp:Context -> psp:Rol`
ownRollenDef :: forall e. (AnyContext ~~> RolDef) e
ownRollenDef = getContextRol (RolDef "model:Perspectives$Context$rolInContext") /-/ binding /-/ context >=> pure <<< map RolDef
