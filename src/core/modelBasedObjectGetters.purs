module Perspectives.ModelBasedObjectGetters where

import Control.Alt ((<|>))
import Data.Newtype (unwrap, wrap)
import Perspectives.CoreTypes (type (~~>))
import Perspectives.DataTypeObjectGetters (buitenRol, context)
import Perspectives.ObjectGetterConstructors (closureOfAspectProperty, closureOfAspectRol, concat, getContextRol, getGebondenAls, searchContextRol, searchExternalUnqualifiedProperty, searchUnqualifiedRolDefinition, some, unlessNull)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.PerspectivesTypes (AnyContext, AnyDefinition, BuitenRol, ContextDef, ContextRol, PBool, PropertyDef, RolDef(..), SimpleValueDef(..), binding)
import Prelude (($), (>=>), (<<<), pure, map, (>>>))


-- | NOTE. The functions in this module have a type defined in their comment. These types all refer to
-- | *type-descriptions* in Perspectives. Hence, 'psp:Rol -> psp:Context' should be read: from the description
-- | of a Rol, retrieve the description of the Context that holds this Rol-description.

-- | True iff the RolDef or one of its AspectRollen has given property "isVerplicht" the value "true".
rolIsVerplicht :: forall e. (RolDef ~~> PBool) e
rolIsVerplicht = some (concat isVerplicht (closureOfAspectRol /-/ isVerplicht))
  where
    isVerplicht :: (RolDef ~~> PBool) e
    isVerplicht = unwrap >>> searchExternalUnqualifiedProperty "isVerplicht" >=> pure <<< map wrap <<< map unwrap

-- | Equal to the 'own' $isVerplicht value; otherwise the logical or of the #aspectProperty values.
rolIsFunctioneel :: forall e. (RolDef ~~> PBool) e
rolIsFunctioneel = some (concat isFunctioneel (closureOfAspectRol /-/ isFunctioneel))
  where
    isFunctioneel :: (RolDef ~~> PBool) e
    isFunctioneel = unwrap >>> searchExternalUnqualifiedProperty "isFunctioneel" >=> pure <<< map wrap <<< map unwrap

-- | Equal to the 'own' $isVerplicht value; otherwise the logical or of the #aspectProperty values.
propertyIsVerplicht :: forall e. (PropertyDef ~~> PBool) e
propertyIsVerplicht = some (concat isVerplicht (closureOfAspectProperty /-/ isVerplicht))
  where
    isVerplicht :: (PropertyDef ~~> PBool) e
    isVerplicht = unwrap >>> searchExternalUnqualifiedProperty "isVerplicht" >=> pure <<< map wrap <<< map unwrap

-- | Equal to the 'own' $isFunctioneel value; otherwise the logical or of the #aspectProperty values.
propertyIsFunctioneel :: forall e. (PropertyDef ~~> PBool) e
propertyIsFunctioneel = some (concat isFunctioneel (closureOfAspectProperty /-/ isFunctioneel))
  where
    isFunctioneel :: (PropertyDef ~~> PBool) e
    isFunctioneel = unwrap >>> searchExternalUnqualifiedProperty "isFunctioneel" >=> pure <<< map wrap <<< map unwrap

-- | The type of the range that has been defined for the Property.
range :: forall e. (PropertyDef ~~> SimpleValueDef) e
range = unwrap >>> wrap >>> searchContextRol (RolDef "model:Perspectives$Property$range") /-/ binding /-/ context >=> pure <<< map SimpleValueDef

-- | Get the rol psp:Context$buitenRolBeschrijving.
-- | `psp:Context -> psp:RolInstance`
buitenRolBeschrijving :: forall e. (AnyContext ~~> ContextRol) e
buitenRolBeschrijving = getContextRol $ RolDef "model:Perspectives$Context$buitenRolBeschrijving"

-- | From a context that is a definition, get the definition of its BuitenRol.
buitenRolBeschrijvingDef :: forall e. (AnyDefinition ~~> RolDef) e
buitenRolBeschrijvingDef = wrap >>> searchUnqualifiedRolDefinition "buitenRolBeschrijving"

-- | Get the psp:Context$buitenRol of a Context that is a definition. External properties of that Context
-- | are defined on that Rol.
-- | `psp:Context -> psp:RolInstance`
binnenRolBeschrijving :: forall e. (AnyContext ~~> ContextRol) e
binnenRolBeschrijving = getContextRol $ RolDef "model:Perspectives$Context$binnenRolBeschrijving"

-- | `psp:Rol -> psp:Context`
contextDef :: forall e. (RolDef ~~> ContextDef) e
contextDef rid =
  unlessNull rolInContextContextDef rid <|>
  unlessNull binnenRolContextDef rid <|>
  unlessNull buitenRolContextDef rid <|>
  -- unlessNull bindingDef rid <|>
  unlessNull gebruikerRolContextDef rid <|>
  contextBotContextDef rid

-- | Get the ContextDef that holds the RolDef in the given Rol.
rolDef2ContextDef :: forall e. RolDef -> (RolDef ~~> ContextDef) e
rolDef2ContextDef rd = unwrap >>> buitenRol /-/ (getGebondenAls rd :: (BuitenRol ~~> ContextRol) e) /-/ context >=> pure <<< map wrap

-- | The type of Rol or Context that can be bound to the Rol.
-- | `psp:Rol -> psp:Context | psp:Rol`
bindingDef :: forall e. (ContextDef ~~> AnyContext) e
bindingDef = unwrap >>> getContextRol (RolDef "model:Perspectives$Rol$mogelijkeBinding") /-/ binding /-/ context

-- | The Context of the RolInContext.
-- | `psp:Rol -> psp:Context`
rolInContextContextDef :: forall e. (RolDef ~~> ContextDef) e
rolInContextContextDef = rolDef2ContextDef (RolDef "model:Perspectives$Context$rolInContext")
-- rolInContextContextDef = unwrap >>> buitenRol /-/ (getGebondenAls (RolDef "model:Perspectives$Context$rolInContext") :: (BuitenRol ~~> ContextRol) e) /-/ context >=> pure <<< map wrap

-- | The Context of the gebruikerRol.
-- | `psp:Rol -> psp:Context`
gebruikerRolContextDef :: forall e. (RolDef ~~> ContextDef) e
gebruikerRolContextDef = rolDef2ContextDef (RolDef "model:Perspectives$Context$gebruikerRol")
-- gebruikerRolContextDef = buitenRol /-/ getGebondenAls "model:Perspectives$Context$gebruikerRol" /-/ context

-- | The Context of the contextBot.
-- | `psp:Rol -> psp:Context`
contextBotContextDef :: forall e. (RolDef ~~> ContextDef) e
contextBotContextDef = rolDef2ContextDef (RolDef "model:Perspectives$Context$contextBot")
-- contextBotContextDef = buitenRol /-/ getGebondenAls "model:Perspectives$Context$contextBot" /-/ context

-- | The Context of the BinnenRol.
-- | `psp:Rol -> psp:Context`
binnenRolContextDef :: forall e. (RolDef ~~> ContextDef) e
binnenRolContextDef = rolDef2ContextDef (RolDef "model:Perspectives$Context$binnenRolBeschrijving")
-- binnenRolContextDef = buitenRol /-/ getGebondenAls "model:Perspectives$Context$binnenRolBeschrijving" /-/ context

-- | The Context of the buitenRolBeschrijving. I.e. starting from a Context that is a BuitenRolBeschrijving, returns
-- | the Context that describes the type that the BuitenRolBeschrijving belongs to.
-- | `psp:Context -> psp:Context`
buitenRolContextDef :: forall e. (RolDef ~~> ContextDef) e
buitenRolContextDef = rolDef2ContextDef (RolDef "model:Perspectives$Context$buitenRolBeschrijving")
-- buitenRolContextDef = buitenRol /-/ getGebondenAls "model:Perspectives$Context$buitenRolBeschrijving" /-/ context
