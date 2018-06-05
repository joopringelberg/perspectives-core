module Perspectives.ModelBasedObjectGetters where

import Control.Alt ((<|>))
import Perspectives.CoreTypes (ObjectsGetter)
import Perspectives.DataTypeObjectGetters (buitenRol, binding, context)
import Perspectives.ObjectGetterConstructors (booleanPropertyGetter, getGebondenAls, getRol, getRolFromPrototypeHierarchy, unlessNull)
import Perspectives.ObjectsGetterComposition ((/-/))

-- | Equal to the 'own' $isVerplicht value; otherwise the logical or of the #aspectProperty values.
rolIsVerplicht :: forall e. ObjectsGetter e
rolIsVerplicht = booleanPropertyGetter "model:Perspectives$Context$aspect"
  "model:Perspectives$Rol$buitenRolBeschrijving$isVerplicht"

-- | Equal to the 'own' $isVerplicht value; otherwise the logical or of the #aspectProperty values.
rolIsFunctioneel :: forall e. ObjectsGetter e
rolIsFunctioneel = booleanPropertyGetter "model:Perspectives$Context$aspect"
  "model:Perspectives$Rol$buitenRolBeschrijving$isFunctioneel"

-- TODO. Dit is een functie naar voorbeeld van een ModelBasedTripleGetter.
-- | The type of the range that has been defined for the Property.
-- | `psp:Property -> psp:SimpleValue`
range :: forall e. ObjectsGetter e
range = getRol "model:Perspectives$Property$range" /-/ binding /-/ context

-- | Get the rol psp:Context$buitenRolBeschrijving.
-- | `psp:Context -> psp:RolInstance`
buitenRolBeschrijving :: forall e. ObjectsGetter e
buitenRolBeschrijving = getRol "model:Perspectives$Context$buitenRolBeschrijving"

-- | Get the Context that describes the buitenRol of a Context that is a definition. External properties of that Context
-- | are defined on that Rol.
-- | `psp:Context -> psp:RolInstance`
buitenRolBeschrijvingDef :: forall e. ObjectsGetter e
buitenRolBeschrijvingDef = getRolFromPrototypeHierarchy "model:Perspectives$Context$buitenRolBeschrijving" /-/ binding /-/ context

-- | Get the psp:Context$buitenRol of a Context that is a definition. External properties of that Context
-- | are defined on that Rol.
-- | `psp:Context -> psp:RolInstance`
binnenRolBeschrijving :: forall e. ObjectsGetter e
binnenRolBeschrijving = getRol "model:Perspectives$Context$binnenRolBeschrijving"

-- | `psp:Rol -> psp:Context`
contextDef :: forall e. ObjectsGetter e
contextDef rid = unlessNull rolInContextContextDef rid <|> unlessNull binnenRolContextDef rid <|> unlessNull buitenRolContextDef rid <|> bindingDef rid

-- | The type of Rol or Context that can be bound to the Rol.
-- | `psp:Rol -> psp:Context | psp:Rol`
bindingDef :: forall e. ObjectsGetter e
bindingDef = getRol "model:Perspectives$Rol$mogelijkeBinding" /-/ binding /-/ context

-- | The Context of the RolInContext.
-- | `psp:Rol -> psp:Context`
rolInContextContextDef :: forall e. ObjectsGetter e
rolInContextContextDef = buitenRol /-/ getGebondenAls "model:Perspectives$Context$rolInContext" /-/ context

-- | The Context of the BinnenRol.
-- | `psp:Rol -> psp:Context`
binnenRolContextDef :: forall e. ObjectsGetter e
binnenRolContextDef = buitenRol /-/ getGebondenAls "model:Perspectives$Context$binnenRolBeschrijving" /-/ context

-- | The Context of the buitenRolBeschrijving. I.e. starting from a Context that is a BuitenRolBeschrijving, returns
-- | the Context that describes the type that the BuitenRolBeschrijving belongs to.
-- | `psp:Context -> psp:Context`
buitenRolContextDef :: forall e. ObjectsGetter e
buitenRolContextDef = buitenRol /-/ getGebondenAls "model:Perspectives$Context$buitenRolBeschrijving" /-/ context

-- | Equal to the 'own' $isVerplicht value; otherwise the logical or of the #aspectProperty values.
propertyIsVerplicht :: forall e. ObjectsGetter e
propertyIsVerplicht = booleanPropertyGetter "model:Perspectives$Context$aspectProperty"
  "model:Perspectives$Property$buitenRolBeschrijving$isVerplicht"

-- | Equal to the 'own' $isFunctioneel value; otherwise the logical or of the #aspectProperty values.
propertyIsFunctioneel :: forall e. ObjectsGetter e
propertyIsFunctioneel = booleanPropertyGetter "model:Perspectives$Context$aspectProperty" "model:Perspectives$Property$buitenRolBeschrijving$isFunctioneel"
