module Perspectives.ModelBasedObjectGetters where

import Control.Alt ((<|>))
import Perspectives.CoreTypes (ObjectsGetter)
import Perspectives.ObjectGetterConstructors (booleanPropertyGetter, getGebondenAls, getRol, unlessNull)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.DataTypeObjectGetters (buitenRol, binding, getRolContext)

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
range = getRol "model:Perspectives$Property$range" /-/ binding /-/ getRolContext

-- | Get the psp:Context$buitenRol of a Context that is a definition. External properties of that Context
-- | are defined on that Rol.
-- | `psp:Context -> psp:RolInstance`
getBuitenRolBeschrijving :: forall e. ObjectsGetter e
getBuitenRolBeschrijving = getRol "model:Perspectives$Context$buitenRolBeschrijving"

-- | Get the psp:Context$buitenRol of a Context that is a definition. External properties of that Context
-- | are defined on that Rol.
-- | `psp:Context -> psp:RolInstance`
getBinnenRolBeschrijving :: forall e. ObjectsGetter e
getBinnenRolBeschrijving = getRol "model:Perspectives$Context$binnenRolBeschrijving"

-- | `psp:Rol -> psp:Context`
getContextDef :: forall e. ObjectsGetter e
getContextDef rid = unlessNull getRolInContextContextDef rid <|> unlessNull getBinnenRolContextDef rid <|> unlessNull getBuitenRolContextDef rid <|> getBindingDef rid

-- | The type of Rol or Context that can be bound to the Rol.
-- | `psp:Rol -> psp:Context | psp:Rol`
getBindingDef :: forall e. ObjectsGetter e
getBindingDef = getRol "model:Perspectives$Rol$mogelijkeBinding" /-/ binding /-/ getRolContext

-- | The Context of the RolInContext.
-- | `psp:Rol -> psp:Context`
getRolInContextContextDef :: forall e. ObjectsGetter e
getRolInContextContextDef = buitenRol /-/ getGebondenAls "model:Perspectives$Context$rolInContext" /-/ getRolContext

-- | The Context of the BinnenRol.
-- | `psp:Rol -> psp:Context`
getBinnenRolContextDef :: forall e. ObjectsGetter e
getBinnenRolContextDef = buitenRol /-/ getGebondenAls "model:Perspectives$Context$binnenRolBeschrijving" /-/ getRolContext

-- | The Context of the BuitenRol.
-- | `psp:Rol -> psp:Context`
getBuitenRolContextDef :: forall e. ObjectsGetter e
getBuitenRolContextDef = buitenRol /-/ getGebondenAls "model:Perspectives$Context$buitenRolBeschrijving" /-/ getRolContext

-- | Equal to the 'own' $isVerplicht value; otherwise the logical or of the #aspectProperty values.
propertyIsVerplicht :: forall e. ObjectsGetter e
propertyIsVerplicht = booleanPropertyGetter "model:Perspectives$Context$aspectProperty"
  "model:Perspectives$Property$buitenRolBeschrijving$isVerplicht"

-- | Equal to the 'own' $isFunctioneel value; otherwise the logical or of the #aspectProperty values.
propertyIsFunctioneel :: forall e. ObjectsGetter e
propertyIsFunctioneel = booleanPropertyGetter "model:Perspectives$Context$aspectProperty" "model:Perspectives$Property$buitenRolBeschrijving$isFunctioneel"
