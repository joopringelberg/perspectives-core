module Perspectives.ModelBasedObjectGetters where

import Perspectives.CoreTypes (ObjectsGetter)
import Perspectives.ObjectGetterConstructors (booleanPropertyGetter, getRol)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.SystemObjectGetters (getRolBinding, getRolContext)

-- | Equal to the 'own' $isVerplicht value; otherwise the logical or of the #aspectProperty values.
propertyIsVerplicht :: forall e. ObjectsGetter e
propertyIsVerplicht = booleanPropertyGetter "model:Perspectives$Context$aspectProperty"
  "model:Perspectives$Property$isVerplicht"

-- | Equal to the 'own' $isFunctioneel value; otherwise the logical or of the #aspectProperty values.
propertyIsFunctioneel :: forall e. ObjectsGetter e
propertyIsFunctioneel = booleanPropertyGetter "model:Perspectives$Context$aspectProperty" "model:Perspectives$Property$isFunctioneel"

-- | Equal to the 'own' $isVerplicht value; otherwise the logical or of the #aspectProperty values.
rolIsVerplicht :: forall e. ObjectsGetter e
rolIsVerplicht = booleanPropertyGetter "model:Perspectives$Context$aspect"
  "model:Perspectives$Rol$isVerplicht"

-- | Equal to the 'own' $isVerplicht value; otherwise the logical or of the #aspectProperty values.
rolIsFunctioneel :: forall e. ObjectsGetter e
rolIsFunctioneel = booleanPropertyGetter "model:Perspectives$Context$aspect"
  "model:Perspectives$Rol$isFunctioneel"

-- TODO. Dit is een functie naar voorbeeld van een ModelBasedTripleGetter.
-- | The type of the range that has been defined for the Property.
-- | `psp:Property -> psp:SimpleValue`
range :: forall e. ObjectsGetter e
range = getRol "model:Perspectives$Property$range" /-/ getRolBinding /-/ getRolContext

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
