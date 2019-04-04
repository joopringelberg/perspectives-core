module Perspectives.ModelBasedStringTripleGetters where

import Control.Alt ((<|>))
import Perspectives.CoreTypes (type (**>), TripleGetter, TypedTripleGetter(..), (@@))
import Perspectives.DataTypeTripleGetters (genericBinding, genericContext)
import Perspectives.EntiteitAndRDFAliases (RolName)
import Perspectives.ModelBasedTripleGetters (isOrHasSuperType, sumToSequence)
import Perspectives.PerspectivesTypes (PBool, AnyDefinition)
import Perspectives.QueryCombinators (notEmpty, conj) as QC
import Perspectives.StringTripleGetterConstructors (searchInAspectRolesAndPrototypes, searchRolInContext)
import Perspectives.TripleGetterComposition ((>->))
import Perspectives.TripleGetterConstructors (unlessFalse, all)
import Prelude ((<>))

-- | allowedBinding `isInEachRolTelescope` t
-- | allowedBinding ## (`isInEachRolTelescope` t)
-- | Means (i.e. this is how it is implemented):
-- | On each path through the mogelijkeBinding graph of allowedBinding there is a type x for which holds:
-- | t isOrHasSuperType x
isInEachRolTelescope :: forall e. String -> (String **> PBool) e
isInEachRolTelescope t = TypedTripleGetter ("isInEachRolTelescope_" <> t) f
  where
    f :: TripleGetter String PBool e
    f allowedBinding = unlessFalse (isOrHasSuperType allowedBinding) t
      <|>
      (allowedBinding @@
        (QC.conj
          (QC.notEmpty (mogelijkeBinding >-> sumToSequence))
          (all (mogelijkeBinding >-> sumToSequence >-> (isInEachRolTelescope t)))))

-- | The type of Rol or Context that can be bound to the Rol, taken
-- | from the RolDef itself or any aspectRol or prototype.
-- | `psp:Rol -> psp:Context | psp:Rol`
mogelijkeBinding :: forall e. (RolName **> AnyDefinition) e
mogelijkeBinding = searchInAspectRolesAndPrototypes f
  where

    f :: (String **> AnyDefinition) e
    f = searchRolInContext "model:Perspectives$Rol$mogelijkeBinding" >-> genericBinding >-> genericContext
