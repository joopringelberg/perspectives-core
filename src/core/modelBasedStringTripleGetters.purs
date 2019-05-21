module Perspectives.ModelBasedStringTripleGetters where

import Control.Alt ((<|>))
import Perspectives.CoreTypes (type (**>), TripleGetter, TypedTripleGetter(..), (@@))
import Perspectives.DataTypeTripleGetters (genericBinding, genericContext)
import Perspectives.EntiteitAndRDFAliases (RolName)
import Perspectives.ModelBasedTripleGetters (isContextTypeOf, isOrHasAspect, isRolTypeOf, sumToSequence)
import Perspectives.PerspectivesTypes (AnyDefinition, PBool, RolInContext)
import Perspectives.QueryCombinators (notEmpty, conj) as QC
import Perspectives.StringTripleGetterConstructors (searchInAspectRolesAndPrototypes, searchRolInContext)
import Perspectives.TripleGetterConstructors (unlessFalse) as TGC
import Perspectives.TripleGetterComposition (unlessFalse, (>->))
import Perspectives.TripleGetterConstructors (all)
import Prelude (const, (<>))

-- | allowedBinding `hasOnEachRolTelescopeTheContextTypeOf` boundValue
-- | allowedBinding `hasOnEachRolTelescopeTheContextTypeOf` boundValue
-- | allowedBinding ## (`hasOnEachRolTelescopeTheContextTypeOf` boundValue)
-- | Means (i.e. this is how it is implemented):
-- | On each path through the mogelijkeBinding graph of allowedBinding there is a type x for which holds:
-- | x isContextTypeOf boundValue, or:
-- | boundValue hasType x
-- | Or: boundValue is on each rolTelescope that starts with allowedBinding
-- | Formulated this way, 'hasOnEachRolTelescopeTheContextTypeOf' has it backwards.
hasOnEachRolTelescopeTheContextTypeOf :: String -> (String **> PBool)
hasOnEachRolTelescopeTheContextTypeOf boundValue = ((isContextTypeOf boundValue) `unlessFalse`
  const
    (QC.conj
      (QC.notEmpty (mogelijkeBinding >-> sumToSequence))
      (all (mogelijkeBinding >-> sumToSequence >-> (hasOnEachRolTelescopeTheContextTypeOf boundValue)))))
  "hasOnEachRolTelescopeTheContextTypeOf"

hasOnEachRolTelescopeTheRolTypeOf :: RolInContext -> (String **> PBool)
hasOnEachRolTelescopeTheRolTypeOf boundValue = ((isRolTypeOf boundValue) `unlessFalse`
  const
    (QC.conj
      (QC.notEmpty (mogelijkeBinding >-> sumToSequence))
      (all (mogelijkeBinding >-> sumToSequence >-> (hasOnEachRolTelescopeTheRolTypeOf boundValue)))))
  "hasOnEachRolTelescopeTheContextTypeOf"

-- TODO. Bij updates wordt de berekende waarheidswaarde niet (altijd) aangepast.
-- | aspect ## (isSubsumedOnEachRolTelescopeOf allowedBinding)
isSubsumedOnEachRolTelescopeOf :: String -> (String **> PBool)
isSubsumedOnEachRolTelescopeOf allowedBinding = TypedTripleGetter ("hasOnEachRolTelescopeTheContextTypeOf_" <> allowedBinding) f
  where
    f :: TripleGetter String PBool
    f aspect = TGC.unlessFalse (isOrHasAspect aspect) allowedBinding
      -- this is: allowedBinding ## (isOrHasAspect aspect)
      -- read as: allowedBinding `isOrHasAspect` aspect
      <|>
      (allowedBinding @@
        (QC.conj
          (QC.notEmpty (mogelijkeBinding >-> sumToSequence))
          (all (mogelijkeBinding >-> sumToSequence >-> (isSubsumedOnEachRolTelescopeOf aspect)))))

-- | The type of Rol or Context that can be bound to the Rol, taken
-- | from the RolDef itself or any aspectRol or prototype.
-- | `psp:Rol -> psp:Context | psp:Rol`
mogelijkeBinding :: (RolName **> AnyDefinition)
mogelijkeBinding = searchInAspectRolesAndPrototypes f
  where

    f :: (String **> AnyDefinition)
    f = searchRolInContext "model:Perspectives$Rol$mogelijkeBinding" >-> genericBinding >-> genericContext
