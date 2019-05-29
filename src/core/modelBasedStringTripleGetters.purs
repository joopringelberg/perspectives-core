module Perspectives.ModelBasedStringTripleGetters where

import Control.Alt ((<|>))
import Perspectives.CoreTypes (type (**>), TripleGetter, TypedTripleGetter(..), (@@))
import Perspectives.DataTypeTripleGetters (genericBinding, genericContext)
import Perspectives.EntiteitAndRDFAliases (RolName)
import Perspectives.Identifiers (LocalName)
import Perspectives.ModelBasedTripleGetters (hasContextType, hasRolType, isOrHasAspect, sumToSequence)
import Perspectives.PerspectivesTypes (class RolClass, AnyDefinition, PBool)
import Perspectives.QueryCombinators (notEmpty, conj) as QC
import Perspectives.StringTripleGetterConstructors (searchInAspectRolesAndPrototypes, searchRolInContext, searchUnqualifiedRolDefinition)
import Perspectives.TripleGetterComposition (unlessFalse, (>->))
import Perspectives.TripleGetterConstructors (all)
import Perspectives.TripleGetterConstructors (unlessFalse) as TGC
import Prelude ((<>))

-- | allowedBinding `hasContextTypeOnEachRolTelescopeOf` boundValue
-- | allowedBinding ## (`hasContextTypeOnEachRolTelescopeOf` boundValue) !!!OMGEKEERD
-- | Means (i.e. this is how it is implemented):
-- | On each path through the mogelijkeBinding graph of allowedBinding there is a type x for which holds:
-- | x isContextTypeOf boundValue, or:
-- | boundValue isContextTypeOf x
-- | Or: boundValue is on each rolTelescope that starts with allowedBinding
-- | Formulated this way, 'hasContextTypeOnEachRolTelescopeOf' has it backwards.
hasContextTypeOnEachRolTelescopeOf :: String -> (String **> PBool)
hasContextTypeOnEachRolTelescopeOf boundValue = ((hasContextType boundValue) `unlessFalse`
  \_ ->
    (QC.conj
      (QC.notEmpty (mogelijkeBinding >-> sumToSequence))
      (all (mogelijkeBinding >-> sumToSequence >-> (hasContextTypeOnEachRolTelescopeOf boundValue)))))
  "hasContextTypeOnEachRolTelescopeOf"

hasRolTypeOnEachRolTelescopeOf :: forall r. RolClass r => r -> (String **> PBool)
hasRolTypeOnEachRolTelescopeOf boundValue = ((hasRolType boundValue) `unlessFalse`
  \_ ->
    (QC.conj
      (QC.notEmpty (mogelijkeBinding >-> sumToSequence))
      (all (mogelijkeBinding >-> sumToSequence >-> (hasRolTypeOnEachRolTelescopeOf boundValue)))))
  "hasRolTypeOnEachRolTelescopeOf"

-- TODO. Bij updates wordt de berekende waarheidswaarde niet (altijd) aangepast.
-- | aspect ## (isSubsumedOnEachRolTelescopeOf allowedBinding)
isSubsumedOnEachRolTelescopeOf :: String -> (String **> PBool)
isSubsumedOnEachRolTelescopeOf allowedBinding = TypedTripleGetter ("isSubsumedOnEachRolTelescopeOf" <> allowedBinding) f
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

-- | Given a local name for a View, looks for a matching View in the Rol definition,
-- | in its prototypes and in its AspectRoles.
searchView :: LocalName -> (RolName **> AnyDefinition)
searchView lvn = searchUnqualifiedRolDefinition lvn
