module Perspectives.ModelBasedStringTripleGetters where

import Control.Alt ((<|>))
import Perspectives.CoreTypes (type (**>), TripleGetter, TypedTripleGetter(..), (@@))
import Perspectives.DataTypeTripleGetters (genericBinding, genericContext, genericRolBindingDef)
import Perspectives.EntiteitAndRDFAliases (RolName, PropertyName)
import Perspectives.Identifiers (LocalName)
import Perspectives.ModelBasedTripleGetters (hasContextType, hasRolType, isOrHasAspect, sumToSequence)
import Perspectives.PerspectivesTypes (class RolClass, AnyDefinition, PBool)
import Perspectives.QueryCombinators (notEmpty, conj) as QC
import Perspectives.StringTripleGetterConstructors (concat, directAspectRoles, getContextRol, getPrototype, searchGeneralUnqualifiedRolDefinition, searchInAspectRolesAndPrototypes, searchRolInContext, searchUnqualifiedRolDefinition)
import Perspectives.TripleGetterComposition (lazyUnionOfTripleObjects, unlessFalse, (>->))
import Perspectives.TripleGetterConstructors (all, closure_)
import Perspectives.TripleGetterConstructors (unlessFalse) as TGC
import Prelude ((<>), ($))

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
    f aspect = TGC.unlessFalse (isOrHasAspect allowedBinding) aspect
      -- this is: aspect ## (isOrHasAspect allowedBinding)
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
searchView = searchGeneralUnqualifiedRolDefinition "viewInRol"

-- | All properties defined in the namespace of the Rol.
ownPropertiesDef :: (RolName **> PropertyName)
ownPropertiesDef = (getContextRol "model:Perspectives$Rol$rolProperty") >-> genericRolBindingDef

-- | All properties defined for the Rol, in the same namespace, or on aspects, or on the MogelijkeBinding. Note that some of these may be an AspectProperty of others.
-- Test.Perspectives.ModelBasedTripleGetters
propertiesDef :: (RolName **> PropertyName)
propertiesDef = concat defsInAspectsAndPrototypes defsInMogelijkeBinding
  where

  defsInAspectsAndPrototypes :: (RolName **> PropertyName)
  defsInAspectsAndPrototypes = closure_ directAspectRoles >-> (closure_ getPrototype) >-> ownPropertiesDef

  defsInMogelijkeBinding :: (PropertyName **> PropertyName)
  defsInMogelijkeBinding = lazyUnionOfTripleObjects
    (mogelijkeBinding >-> sumToSequence)
    (\_ -> propertiesDef)
    "propertiesDef"
