module Perspectives.ModelBasedStringTripleGetters where

import Control.Alt ((<|>))
import Perspectives.CoreTypes (type (**>), TripleGetter, TypedTripleGetter(..), (@@))
import Perspectives.DataTypeTripleGetters (genericBinding, genericContext)
import Perspectives.EntiteitAndRDFAliases (RolName)
import Perspectives.ModelBasedTripleGetters (isContextTypeOf, isOrHasAspect, sumToSequence)
import Perspectives.PerspectivesTypes (PBool, AnyDefinition)
import Perspectives.QueryCombinators (notEmpty, conj) as QC
import Perspectives.StringTripleGetterConstructors (searchInAspectRolesAndPrototypes, searchRolInContext)
import Perspectives.TripleGetterComposition ((>->))
import Perspectives.TripleGetterConstructors (unlessFalse, all)
import Prelude ((<>))

-- | allowedBinding `hasOnEachRolTelescopeTheTypeOf` t
-- | allowedBinding `hasOnEachRolTelescopeTheTypeOf` t
-- | allowedBinding ## (`hasOnEachRolTelescopeTheTypeOf` t)
-- | Means (i.e. this is how it is implemented):
-- | On each path through the mogelijkeBinding graph of allowedBinding there is a type x for which holds:
-- | x isContextTypeOf t, or:
-- | t hasType x
-- | Or: t is on each rolTelescope that starts with allowedBinding
-- | Formulated this way, 'hasOnEachRolTelescopeTheTypeOf' has it backwards.
hasOnEachRolTelescopeTheTypeOf :: forall e. String -> (String **> PBool) e
hasOnEachRolTelescopeTheTypeOf t = TypedTripleGetter ("hasOnEachRolTelescopeTheTypeOf_" <> t) f
  where
    f :: TripleGetter String PBool e
    f allowedBinding = unlessFalse (isContextTypeOf t) allowedBinding
      -- this is: allowedBinding ## (isContextTypeOf t)
      -- read as: allowedBinding `isContextTypeOf` t
      -- or: t `hasType` allowedBinding
      <|>
      (allowedBinding @@
        (QC.conj
          (QC.notEmpty (mogelijkeBinding >-> sumToSequence))
          (all (mogelijkeBinding >-> sumToSequence >-> (hasOnEachRolTelescopeTheTypeOf t)))))

-- | aspect ## (isSubsumedOnEachRolTelescopeOf allowedBinding)
isSubsumedOnEachRolTelescopeOf :: forall e. String -> (String **> PBool) e
isSubsumedOnEachRolTelescopeOf allowedBinding = TypedTripleGetter ("hasOnEachRolTelescopeTheTypeOf_" <> allowedBinding) f
  where
    f :: TripleGetter String PBool e
    f aspect = unlessFalse (isOrHasAspect aspect) allowedBinding
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
mogelijkeBinding :: forall e. (RolName **> AnyDefinition) e
mogelijkeBinding = searchInAspectRolesAndPrototypes f
  where

    f :: (String **> AnyDefinition) e
    f = searchRolInContext "model:Perspectives$Rol$mogelijkeBinding" >-> genericBinding >-> genericContext
