module Perspectives.ModelBasedStringTripleGetters where

import Control.Alt ((<|>))
import Data.Newtype (unwrap)
import Perspectives.CoreTypes (type (**>), TripleGetter, TypedTripleGetter(..), (@@))
import Perspectives.DataTypeTripleGetters (genericBinding, genericContext)
import Perspectives.EntiteitAndRDFAliases (RolName)
import Perspectives.ModelBasedTripleGetters (isContextTypeOf, isOrHasAspect, isRolTypeOf, sumToSequence)
import Perspectives.PerspectivesTypes (AnyDefinition, PBool, RolInContext)
import Perspectives.QueryCombinators (notEmpty, conj) as QC
import Perspectives.StringTripleGetterConstructors (searchInAspectRolesAndPrototypes, searchRolInContext)
import Perspectives.TripleGetterComposition ((>->))
import Perspectives.TripleGetterConstructors (unlessFalse, all)
import Prelude ((<>))

-- | allowedBinding `hasOnEachRolTelescopeTheContextTypeOf` boundValue
-- | allowedBinding `hasOnEachRolTelescopeTheContextTypeOf` boundValue
-- | allowedBinding ## (`hasOnEachRolTelescopeTheContextTypeOf` boundValue)
-- | Means (i.e. this is how it is implemented):
-- | On each path through the mogelijkeBinding graph of allowedBinding there is a type x for which holds:
-- | x isContextTypeOf boundValue, or:
-- | boundValue hasType x
-- | Or: boundValue is on each rolTelescope that starts with allowedBinding
-- | Formulated this way, 'hasOnEachRolTelescopeTheContextTypeOf' has it backwards.
hasOnEachRolTelescopeTheContextTypeOf :: forall e. String -> (String **> PBool) e
hasOnEachRolTelescopeTheContextTypeOf boundValue = TypedTripleGetter ("hasOnEachRolTelescopeTheContextTypeOf_" <> boundValue) f
  where
    f :: TripleGetter String PBool e
    f allowedBinding = unlessFalse (isContextTypeOf boundValue) allowedBinding
      -- this is: allowedBinding ## (isContextTypeOf boundValue)
      -- read as: allowedBinding `isContextTypeOf` boundValue
      -- or: boundValue `hasType` allowedBinding
      <|>
      (allowedBinding @@
        (QC.conj
          (QC.notEmpty (mogelijkeBinding >-> sumToSequence))
          (all (mogelijkeBinding >-> sumToSequence >-> (hasOnEachRolTelescopeTheContextTypeOf boundValue)))))

hasOnEachRolTelescopeTheRolTypeOf :: forall e. RolInContext -> (String **> PBool) e
hasOnEachRolTelescopeTheRolTypeOf boundValue = TypedTripleGetter ("hasOnEachRolTelescopeTheContextTypeOf_" <> (unwrap boundValue)) f
  where
    f :: TripleGetter String PBool e
    f allowedBinding = unlessFalse (isRolTypeOf boundValue) allowedBinding
      <|>
      (allowedBinding @@
        (QC.conj
          (QC.notEmpty (mogelijkeBinding >-> sumToSequence))
          (all (mogelijkeBinding >-> sumToSequence >-> (hasOnEachRolTelescopeTheRolTypeOf boundValue)))))

-- | aspect ## (isSubsumedOnEachRolTelescopeOf allowedBinding)
isSubsumedOnEachRolTelescopeOf :: forall e. String -> (String **> PBool) e
isSubsumedOnEachRolTelescopeOf allowedBinding = TypedTripleGetter ("hasOnEachRolTelescopeTheContextTypeOf_" <> allowedBinding) f
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
