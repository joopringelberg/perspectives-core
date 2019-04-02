module Perspectives.ModelBasedStringTripleGetters where

import Control.Alt ((<|>))
import Perspectives.CoreTypes (type (**>), TripleGetter, TypedTripleGetter(..), (@@))
import Perspectives.DataTypeTripleGetters (genericBinding, genericContext)
import Perspectives.EntiteitAndRDFAliases (RolName)
import Perspectives.ModelBasedTripleGetters (sumToSequence)
import Perspectives.PerspectivesTypes (PBool, AnyDefinition)
import Perspectives.QueryCombinators (notEmpty, conj) as QC
import Perspectives.StringTripleGetterConstructors (searchInAspectRolesAndPrototypes, searchRolInContext)
import Perspectives.TripleGetterComposition ((>->))
import Perspectives.TripleGetterConstructors (agreesWithType, unlessFalse, all)
import Prelude ((<>))

-- | True iff t (the first parameter) either agrees with the head of the graph, or if it is in the rol telescope
-- | for each of its mogelijkeBindingen.
isInEachRolTelescope :: forall e. String -> (String **> PBool) e
isInEachRolTelescope t = TypedTripleGetter ("isInEachRolTelescope_" <> t) f
  where
    f :: TripleGetter String PBool e
    f headOfGraph = unlessFalse (agreesWithType t) headOfGraph
      <|>
      (headOfGraph @@
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
