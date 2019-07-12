module Perspectives.Syntax (module Perspectives.InstanceRepresentation, module Perspectives.Syntax) where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Perspectives.Identifiers (QualifiedName, PEIdentifier)
import Perspectives.InstanceRepresentation
import Perspectives.EntiteitAndRDFAliases (Comment)
import Prelude (class Show)

-----------------------------------------------------------
-- CONTEXTDECLARATION, ENCLOSINGCONTEXTDECLARATION
-----------------------------------------------------------
data ContextDeclaration = ContextDeclaration QualifiedName QualifiedName (Array Comment)

derive instance genericContextDeclaration :: Generic ContextDeclaration _

instance showContextDeclaration :: Show ContextDeclaration where
  show = genericShow

data EnclosingContextDeclaration = EnclosingContextDeclaration PEIdentifier (Array Comment)

derive instance genericEnclosingContextDeclaration :: Generic EnclosingContextDeclaration _

instance showEnclosingContextDeclaration :: Show EnclosingContextDeclaration where
  show = genericShow
