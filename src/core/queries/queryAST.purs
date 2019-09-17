module Perspectives.QueryAST where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Perspectives.EntiteitAndRDFAliases (ContextID)

-- TODO: positie in de oorspronkelijke tekst toevoegen.

data QueryStep
  = Compose QueryStep QueryStep
  | Terminal ElementaryQueryStep
  | Filter QueryStep QueryStep
  | Disjunction QueryStep QueryStep

  | Conjunction QueryStep QueryStep

  | NotEmpty QueryStep
  | Closure QueryStep
  | Closure' QueryStep
  | LastElement QueryStep
  | UseCache QueryStep
  | IgnoreCache QueryStep
  | Contains QueryStep QueryStep
  | SetVariable String QueryStep

data ElementaryQueryStep
  = QualifiedRole String
  | UnqualifiedRole String
  | QualifiedProperty String
  | UnqualifiedProperty String
  | QualifiedExternalProperty String
  | UnqualifiedExternalProperty String
  | Binding
  | Context
  | ExternalRole

  | Constant ContextID String -- ContextID identifies the type of the constant.
  | RolesOf String
  | Variable String
  | Identity
  | Type
  | IedereRolInContext
  | RolTypen
  | Label

derive instance genericRepQueryStep :: Generic QueryStep _

instance showQueryStep :: Show QueryStep where
  show s = genericShow s

derive instance genericRepElementaryQueryStep :: Generic ElementaryQueryStep _

instance showElementaryQueryStep :: Show ElementaryQueryStep where
  show = genericShow
