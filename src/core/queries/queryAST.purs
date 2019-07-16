module Perspectives.QueryAST where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Perspectives.EntiteitAndRDFAliases (ContextID)

-- TODO: positie in de oorspronkelijke tekst toevoegen.

data QueryStep
  = Filter QueryStep QueryStep
  | Concat (Array QueryStep)
  | Compose (Array QueryStep)
  | NotEmpty QueryStep
  | Closure QueryStep
  | Closure' QueryStep
  | LastElement QueryStep
  | UseCache QueryStep
  | IgnoreCache QueryStep
  | Contains QueryStep QueryStep
  | SetVariable String QueryStep
  | Terminal ElementaryQueryStep

data ElementaryQueryStep
  = UnqualifiedRol String
  | QualifiedRol String
  | UnqualifiedProperty String
  | QualifiedProperty String
  | UnqualifiedExternalProperty String
  | QualifiedExternalProperty String
  | UnqualifiedInternalProperty String
  | QualifiedInternalProperty String
  | Constant ContextID String -- ContextID identifies the type of the constant.
  | RolesOf String
  | Variable String
  | Binding
  | Context
  | Identity
  | Type
  | BuitenRol
  | IedereRolInContext
  | RolTypen
  | Label

derive instance genericRepQueryStep :: Generic QueryStep _

instance showQueryStep :: Show QueryStep where
  show s = genericShow s

derive instance genericRepElementaryQueryStep :: Generic ElementaryQueryStep _

instance showElementaryQueryStep :: Show ElementaryQueryStep where
  show = genericShow
