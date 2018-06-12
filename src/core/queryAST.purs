module Perspectives.QueryAST where

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
