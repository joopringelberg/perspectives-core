module Perspectives.QueryAST where

data QueryStep
  = Filter QueryStep QueryStep
  | Concat QueryStep QueryStep
  | Compose ElementaryQueryStep QueryStep
  | NotEmpty QueryStep
  | Closure QueryStep
  | Closure' QueryStep
  | LastElement QueryStep
  | Contains String QueryStep
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
  | Constant String
  | RolesOf String
  | Variable String
  | Binding
  | Context
  | UseCache
  | IgnoreCache
  | Identity
  | Type
  | BuitenRol
  | IedereRolInContext
  | RolTypen
  | Label
