module Perspectives.QueryAST where

data QueryStep
  = Filter QueryStep QueryStep
  | Concat (Array QueryStep)
  | Compose (Array QueryStep)
  | NotEmpty QueryStep
  | Closure QueryStep
  | Closure' QueryStep
  | LastElement QueryStep
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
