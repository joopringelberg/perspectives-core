module Perspectives.QueryAST where

-- | This module gives an Abstract Syntax Tree representation for querypaths. A querypath is an expression that
-- | starts at a Context or Role and ends at a Context, Role or a value for a Property.
-- | The `Perspectives.Query.DescriptionCompiler` turns such an AST into a QueryFunctionDescription. Such a description
-- | consists of the origin (Domain) and destination of the querypath, and a description of the function that computes
-- | the destination from the origin.

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Perspectives.EntiteitAndRDFAliases (ContextID)

-- TODO: positie in de oorspronkelijke tekst toevoegen.

data QueryStep
  = Compose QueryStep QueryStep
  | Terminal ElementaryQueryStep
  | Filter QueryStep QueryStep
  | Disjunction QueryStep QueryStep
  | Conjunction QueryStep QueryStep

  -- To be implemented.
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

  -- To be implemented.
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

instance eqElementaryQueryStep :: Eq ElementaryQueryStep where
  eq = genericEq

instance eqQueryStep :: Eq QueryStep where
  eq e1 e2 = genericEq e1 e2
