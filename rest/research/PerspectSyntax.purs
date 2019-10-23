module Perspectives.Syntax where

-- Dit is de compiler target.

import Prelude
import Data.List (List)
import Perspectives.Syntax2 (EntityCollection)

data Expr = Ctxt EntityCollection
          | CtxtDef ContextDefinition

-----------------------------------------------------------
-- Definitions
-----------------------------------------------------------

newtype ContextDefinition = ContextDefinition
  { id :: String
  , contextType :: String
  , privateProperties :: List PropertyDefinition
  , publicProperties :: List PropertyDefinition
  , rolDefinitions :: List RolDefinition
  }

newtype RolDefinition = RolDefinition
  { id :: String                            -- id
  , rolType :: String                       -- ??
  , binding :: RolAssignment
  , properties :: List PropertyDefinition
  }

newtype PropertyDefinition = PropertyDefinition
  { scope :: String
  , name :: String
  , properties :: List PropertyAssignment}

-----------------------------------------------------------
-- Instances
-----------------------------------------------------------

data SimpleValue =
    String String
  | Int Int
  | Bool Boolean
  -- en dan nog date

-- propertyAssignment = type '=' simpleValue
newtype PropertyAssignment = PropertyAssignment {name :: String, scope :: String, value :: SimpleValue}

-- rolAssignment = type '=>' identifier
newtype RolAssignment = RolAssignment {name :: String, binding :: String}

-- rolAssignment = type '=>' identifier BLOCK propertyAssignment*
newtype RolAssignmentWithPropertyAssignments = RolAssignmentWithPropertyAssignments
  {rolInContextType :: String, binding :: String, properties :: List PropertyAssignment}

-----------------------------------------------------------
-- Show instances
-----------------------------------------------------------

instance showSimpleValue :: Show SimpleValue where
  show (String s) = show s
  show (Int i) = show i
  show (Bool b) = show b

instance showRolAssignment :: Show RolAssignment where
  show (RolAssignment{name, binding}) = show name <> " = " <> show binding

instance showRolAssignmentWithPropertyAssignments :: Show RolAssignmentWithPropertyAssignments where
  show (RolAssignmentWithPropertyAssignments{rolInContextType, binding, properties}) = rolInContextType <> " => " <> binding <> "\n" <> show properties

instance showContextDefinition :: Show ContextDefinition where
  show (ContextDefinition{id, contextType, privateProperties, publicProperties, rolDefinitions }) =
    contextType <> " " <> id <>
      "\nprivate:\n" <> show privateProperties <>
      "\npublic:\n" <> show publicProperties <>
      "\nroles:\n" <> show rolDefinitions

instance showPropertyDefinition :: Show PropertyDefinition where
  show (PropertyDefinition {scope, name, properties}) =
    "\n" <> scope <> " " <> name <>
    "\nProperties:\n" <> show properties

instance showPropertyAssignment :: Show PropertyAssignment where
  show (PropertyAssignment{name, scope, value}) = show scope <> ": " <> name <> " = " <> show value

instance showRolDefinition :: Show RolDefinition where
  show (RolDefinition{id, rolType, binding: (RolAssignment{binding: bnd}), properties}) =
    "\nType: " <> rolType <>
    "\nID: " <> id <>
    "\nBinding: " <> show bnd <>
    "\nProperties " <> show properties

instance showExpr :: Show Expr where
  show (Ctxt c) = show c
  show (CtxtDef d) = show d
