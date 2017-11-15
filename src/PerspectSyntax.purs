module Perspectives.Syntax where

-- Dit is de compiler target.

import Prelude
import Data.List (List)
import Data.StrMap (StrMap)

data Expr = Ctxt Context
          | Rol Rol

newtype ContextDefinition = ContextDefinition
  { id :: String
  , contextType :: String
  , privateProperties :: List PropertyDefinition
  , publicProperties :: List PropertyDefinition
  , roles :: List RolDefinition
  }

newtype RolDefinition = RolDefinition
  { id :: String
  , rolType :: String
  , binding :: RolAssignment
  , properties :: List PropertyDefinition
  }

newtype Context = Context
  { id :: String
  , contextType :: String
  , properties :: List PropertyAssignment
  , roles :: List RolAssignmentWithPropertyAssignments
  }

type CommonRolMembers m =
  { id :: String
  , rol_Context :: Context
  , properties :: StrMap (Array Object)
  | m}

data Rol =  BinnenRol (CommonRolMembers (binding :: Rol))
          | BuitenRol (CommonRolMembers ())
          | RolInContext (CommonRolMembers (binding :: Rol))

type Object = String

data SimpleValue =
    String String
  | Int Int
  | Bool Boolean
  -- en dan nog date

-- propertyAssignment = type '=' simpleValue
newtype PropertyAssignment = PropertyAssignment {name :: String, op :: Unit, value :: SimpleValue}

-- rolAssignment = type '=>' identifier
newtype RolAssignment = RolAssignment {name :: String, binding :: String}

-- rolAssignment = type '=>' identifier BLOCK propertyAssignment*
newtype RolAssignmentWithPropertyAssignments = RolAssignmentWithPropertyAssignments
  {name :: String, binding :: String, properties :: List PropertyAssignment}

newtype PropertyDefinition = PropertyDefinition
  { scope :: String
  , name :: String
  , properties :: List PropertyAssignment}

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
  show (RolAssignmentWithPropertyAssignments{name, binding, properties}) = name <> " => " <> binding <> "\n" <> show properties

instance showContextDefinition :: Show ContextDefinition where
  show (ContextDefinition{id, contextType, privateProperties, publicProperties, roles }) =
    contextType <> " " <> id <>
      "\nprivate:\n" <> show privateProperties <>
      "\npublic:\n" <> show publicProperties <>
      "\nroles:\n" <> show roles

instance showContext :: Show Context where
  show (Context{id, contextType, properties, roles }) =
    "\nType: " <> contextType <>
    "\nID: " <> id <>
    "\nProperties:\n" <> show properties <>
    "\nRoles:\n" <> show roles

instance showPropertyDefinition :: Show PropertyDefinition where
  show (PropertyDefinition {scope, name, properties}) =
    "\n" <> scope <> " " <> name <>
    "\nProperties:\n" <> show properties

instance showPropertyAssignment :: Show PropertyAssignment where
  show (PropertyAssignment{name, value}) = show name <> " = " <> show value

instance showRolDefinition :: Show RolDefinition where
  show (RolDefinition{id, rolType, binding: (RolAssignment{binding: bnd}), properties}) =
    "\nType: " <> rolType <>
    "\nID: " <> id <>
    "\nBinding: " <> show bnd <>
    "\nProperties " <> show properties
