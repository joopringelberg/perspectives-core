module Perspectives.Syntax where

-- Dit is de compiler target.

import Prelude
import Data.List (List)
import Data.StrMap (StrMap)

data Expr = Ctxt Context
          | Rol Rol

data Context1 = Context1 { id :: String
                    , context_BinnenRol :: Rol
                    , context_BuitenRol :: Rol
                    , rollen :: StrMap Rol }

newtype ContextDefinition = ContextDefinition
  { id :: String
  , contextType :: String
  , privateProperties :: List PropertyAssignment
  , publicProperties :: List PropertyAssignment
  , roles :: List RolAssignmentWithPropertyAssignments
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

instance showPropertyAssignment :: Show PropertyAssignment where
  show (PropertyAssignment{name, value}) = show name <> " = " <> show value

-- rolAssignment = type '=>' identifier
newtype RolAssignment = RolAssignment {name :: String, value :: String}

-- rolAssignment = type '=>' identifier BLOCK propertyAssignment*
newtype RolAssignmentWithPropertyAssignments = RolAssignmentWithPropertyAssignments
  {name :: String, binding :: String, properties :: List PropertyAssignment}

newtype PropertyDefinition = PropertyDefinition
  { scope :: String
  , name :: String
  , properties :: List PropertyAssignment}

instance showSimpleValue :: Show SimpleValue where
  show (String s) = show s
  show (Int i) = show i
  show (Bool b) = show b

instance showRolAssignment :: Show RolAssignment where
  show (RolAssignment{name, value}) = show name <> " = " <> show value

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
    "\nScope: " <> scope <>
    "\nID: " <> name <>
    "\nProperties:\n" <> show properties
