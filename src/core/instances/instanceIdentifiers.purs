module Perspectives.Representation.InstanceIdentifiers where

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Prelude (class Eq, class Ord, class Show, compare, show, (<<<), (==))
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype ContextInstance = ContextInstance String
derive instance newtypeContextInstance :: Newtype ContextInstance _
derive instance genericRepContextInstance :: Generic ContextInstance _
derive newtype instance writeForeignContextInstance :: WriteForeign ContextInstance
derive newtype instance readForeignContextInstance :: ReadForeign ContextInstance
instance showContextInstance :: Show ContextInstance where
  show = show <<< unwrap
instance eqContextInstance :: Eq ContextInstance where
  eq (ContextInstance id1) (ContextInstance id2) = id1 == id2

newtype RoleInstance = RoleInstance String
derive instance newtypeRoleInstance :: Newtype RoleInstance _
derive instance genericRepRoleInstance :: Generic RoleInstance _
derive newtype instance writeForeignRoleInstance :: WriteForeign RoleInstance
derive newtype instance readForeignRoleInstance :: ReadForeign RoleInstance
instance showRoleInstance :: Show RoleInstance where
  show = show <<< unwrap
instance eqRoleInstance :: Eq RoleInstance where
  eq (RoleInstance id1) (RoleInstance id2) = id1 == id2
instance orRoleInstance :: Ord RoleInstance where
  compare (RoleInstance a) (RoleInstance b) = compare a b

newtype Value = Value String
derive instance newtypeValue :: Newtype Value _
derive instance genericRepValue :: Generic Value _
derive newtype instance writeForeignValue :: WriteForeign Value
derive newtype instance readForeignValue :: ReadForeign Value
instance showValue :: Show Value where
  show = show <<< unwrap
instance eqValue :: Eq Value where
  eq (Value id1) (Value id2) = id1 == id2
