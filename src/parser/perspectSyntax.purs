module Perspectives.Syntax where

import Data.List (List)
import Data.Maybe (Maybe)
import Data.StrMap (StrMap, values)
import Prelude (class Show, show, (<>))

data NamedEntityCollection = NamedEntityCollection PerspectName EntityCollection

newtype EntityCollection = EntityCollection (StrMap PerspectEntity)

type ID = String

data PerspectEntity = Context PerspectContext | Rol PerspectRol

type Comment = String

type OptionalComment = Maybe Comment

newtype Comments e = Comments
  { commentBefore :: Array Comment
  , commentAfter :: OptionalComment
  | e}

type PropertyComments = Comments ()
type ContextRoleComments = Comments (propertyComments :: StrMap (Comments ()))

newtype PerspectContext = PerspectContext
  { id :: ID
  , pspType :: ID
  , binnenRol :: BinnenRol
  , buitenRol :: ID
  , rolInContext :: Array ID
  , comments :: Maybe ContextRoleComments
  }

newtype PerspectRol =
  PerspectRol
    { id :: ID
    , pspType :: ID
    , binding :: Maybe ID
    , context :: ID
    , properties :: StrMap (Array String)
    , gevuldeRollen :: StrMap (Array ID)
    , comments :: Maybe ContextRoleComments
    }

newtype BinnenRol =
  BinnenRol
    { id :: ID
    , pspType :: ID
    , binding :: Maybe ID
    , properties :: StrMap (Array String)
    }

type PerspectName = String
type PropertyName = String
type RoleName = String

data TypeDeclaration = TypeDeclaration PerspectName PerspectName OptionalComment

data RolePropertyAssignment = RolePropertyAssignment PropertyName SimpleValue

type Scope = String
data ContextPropertyAssignment = ContextPropertyAssignment PropertyName Scope SimpleValue

data RolBinding = RolBinding RoleName PerspectName EntityCollection (List RolePropertyAssignment)

data SimpleValue =
    String String
  | Int Int
  | Bool Boolean
  -- en dan nog date


-----------------------------------------------------------
-- Show instances
-----------------------------------------------------------

foreign import jsonStringify :: forall a. {|a} -> String

instance showPerspectContext :: Show PerspectContext where
  show (PerspectContext r) = jsonStringify r

instance showNamedEntityCollection :: Show NamedEntityCollection where
  show (NamedEntityCollection name collection) = name <> ": \n" <> show collection

instance showEntityCollection :: Show EntityCollection where
  show (EntityCollection s) = show (values s)

instance showPerspectEntity :: Show PerspectEntity where
  show (Context ct) = show ct
  show (Rol r) = show r

instance showPerspectRol :: Show PerspectRol where
  show (PerspectRol r) = jsonStringify r

instance showSimpleValue :: Show SimpleValue where
  show (String s) = show s
  show (Int i) = show i
  show (Bool b) = show b

instance showTypeDeclaration :: Show TypeDeclaration where
  show (TypeDeclaration tp inst cmt) = tp <> "=" <> inst <> show cmt

instance showComments :: Show (Comments e) where
  show (Comments {commentBefore, commentAfter}) = "commentBefore: " <> show commentBefore <> "\ncommentAfter: " <> show commentAfter
