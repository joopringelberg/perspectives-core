module Perspectives.Syntax where

import Data.Maybe (Maybe)
import Data.StrMap (StrMap)
import Prelude (class Show, show, (<>))

type ID = String
type Comment = String

newtype Comments e = Comments
  { commentBefore :: Array Comment
  , commentAfter :: Array Comment
  | e}

newtype PerspectContext = PerspectContext PerspectContextProperties

type PerspectContextProperties =
  { id :: ID
  , displayName :: String
  , pspType :: ID
  , binnenRol :: BinnenRol -- TODO: vervang door PerspectRol, zodat de binnenrol ook kan vullen (b.v. rollen van Acties).
  , buitenRol :: ID
  , rolInContext :: StrMap (Array ID)
  , comments :: Comments ()
  }

newtype PerspectRol =
  PerspectRol PerspectRolProperties

type PerspectRolProperties =
    { id :: ID
    , occurrence :: Int
    , pspType :: ID
    , binding :: Maybe ID
    , context :: ID
    , properties :: StrMap PropertyValueWithComments
    , gevuldeRollen :: StrMap (Array ID)
    , comments :: Comments ()
    }

type PropertyValueWithComments = Comments (value :: Array String)

newtype BinnenRol =
  BinnenRol
    { id :: ID
    , pspType :: ID
    , binding :: Maybe ID
    , properties :: StrMap PropertyValueWithComments
    }

type PerspectName = String
type PropertyName = String
type RoleName = String

data EnclosingContextDeclaration = EnclosingContextDeclaration Expanded (Array Comment)

data ContextDeclaration = ContextDeclaration Expanded Expanded (Array Comment)

data SimpleValue =
    String String
  | Int Int
  | Bool Boolean
  -- en dan nog date

propertyValue :: PropertyValueWithComments -> Array String
propertyValue (Comments{value}) = value

-----------------------------------------------------------
-- ContextName
-----------------------------------------------------------
type Prefix = String
type LocalName = String
type DomeinName = String

data Expanded = Expanded DomeinName LocalName

-----------------------------------------------------------
-- Instances
-----------------------------------------------------------

foreign import jsonStringify :: forall a. {|a} -> String

instance showPerspectContext :: Show PerspectContext where
  show (PerspectContext r) = jsonStringify r

instance showPerspectRol :: Show PerspectRol where
  show (PerspectRol r) = jsonStringify r

instance showSimpleValue :: Show SimpleValue where
  show (String s) = show s
  show (Int i) = show i
  show (Bool b) = show b

instance showComments :: Show (Comments e) where
  show (Comments {commentBefore, commentAfter}) = "commentBefore: " <> show commentBefore <> "\ncommentAfter: " <> show commentAfter

instance showExpanded :: Show Expanded where
  show (Expanded domeinName localName) = domeinName <> localName
