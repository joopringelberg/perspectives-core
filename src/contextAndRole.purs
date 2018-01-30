module Perspectives.ContextAndRole where

import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Maybe (Maybe(..))
import Data.Ord (Ordering, compare)
import Data.StrMap (StrMap, empty)
import Perspectives.Syntax (Comments(..), ContextRecord, ID, PerspectContext(..), PerspectRol(..), PropertyValueWithComments, Revision, RolRecord, noRevision)

-- CONTEXT

foreign import context_id :: PerspectContext -> ID

foreign import context_rev_aux :: forall a. Maybe a -> PerspectContext -> Maybe String

context_rev :: PerspectContext -> Maybe String
context_rev = context_rev_aux Nothing

context_rev' :: PerspectContext -> Revision
context_rev' (PerspectContext c) = c._rev

foreign import context_displayName :: PerspectContext -> String

foreign import context_pspType :: PerspectContext -> ID

foreign import context_binnenRol :: PerspectContext -> PerspectRol

foreign import context_buitenRol :: PerspectContext -> ID

foreign import context_rolInContext :: PerspectContext -> StrMap (Array ID)

foreign import context_comments :: PerspectContext -> Comments

defaultContextRecord :: ContextRecord
defaultContextRecord =
  { _id: ""
  , _rev: noRevision
  , displayName: ""
  , pspType: ""
  , binnenRol: PerspectRol defaultRolRecord
  , buitenRol: ""
  , rolInContext: empty
  , comments: defaultComments
  }

defaultComments :: Comments
defaultComments = Comments { commentBefore: [], commentAfter: []}

defaultRolRecord :: RolRecord
defaultRolRecord =
  { _id: ""
  , pspType: ""
  , context: ""
  , _rev: noRevision
  , binding: NullOrUndefined Nothing
  , properties: empty
  , gevuldeRollen: empty
  , occurrence: 0
  , comments: defaultComments
  }

-- ROL

foreign import rol_id :: PerspectRol -> ID

foreign import rol_rev_aux :: forall a. Maybe a -> PerspectRol -> Maybe String

rol_rev :: PerspectRol -> Maybe String
rol_rev = rol_rev_aux Nothing

rol_rev' :: PerspectRol -> Revision
rol_rev' (PerspectRol r)= r._rev

foreign import rol_occurrence :: PerspectRol -> Int

foreign import rol_pspType :: PerspectRol -> ID

foreign import rol_binding_aux :: forall a. Maybe a -> PerspectRol -> Maybe ID

rol_binding :: PerspectRol -> Maybe ID
rol_binding = rol_binding_aux Nothing

foreign import rol_context :: PerspectRol -> ID

foreign import rol_properties :: PerspectRol -> StrMap PropertyValueWithComments

foreign import rol_gevuldeRollen :: PerspectRol -> StrMap (Array ID)

foreign import rol_comments :: PerspectRol -> Comments

compareOccurrences :: PerspectRol -> PerspectRol -> Ordering
compareOccurrences a b = compare (rol_occurrence a) (rol_occurrence b)
