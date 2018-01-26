module Perspectives.ContextAndRole where

import Data.Maybe (Maybe(..))
import Data.Ord (Ordering, compare)
import Data.StrMap (StrMap)
import Perspectives.Syntax (Comments, ID, PerspectContext, PerspectRol, PropertyValueWithComments)

-- CONTEXT

foreign import context_id :: PerspectContext -> ID

foreign import context_rev_aux :: forall a. Maybe a -> PerspectContext -> Maybe String

context_rev :: PerspectContext -> Maybe String
context_rev = context_rev_aux Nothing

foreign import context_displayName :: PerspectContext -> String

foreign import context_pspType :: PerspectContext -> ID

foreign import context_binnenRol :: PerspectContext -> PerspectRol

foreign import context_buitenRol :: PerspectContext -> ID

foreign import context_rolInContext :: PerspectContext -> StrMap (Array ID)

foreign import context_comments :: PerspectContext -> Comments ()

foreign import createPerspectContext :: forall a. {|a} -> PerspectContext

-- ROL

foreign import rol_id :: PerspectRol -> ID

foreign import rol_rev_aux :: forall a. Maybe a -> PerspectRol -> Maybe String

rol_rev :: PerspectRol -> Maybe String
rol_rev = rol_rev_aux Nothing

foreign import rol_occurrence :: PerspectRol -> Int

foreign import rol_pspType :: PerspectRol -> ID

foreign import rol_binding_aux :: forall a. Maybe a -> PerspectRol -> Maybe ID

rol_binding :: PerspectRol -> Maybe ID
rol_binding = rol_binding_aux Nothing

foreign import rol_context :: PerspectRol -> ID

foreign import rol_properties :: PerspectRol -> StrMap PropertyValueWithComments

foreign import rol_gevuldeRollen :: PerspectRol -> StrMap (Array ID)

foreign import rol_comments :: PerspectRol -> Comments ()

compareOccurrences :: PerspectRol -> PerspectRol -> Ordering
compareOccurrences a b = compare (rol_occurrence a) (rol_occurrence b)

foreign import createPerspectRol :: forall a. {|a} -> PerspectRol
