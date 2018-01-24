module Perspectives.ContextAndRole where

import Control.Monad.Aff (Aff)
import Control.Monad.ST (ST)
import Data.Maybe (Maybe(..), fromJust)
import Data.Ord (Ordering, compare)
import Data.StrMap (StrMap)
import Partial.Unsafe (unsafePartial)
import Perspectives.Resource (PROPDEFS, ResourceDefinitions, getRole)
import Perspectives.ResourceTypes (DomeinFileEffects)
import Perspectives.Syntax (BinnenRol(..), Comments, ID, PerspectContext, PerspectRol, PropertyValueWithComments)
import Prelude (bind, pure, ($))

-- CONTEXT

foreign import context_id :: PerspectContext -> ID

foreign import context_displayName :: PerspectContext -> String

foreign import context_pspType :: PerspectContext -> ID

foreign import context_binnenRol :: PerspectContext -> BinnenRol

foreign import context_buitenRol :: PerspectContext -> ID

foreign import context_rolInContext :: PerspectContext -> StrMap (Array ID)

foreign import context_comments :: PerspectContext -> Comments ()

foreign import createCompactContext :: forall a. {|a} -> PerspectContext

foreign import isCompactContext :: PerspectContext -> Boolean

foreign import createClassicContext :: forall a. {|a} -> PerspectContext

foreign import context_internalProperties :: PerspectContext -> StrMap PropertyValueWithComments

foreign import context_externalProperties :: PerspectContext -> StrMap PropertyValueWithComments

publicProperties :: forall e.
  PerspectContext
  -> Aff (DomeinFileEffects (st :: ST ResourceDefinitions, prd :: PROPDEFS | e)) (StrMap PropertyValueWithComments)
publicProperties c = case isCompactContext c of
  true -> pure $ context_externalProperties c
  false -> do
    maybeBuitenRol <- getRole $ context_buitenRol c
    -- A buitenRol will exist for any ClassicContext. If we cannot reach it, an error will be thrown that we need not handle here.
    pure $ rol_properties $ unsafePartial $ fromJust maybeBuitenRol

privateProperties :: PerspectContext -> StrMap PropertyValueWithComments
privateProperties c = case isCompactContext c of
  true -> context_internalProperties c
  false -> let (BinnenRol{properties}) = context_binnenRol c in properties

-- ROL

foreign import rol_id :: PerspectRol -> ID

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

foreign import isBuitenRol :: PerspectRol -> Boolean
