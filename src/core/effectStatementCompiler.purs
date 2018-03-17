module Perspectives.EffectStatementCompiler where

import Data.Array (foldl, unsnoc, head)
import Data.Maybe (Maybe(..))
import Data.URI (Query(..))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID)
import Perspectives.PerspectivesState (MonadPerspectives)
import Perspectives.Property (getContextType)
import Perspectives.QueryEffect (QueryEffect)
import Perspectives.SystemQueries (identity)
import Perspectives.TripleAdministration (NamedFunction(..))
import Prelude (Unit, bind, otherwise, pure, unit)

nullQueryEffect :: forall e. QueryEffect e
nullQueryEffect = NamedFunction "nullQueryEffect" (\_ -> pure unit)

constructEffectFunction :: forall e. ContextID -> MonadPerspectives (AjaxAvarCache e) (QueryEffect e)
constructEffectFunction typeDescriptionID = do
  (pspType :: Array ID) <- getContextType typeDescriptionID
  case head pspType of
    Nothing -> pure nullQueryEffect
    otherwise -> pure nullQueryEffect
