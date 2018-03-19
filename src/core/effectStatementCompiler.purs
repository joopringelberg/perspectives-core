module Perspectives.EffectStatementCompiler where

import Data.Array (foldl, unsnoc, head)
import Data.Maybe (Maybe(..))
import Data.URI (Query(..))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID)
import Perspectives.PerspectivesState (MonadPerspectives)
import Perspectives.Property (getContextType)
import Perspectives.QueryEffect (QueryEffect)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.SystemQueries (identity)
import Perspectives.TripleAdministration (NamedFunction(..))
import Perspectives.TripleGetter (constructExternalPropertyGetter, (##))
import Prelude (bind, const, otherwise, pure, unit, (>=>), (>>=))

nullQueryEffect :: forall e. QueryEffect e
nullQueryEffect = NamedFunction "nullQueryEffect" (\_ -> pure unit)

constructEffectExpressie :: forall e. ContextID -> MonadPerspectives (AjaxAvarCache e) (QueryEffect e)
constructEffectExpressie typeDescriptionID = do
  (pspType :: Array ID) <- getContextType typeDescriptionID
  case head pspType of
    Nothing -> pure nullQueryEffect
    -- (Just "model:Effect$Constant") -> do
    --   arrWithConst <- typeDescriptionID ## ((constructExternalPropertyGetter "model:Effect$Constant$value") >>= tripleObjects_)
    --   pure $ const arrWithConst
    (Just _) -> pure nullQueryEffect
