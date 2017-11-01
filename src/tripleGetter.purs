module Perspectives.TripleGetter where

import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Perspectives.Property (PropertyName, getObjectsGetter)
import Perspectives.TripleAdministration (NamedFunction(..), Triple(..), TripleGetter, addToTripleIndex, lookupInTripleIndex)
import Prelude (bind, pure)

applyNamedFunction :: forall a b. NamedFunction (a -> b) -> a -> b
applyNamedFunction (NamedFunction _ f) a = f a

applyToNamedFunction :: forall a b. a -> NamedFunction (a -> b) -> b
applyToNamedFunction a (NamedFunction _ f)= f a

infix 0 applyToNamedFunction as ##

type NamedTripleGetter e = NamedFunction (TripleGetter e)

constructTripleGetterFromArbitraryFunction :: forall e.
  PropertyName ->
  TripleGetter e ->
  NamedFunction (TripleGetter e)
constructTripleGetterFromArbitraryFunction pn getter = NamedFunction pn tripleGetter where
  tripleGetter :: TripleGetter e
  tripleGetter id = do
    mt <- liftEff (lookupInTripleIndex id pn)
    case mt of
      Nothing -> do
        (Triple{object}) <- getter id
        liftEff (addToTripleIndex id pn object [] [] getter)
      (Just t) -> pure t

-- | Use this function to construct property getters that memorize in the triple administration.
constructTripleGetter :: forall e.
  PropertyName ->
  NamedFunction (TripleGetter e)
constructTripleGetter pn = NamedFunction pn tripleGetter where
  tripleGetter :: TripleGetter e
  tripleGetter id = do
    mt <- liftEff (lookupInTripleIndex id pn)
    case mt of
      Nothing -> do
        (object' :: Array String) <- getObjectsGetter pn id
        liftEff (addToTripleIndex id pn object' [] [] tripleGetter)
      (Just t) -> pure t
