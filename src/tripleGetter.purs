module Perspectives.TripleGetter where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Perspectives.Property (ObjectsGetter, PropDefsEffects, PropertyName, addToObjectsGetterIndex, getObjectsGetter)
import Perspectives.ResourceTypes (Resource)
import Perspectives.TripleAdministration (Triple, addToTripleIndex, lookupInTripleIndex)
import Prelude (bind, pure)

data NamedFunction f = NamedFunction String f

applyNamedFunction :: forall a b. NamedFunction (a -> b) -> a -> b
applyNamedFunction (NamedFunction _ f) a = f a

applyToNamedFunction :: forall a b. a -> NamedFunction (a -> b) -> b
applyToNamedFunction a (NamedFunction _ f)= f a

infix 0 applyToNamedFunction as ##

type TripleGetter e = Resource -> Aff (PropDefsEffects e) Triple

type NamedTripleGetter = forall e. NamedFunction (TripleGetter e)

constructTripleGetterFromArbitraryFunction :: forall e.
  PropertyName ->
  ObjectsGetter e ->
  NamedFunction (TripleGetter e)
constructTripleGetterFromArbitraryFunction pn getter = NamedFunction pn tripleGetter where
  tripleGetter :: TripleGetter e
  tripleGetter id = do
    mt <- liftEff (lookupInTripleIndex id pn)
    case mt of
      Nothing -> do
        (object' :: Array String) <- getter id
        _ <- liftEff (addToObjectsGetterIndex pn getter)
        liftEff (addToTripleIndex id pn object' [])
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
        _ <- liftEff (addToObjectsGetterIndex pn (getObjectsGetter pn))
        liftEff (addToTripleIndex id pn object' [])
      (Just t) -> pure t
