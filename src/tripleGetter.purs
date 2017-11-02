module Perspectives.TripleGetter where

import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Perspectives.Property (PropertyName, ObjectsGetter, getObjectsGetter)
import Perspectives.TripleAdministration (NamedFunction(..), Triple(..), TripleGetter, addToTripleIndex, lookupInTripleIndex, memorize)
import Prelude (bind, pure, ($))

applyNamedFunction :: forall a b. NamedFunction (a -> b) -> a -> b
applyNamedFunction (NamedFunction _ f) a = f a

applyToNamedFunction :: forall a b. a -> NamedFunction (a -> b) -> b
applyToNamedFunction a (NamedFunction _ f)= f a

infix 0 applyToNamedFunction as ##

type NamedTripleGetter e = NamedFunction (TripleGetter e)

constructTripleGetterFromArbitraryFunction :: forall e.
  PropertyName ->
  ObjectsGetter e ->
  NamedFunction (TripleGetter e)
constructTripleGetterFromArbitraryFunction pn objGetter = memorize getter pn
  where
  getter :: TripleGetter e
  getter id = do
    (object :: Array String) <- objGetter id
    pure $ Triple { subject: id
                  , predicate : pn
                  , object : object
                  , dependencies : []
                  , supports : []
                  , tripleGetter : getter}

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
