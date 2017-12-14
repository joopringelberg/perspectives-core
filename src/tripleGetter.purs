module Perspectives.TripleGetter where

import Control.Monad (class Monad)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.State.Trans (StateT, evalStateT, get)
import Data.Maybe (Maybe(..))
import Perspectives.Property (ObjectsGetter, PropertyName, getObjectsGetter, getPrivateProperty, getProperty, getPublicProperty, getRol)
import Perspectives.Syntax (RoleName)
import Perspectives.TripleAdministration (NamedFunction(..), Triple(..), TripleGetter, addToTripleIndex, lookupInTripleIndex, memorize)
import Prelude (bind, pure, ($))

applyNamedFunction :: forall a b. NamedFunction (a -> b) -> a -> b
applyNamedFunction (NamedFunction _ f) a = f a

applyToNamedFunction :: forall a b m. Monad m => a -> NamedFunction (a -> StateT Boolean m b) -> m b
applyToNamedFunction a (NamedFunction _ f)= evalStateT (f a) true

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
    (object :: Array String) <- liftAff $ objGetter id
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
    remember <- get
    case remember of
      true -> do
        mt <- liftAff $ liftEff (lookupInTripleIndex id pn)
        case mt of
          Nothing -> do
            (object :: Array String) <- liftAff $ getObjectsGetter pn id
            liftEff (addToTripleIndex id pn object [] [] tripleGetter)
          (Just t) -> pure t
      false -> do
        (object :: Array String) <- liftAff $ getObjectsGetter pn id
        liftEff (addToTripleIndex id pn object [] [] tripleGetter)

-- | Use this function to construct property getters that memorize in the triple administration. Use with:
-- | - getRol
-- | - getPublicProperty
-- | - getPrivateProperty
-- | - getProperty
constructTripleGetter' :: forall e.
  (String -> ObjectsGetter e) ->
  PropertyName ->
  NamedFunction (TripleGetter e)
constructTripleGetter' objectsGetter pn = NamedFunction pn tripleGetter where
  tripleGetter :: TripleGetter e
  tripleGetter id = do
    memorize <- get
    case memorize of
      true -> do
        mt <- liftAff $ liftEff (lookupInTripleIndex id pn)
        case mt of
          Nothing -> do
            (object :: Array String) <- liftAff $ objectsGetter pn id
            liftAff $ liftEff (addToTripleIndex id pn object [] [] tripleGetter)
          (Just t) -> pure t
      false -> do
        (object :: Array String) <- liftAff $ objectsGetter pn id
        liftAff $ liftEff (addToTripleIndex id pn object [] [] tripleGetter)


constructPublicPropertyGetter :: forall e.
  PropertyName ->
  NamedFunction (TripleGetter e)
constructPublicPropertyGetter pn = constructTripleGetter' getPublicProperty pn

constructPrivatePropertyGetter :: forall e.
  PropertyName ->
  NamedFunction (TripleGetter e)
constructPrivatePropertyGetter pn = constructTripleGetter' getPrivateProperty pn

constructPropertyGetter :: forall e.
  PropertyName ->
  NamedFunction (TripleGetter e)
constructPropertyGetter pn = constructTripleGetter' getProperty pn

constructRolGetter :: forall e.
  RoleName ->
  NamedFunction (TripleGetter e)
constructRolGetter pn = constructTripleGetter' getRol pn
