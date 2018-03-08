module Perspectives.TripleGetter where

import Perspectives.EntiteitAndRDFAliases
import Control.Monad (class Monad)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Perspectives.PerspectivesState (memorizeQueryResults)
import Perspectives.Property (ObjectsGetter, getGebondenAls, getInternalProperty, getProperty, getExternalProperty, getRol)
import Perspectives.TripleAdministration (NamedFunction(..), Triple(..), TripleGetter, addToTripleIndex, lookupInTripleIndex, memorize)
import Prelude (bind, ifM, pure, ($))

applyNamedFunction :: forall a b. NamedFunction (a -> b) -> a -> b
applyNamedFunction (NamedFunction _ f) a = f a

applyToNamedFunction :: forall a b m. Monad m => a -> NamedFunction (a -> m b) -> m b
applyToNamedFunction a (NamedFunction _ f)= f a

infix 0 applyToNamedFunction as ##

type NamedTripleGetter e = NamedFunction (TripleGetter e)

-- | Construct a memorizing triple getter from an arbitrary ObjectsGetter. This function is used
-- | to construct getters for the properties of contexts and roles that are not roles or properties, such as
-- | psp:type, psp:binding, psp:label and psp:context. Furthermore, for psp:identity, psp:buitenRol, psp:binnenRol,
-- | psp:iedereRolInContext and psp:rolTypen.
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

-- | Use this function to construct property getters that memorize in the triple administration. Use with:
-- | - getRol
-- | - getExternalProperty
-- | - getInternalProperty
-- | - getProperty
constructTripleGetter :: forall e.
  (String -> ObjectsGetter e) ->
  PropertyName ->
  NamedFunction (TripleGetter e)
constructTripleGetter objectsGetter pn = NamedFunction pn tripleGetter where
  tripleGetter :: TripleGetter e
  tripleGetter id = ifM memorizeQueryResults
    do
      mt <- liftAff $ liftEff (lookupInTripleIndex id pn)
      case mt of
        Nothing -> do
          (object :: Array String) <- objectsGetter pn id
          liftAff $ liftEff (addToTripleIndex id pn object [] [] tripleGetter)
        (Just t) -> pure t
    do
      (object :: Array String) <- objectsGetter pn id
      liftAff $ liftEff (addToTripleIndex id pn object [] [] tripleGetter)

constructExternalPropertyGetter :: forall e.
  PropertyName ->
  NamedFunction (TripleGetter e)
constructExternalPropertyGetter pn = constructTripleGetter getExternalProperty pn

constructInternalPropertyGetter :: forall e.
  PropertyName ->
  NamedFunction (TripleGetter e)
constructInternalPropertyGetter pn = constructTripleGetter getInternalProperty pn

constructRolPropertyGetter :: forall e.
  PropertyName ->
  NamedFunction (TripleGetter e)
constructRolPropertyGetter pn = constructTripleGetter getProperty pn

constructRolGetter :: forall e.
  RolName ->
  NamedFunction (TripleGetter e)
constructRolGetter pn = constructTripleGetter getRol pn

constructInverseRolGetter :: forall e.
  RolName ->
  NamedFunction (TripleGetter e)
constructInverseRolGetter pn = constructTripleGetter getGebondenAls pn
