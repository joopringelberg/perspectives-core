module Perspectives.TripleGetter where

import Perspectives.EntiteitAndRDFAliases

import Control.Monad (class Monad)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.State (lift, modify, gets)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (insert, lookup)
import Perspectives.CoreTypes (Domain, MonadPerspectivesQuery, NamedFunction(..), ObjectsGetter, TripleGetter, TypedTripleGetter(..), Range, VariableName)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.Identifiers (LocalName)
import Perspectives.Property (getExternalProperty, getGebondenAls, getInternalProperty, getProperty, getPropertyFromRolTelescope, getRol, getRolFromContextTypeHierarchy, lookupExternalProperty, lookupInternalProperty)
import Perspectives.TripleAdministration (addToTripleIndex, lookupInTripleIndex, memorizeQueryResults)
import Prelude (Unit, bind, id, ifM, pure, ($), (<<<))

applyNamedFunction :: forall a b. NamedFunction (a -> b) -> a -> b
applyNamedFunction (NamedFunction _ f) a = f a

applyToNamedFunction :: forall a b m. Monad m => a -> NamedFunction (a -> m b) -> m b
applyToNamedFunction a (NamedFunction _ f) = f a

putQueryVariable :: forall e. VariableName -> Array String -> MonadPerspectivesQuery e Unit
putQueryVariable var valueArray = modify \env -> insert var valueArray env

readQueryVariable :: forall e. VariableName -> MonadPerspectivesQuery e (Array String)
readQueryVariable var = gets \env -> maybe [] id (lookup var env)

constructTripleGetterFromEffectExpression :: forall e.
  PropertyName ->
  (ID -> MonadPerspectivesQuery (AjaxAvarCache e) (Array String)) ->
  Domain ->
  Range ->
  TypedTripleGetter e
constructTripleGetterFromEffectExpression pn objectsGetter domain range = TypedTripleGetter pn tripleGetter domain range where
  tripleGetter :: TripleGetter e
  tripleGetter id = ifM memorizeQueryResults
    do
      mt <- lift $ liftAff $ liftEff (lookupInTripleIndex id pn)
      case mt of
        Nothing -> do
          (object :: Array String) <- objectsGetter id
          lift $ liftAff $ liftEff (addToTripleIndex id pn object [] [] tripleGetter)
        (Just t) -> pure t
    do
      (object :: Array String) <- objectsGetter id
      lift $ liftAff $ liftEff (addToTripleIndex id pn object [] [] tripleGetter)

-- | Construct a memorizing triple getter from an arbitrary ObjectsGetter. This function is used, a.o.,
-- | to construct getters for the properties of contexts and roles that are not roles or properties, such as
-- | psp:type, psp:binding, psp:label and psp:context. Furthermore, for psp:identity, psp:buitenRol, psp:binnenRol,
-- | psp:iedereRolInContext and psp:rolTypen.
constructTripleGetterFromObjectsGetter :: forall e a.
  PropertyName ->
  ObjectsGetter e ->
  Domain ->
  Range ->
  TypedTripleGetter e
constructTripleGetterFromObjectsGetter pn objGetter = constructTripleGetterFromEffectExpression pn (lift <<< objGetter)

-- | Use this function to construct property getters that memorize in the triple administration. Use with:
-- | - getRol
-- | - getExternalProperty
-- | - getInternalProperty
-- | - getProperty
constructTripleGetter :: forall e.
  (String -> ObjectsGetter e) ->
  PropertyName ->
  Domain ->
  Range ->
  TypedTripleGetter e
constructTripleGetter objectsGetter pn = constructTripleGetterFromObjectsGetter pn $ objectsGetter pn

constructExternalPropertyGetter :: forall e.
  PropertyName ->
  Domain ->
  Range ->
  TypedTripleGetter e
constructExternalPropertyGetter pn = constructTripleGetter getExternalProperty pn

constructExternalPropertyLookup :: forall e.
  LocalName ->
  Domain ->
  Range ->
  TypedTripleGetter e
constructExternalPropertyLookup ln = constructTripleGetter lookupExternalProperty ln

constructInternalPropertyGetter :: forall e.
  PropertyName ->
  Domain ->
  Range ->
  TypedTripleGetter e
constructInternalPropertyGetter pn = constructTripleGetter getInternalProperty pn

constructInternalPropertyLookup :: forall e.
  LocalName ->
  Domain ->
  Range ->
  TypedTripleGetter e
constructInternalPropertyLookup ln = constructTripleGetter lookupInternalProperty ln

constructRolPropertyGetter :: forall e.
  PropertyName ->
  Domain ->
  Range ->
  TypedTripleGetter e
constructRolPropertyGetter pn = constructTripleGetter getProperty pn

constructRolPropertyLookup :: forall e.
  LocalName ->
  Domain ->
  Range ->
  TypedTripleGetter e
constructRolPropertyLookup ln = constructTripleGetter getPropertyFromRolTelescope ln

constructRolGetter :: forall e.
  RolName ->
  Domain ->
  TypedTripleGetter e
constructRolGetter rn d = constructTripleGetter getRol rn d rn

constructRolLookup :: forall e.
  LocalName ->
  Domain ->
  Range ->
  TypedTripleGetter e
constructRolLookup rn = constructTripleGetter getRolFromContextTypeHierarchy rn

constructInverseRolGetter :: forall e.
  RolName ->
  Domain ->
  Range ->
  TypedTripleGetter e
constructInverseRolGetter pn = constructTripleGetter getGebondenAls pn
