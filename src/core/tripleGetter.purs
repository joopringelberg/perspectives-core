module Perspectives.TripleGetter where

import Perspectives.EntiteitAndRDFAliases

import Control.Monad (class Monad)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.State (evalStateT, lift, modify, gets)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (insert, lookup, singleton)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.Identifiers (LocalName)
import Perspectives.PerspectivesState (MonadPerspectives)
import Perspectives.Property (ObjectsGetter, getExternalProperty, getGebondenAls, getInternalProperty, getProperty, getPropertyFromRolTelescope, getRol, getRolFromContextTypeHierarchy, lookupExternalProperty, lookupInternalProperty)
import Perspectives.TripleAdministration (NamedFunction(..), Triple(..), TripleGetter, MonadPerspectivesQuery, addToTripleIndex, lookupInTripleIndex, memorize, memorizeQueryResults)
import Prelude (Unit, bind, id, ifM, pure, ($))

applyNamedFunction :: forall a b. NamedFunction (a -> b) -> a -> b
applyNamedFunction (NamedFunction _ f) a = f a

applyToNamedFunction :: forall a b m. Monad m => a -> NamedFunction (a -> m b) -> m b
applyToNamedFunction a (NamedFunction _ f)= f a

runQuery :: forall e. Subject -> NamedTripleGetter e -> (MonadPerspectives (AjaxAvarCache e)) (Triple e)
runQuery a (NamedFunction _ f)= evalStateT (f a) (singleton "#start" [a])

infix 0 runQuery as ##

runTripleGetter :: forall e a.
  Subject
  -> (Subject -> MonadPerspectivesQuery e a)
  -> (MonadPerspectives e) a
runTripleGetter a f = evalStateT (f a) (singleton "#start" [a])

type VariableName = String

putQueryVariable :: forall e. VariableName -> Array String -> MonadPerspectivesQuery e Unit
putQueryVariable var valueArray = modify \env -> insert var valueArray env

readQueryVariable :: forall e. VariableName -> MonadPerspectivesQuery e (Array String)
readQueryVariable var = gets \env -> maybe [] id (lookup var env)

type NamedTripleGetter e = NamedFunction (TripleGetter e)

-- | Construct a memorizing triple getter from an arbitrary ObjectsGetter. This function is used
-- | to construct getters for the properties of contexts and roles that are not roles or properties, such as
-- | psp:type, psp:binding, psp:label and psp:context. Furthermore, for psp:identity, psp:buitenRol, psp:binnenRol,
-- | psp:iedereRolInContext and psp:rolTypen.
constructTripleGetterFromObjectsGetter :: forall e.
  PropertyName ->
  ObjectsGetter e ->
  NamedFunction (TripleGetter e)
constructTripleGetterFromObjectsGetter pn objGetter = memorize getter pn
  where
  getter :: TripleGetter e
  getter id = do
    (object :: Array String) <- lift $ objGetter id
    pure $ Triple { subject: id
                  , predicate : pn
                  , object : object
                  , dependencies : []
                  , supports : []
                  , tripleGetter : getter}

constructTripleGetterFromObjectsGetter' :: forall e.
  PropertyName ->
  (ID -> MonadPerspectivesQuery (AjaxAvarCache e) (Array String)) ->
  NamedFunction (TripleGetter e)
constructTripleGetterFromObjectsGetter' pn objGetter = memorize getter pn
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
      mt <- lift $ liftAff $ liftEff (lookupInTripleIndex id pn)
      case mt of
        Nothing -> do
          (object :: Array String) <- lift $ objectsGetter pn id
          lift $ liftAff $ liftEff (addToTripleIndex id pn object [] [] tripleGetter)
        (Just t) -> pure t
    do
      (object :: Array String) <- lift $ objectsGetter pn id
      lift $ liftAff $ liftEff (addToTripleIndex id pn object [] [] tripleGetter)

constructExternalPropertyGetter :: forall e.
  PropertyName ->
  NamedFunction (TripleGetter e)
constructExternalPropertyGetter pn = constructTripleGetter getExternalProperty pn

constructExternalPropertyLookup :: forall e.
  LocalName ->
  NamedFunction (TripleGetter e)
constructExternalPropertyLookup ln = constructTripleGetter lookupExternalProperty ln

constructInternalPropertyGetter :: forall e.
  PropertyName ->
  NamedFunction (TripleGetter e)
constructInternalPropertyGetter pn = constructTripleGetter getInternalProperty pn

constructInternalPropertyLookup :: forall e.
  LocalName ->
  NamedFunction (TripleGetter e)
constructInternalPropertyLookup ln = constructTripleGetter lookupInternalProperty ln

constructRolPropertyGetter :: forall e.
  PropertyName ->
  NamedFunction (TripleGetter e)
constructRolPropertyGetter pn = constructTripleGetter getProperty pn

constructRolPropertyLookup :: forall e.
  LocalName ->
  NamedFunction (TripleGetter e)
constructRolPropertyLookup ln = constructTripleGetter getPropertyFromRolTelescope ln

constructRolGetter :: forall e.
  RolName ->
  NamedFunction (TripleGetter e)
constructRolGetter rn = constructTripleGetter getRol rn

constructRolLookup :: forall e.
  LocalName ->
  NamedFunction (TripleGetter e)
constructRolLookup rn = constructTripleGetter getRolFromContextTypeHierarchy rn

constructInverseRolGetter :: forall e.
  RolName ->
  NamedFunction (TripleGetter e)
constructInverseRolGetter pn = constructTripleGetter getGebondenAls pn
