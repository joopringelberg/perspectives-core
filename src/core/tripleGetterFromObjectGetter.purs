module Perspectives.TripleGetterFromObjectGetter where

import Effect.Class (liftEffect)
import Control.Monad.State (lift)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Perspectives.CoreTypes (MonadPerspectivesQuery, Triple(..), TripleGetter, TripleRef(..), TypedTripleGetter(..), type (~~>))

import Perspectives.EntiteitAndRDFAliases (Predicate)
import Perspectives.Identifiers (LocalName)
import Perspectives.ObjectGetterConstructors (getRoleBinders, searchExternalProperty, searchInternalUnqualifiedProperty, searchProperty)
import Perspectives.PerspectivesTypes (class RolClass, AnyContext, PropertyDef, RolDef, Value, getProperty, typeWithPerspectivesTypes)
import Perspectives.TripleAdministration (addToTripleIndex, lookupInTripleIndex, memorizeQueryResults)
import Prelude (class Show, bind, flip, pure, show, ($), (<<<), (<>))

constructTripleGetterFromEffectExpression :: forall s o. Show s =>
  Predicate ->
  (s -> MonadPerspectivesQuery (Array o)) ->
  TypedTripleGetter s o
constructTripleGetterFromEffectExpression pn objectsGetter = TypedTripleGetter pn tripleGetter where
  tripleGetter :: TripleGetter s o
  tripleGetter id = do
    b <- memorizeQueryResults
    if b
      then do
        mt <- liftEffect (lookupInTripleIndex (typeWithPerspectivesTypes id) pn)
        case mt of
          Nothing -> do
            (object :: Array o) <- objectsGetter id
            t <- liftEffect (addToTripleIndex
              (typeWithPerspectivesTypes id)
              pn
              (typeWithPerspectivesTypes object)
              []
              [TripleRef {subject: show id, predicate: pn}]
              (typeWithPerspectivesTypes tripleGetter))
            pure $ ((typeWithPerspectivesTypes t) :: Triple s o)
          (Just t) -> pure ((typeWithPerspectivesTypes t) :: Triple s o)
      else do
        (object :: Array o) <- objectsGetter id
        pure (Triple{ subject: id
                  , predicate: pn
                  , object: object
                  , dependencies: []
                  , supports : []
                  , tripleGetter: tripleGetter
                  })

-- | Construct a TripleGetter from an ObjectsGetter, that is supported by a Triple returned by an arbitrary
-- | TripleGetter. In this way we can insert a computed (rather than calculated by a query) Triple in the
-- | dependency tracking store and have it recomputed when the support changes value.
constructTripleGetterWithArbitrarySupport :: forall s o.
  Predicate ->
  (s -> MonadPerspectivesQuery (Array o)) ->
  TypedTripleGetter s o ->
  TypedTripleGetter s o
constructTripleGetterWithArbitrarySupport pn objectsGetter (TypedTripleGetter _ supportGetter) = TypedTripleGetter pn tripleGetter where
  tripleGetter :: TripleGetter s o
  tripleGetter id = do
    b <- memorizeQueryResults
    if b
      then do
        mt <- liftEffect (lookupInTripleIndex (typeWithPerspectivesTypes id) pn)
        case mt of
          Nothing -> do
            (object :: Array o) <- objectsGetter id
            (Triple{subject, predicate}) <- supportGetter id
            t <- liftEffect $ addToTripleIndex
              (typeWithPerspectivesTypes id)
              pn
              (typeWithPerspectivesTypes object)
              []
              [TripleRef {subject: (typeWithPerspectivesTypes subject), predicate: predicate}]
              (typeWithPerspectivesTypes tripleGetter)
            pure $ ((typeWithPerspectivesTypes t) :: Triple s o)
          (Just t) -> pure ((typeWithPerspectivesTypes t) :: Triple s o)
      else do
        (object :: Array o) <- objectsGetter id
        pure (Triple{ subject: id
                  , predicate: pn
                  , object: object
                  , dependencies: []
                  , supports : []
                  , tripleGetter: tripleGetter
                  })

-- | Construct a memorizing triple getter from an arbitrary ObjectsGetter. This function is used, a.o.,
-- | to construct getters for the properties of contexts and roles that are not roles or properties, such as
-- | psp:type, psp:binding, psp:label and psp:context. Furthermore, for psp:identity, psp:buitenRol, psp:binnenRol,
-- | psp:iedereRolInContext and psp:typeVanIedereRolInContext.

-- TODO: faseer deze functie uit tgv `trackedAs`
constructTripleGetterFromObjectsGetter :: forall s o. Show s =>
  Predicate ->
  (s ~~> o) ->
  TypedTripleGetter s o
constructTripleGetterFromObjectsGetter pn objGetter = constructTripleGetterFromEffectExpression pn (lift <<< objGetter)

constructTripleGetter :: forall s o. Show s =>
  Predicate ->
  (s ~~> o) ->
  TypedTripleGetter s o
constructTripleGetter pn objectsGetter = constructTripleGetterFromEffectExpression pn (lift <<< objectsGetter)

trackedAs :: forall s o. Show s =>
  (s ~~> o) ->
  Predicate ->
  TypedTripleGetter s o
trackedAs = flip constructTripleGetter

constructExternalPropertySearch ::
  PropertyDef ->
  TypedTripleGetter AnyContext Value
constructExternalPropertySearch ln = searchExternalProperty ln `trackedAs` unwrap ln

-- constructInternalPropertyGetter :: forall e.
--   PropertyDef ->
--   TypedTripleGetter AnyContext Value e
-- constructInternalPropertyGetter pn = trackedAs (unwrap pn) (getInternalProperty pn)

constructInternalPropertyLookup ::
  LocalName ->
  TypedTripleGetter AnyContext Value
constructInternalPropertyLookup ln = searchInternalUnqualifiedProperty ln `trackedAs` ln

constructRolPropertyGetter :: forall r. RolClass r =>
  PropertyDef ->
  TypedTripleGetter r Value
constructRolPropertyGetter pn = getProperty pn `trackedAs` unwrap pn

constructRolPropertySearch :: forall r.
  RolClass r =>
  PropertyDef ->
  TypedTripleGetter r Value
constructRolPropertySearch qn = searchProperty qn `trackedAs` unwrap qn

constructInverseRolGetter :: forall r b.
  RolClass r =>
  RolClass b =>
  RolDef ->
  TypedTripleGetter r b
constructInverseRolGetter pn = getRoleBinders pn `trackedAs` ((unwrap pn) <> "_inverse")
