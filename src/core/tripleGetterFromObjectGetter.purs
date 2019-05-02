module Perspectives.TripleGetterFromObjectGetter where

import Effect.Class (liftEff)
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

constructTripleGetterFromEffectExpression :: forall s o e. Show s =>
  Predicate ->
  (s -> MonadPerspectivesQuery (AjaxAvarCache e) (Array o)) ->
  TypedTripleGetter s o e
constructTripleGetterFromEffectExpression pn objectsGetter = TypedTripleGetter pn tripleGetter where
  tripleGetter :: TripleGetter s o e
  tripleGetter id = do
    b <- memorizeQueryResults
    if b
      then do
        mt <- liftEff (lookupInTripleIndex (typeWithPerspectivesTypes id) pn)
        case mt of
          Nothing -> do
            (object :: Array o) <- objectsGetter id
            t <- liftEff (addToTripleIndex
              (typeWithPerspectivesTypes id)
              pn
              (typeWithPerspectivesTypes object)
              []
              [TripleRef {subject: show id, predicate: pn}]
              (typeWithPerspectivesTypes tripleGetter))
            pure $ ((typeWithPerspectivesTypes t) :: Triple s o e)
          (Just t) -> pure ((typeWithPerspectivesTypes t) :: Triple s o e)
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
constructTripleGetterWithArbitrarySupport :: forall s o e.
  Predicate ->
  (s -> MonadPerspectivesQuery (AjaxAvarCache e) (Array o)) ->
  TypedTripleGetter s o e ->
  TypedTripleGetter s o e
constructTripleGetterWithArbitrarySupport pn objectsGetter (TypedTripleGetter _ supportGetter) = TypedTripleGetter pn tripleGetter where
  tripleGetter :: TripleGetter s o e
  tripleGetter id = do
    b <- memorizeQueryResults
    if b
      then do
        mt <- liftEff (lookupInTripleIndex (typeWithPerspectivesTypes id) pn)
        case mt of
          Nothing -> do
            (object :: Array o) <- objectsGetter id
            (Triple{subject, predicate}) <- supportGetter id
            t <- liftEff $ addToTripleIndex
              (typeWithPerspectivesTypes id)
              pn
              (typeWithPerspectivesTypes object)
              []
              [TripleRef {subject: (typeWithPerspectivesTypes subject), predicate: predicate}]
              (typeWithPerspectivesTypes tripleGetter)
            pure $ ((typeWithPerspectivesTypes t) :: Triple s o e)
          (Just t) -> pure ((typeWithPerspectivesTypes t) :: Triple s o e)
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
constructTripleGetterFromObjectsGetter :: forall s o e. Show s =>
  Predicate ->
  (s ~~> o) e ->
  TypedTripleGetter s o e
constructTripleGetterFromObjectsGetter pn objGetter = constructTripleGetterFromEffectExpression pn (lift <<< objGetter)

constructTripleGetter :: forall r s o e. Show s =>
  Predicate ->
  (s ~~> o) e ->
  TypedTripleGetter s o e
constructTripleGetter pn objectsGetter = constructTripleGetterFromEffectExpression pn (lift <<< objectsGetter)

trackedAs :: forall r s o e. Show s =>
  (s ~~> o) e ->
  Predicate ->
  TypedTripleGetter s o e
trackedAs = flip constructTripleGetter

constructExternalPropertySearch :: forall e.
  PropertyDef ->
  TypedTripleGetter AnyContext Value e
constructExternalPropertySearch ln = searchExternalProperty ln `trackedAs` unwrap ln

-- constructInternalPropertyGetter :: forall e.
--   PropertyDef ->
--   TypedTripleGetter AnyContext Value e
-- constructInternalPropertyGetter pn = trackedAs (unwrap pn) (getInternalProperty pn)

constructInternalPropertyLookup :: forall e.
  LocalName ->
  TypedTripleGetter AnyContext Value e
constructInternalPropertyLookup ln = searchInternalUnqualifiedProperty ln `trackedAs` ln

constructRolPropertyGetter :: forall r e. RolClass r =>
  PropertyDef ->
  TypedTripleGetter r Value e
constructRolPropertyGetter pn = getProperty pn `trackedAs` unwrap pn

constructRolPropertySearch :: forall r e.
  RolClass r =>
  PropertyDef ->
  TypedTripleGetter r Value e
constructRolPropertySearch qn = searchProperty qn `trackedAs` unwrap qn

constructInverseRolGetter :: forall r b e.
  RolClass r =>
  RolClass b =>
  RolDef ->
  TypedTripleGetter r b e
constructInverseRolGetter pn = getRoleBinders pn `trackedAs` ((unwrap pn) <> "_inverse")
