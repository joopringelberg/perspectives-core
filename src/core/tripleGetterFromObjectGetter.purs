module Perspectives.TripleGetterFromObjectGetter where

import Control.Monad.Error.Class (throwError)
import Control.Monad.State (lift)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectivesQuery, Triple(..), TripleGetter, TripleRef(..), TypedTripleGetter(..), type (~~>), type (**>))
import Perspectives.EntiteitAndRDFAliases (Predicate)
import Perspectives.Identifiers (LocalName)
import Perspectives.ObjectGetterConstructors (getRoleBinders, searchExternalProperty, searchInternalUnqualifiedProperty, searchProperty)
import Perspectives.PerspectivesTypes (class RolClass, AnyContext, PropertyDef, RolDef, Value, getProperty, typeWithPerspectivesTypes)
import Perspectives.TripleAdministration (addToTripleIndex, detectCycles, lookupInTripleIndex, memorize, memorizeQueryResults)
import Prelude (class Show, bind, flip, pure, show, ($), (<<<), (<>), discard)

trackedAs :: forall s o. Show s =>
  (s ~~> o) ->
  Predicate ->
  TypedTripleGetter s o
trackedAs = flip constructTripleGetter

constructTripleGetter :: forall s o. Show s =>
  Predicate ->
  (s ~~> o) ->
  TypedTripleGetter s o
constructTripleGetter pn objectsGetter = constructTripleGetterFromEffectExpression pn (lift <<< objectsGetter)

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
              []
              (\s -> throwError (error ("Basic triple recomputed: " <> s <> " - " <> pn))))
            lift $ lift $ detectCycles $ TripleRef {subject: show id, predicate: pn}
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

tripleGetterFromTripleGetter :: forall s o t. (s **> o) -> String -> (Array o -> Array t) -> (s **> t)
tripleGetterFromTripleGetter (TypedTripleGetter nameOfp p) name f = memorize tripleGetter name where
  tripleGetter :: TripleGetter s t
  tripleGetter id = do
    (Triple{subject, predicate, object}) <- p id
    t <- liftEffect (addToTripleIndex
      (typeWithPerspectivesTypes id)
      name
      (typeWithPerspectivesTypes (f object))
      []
      [TripleRef {subject: typeWithPerspectivesTypes subject, predicate: predicate}]
      (typeWithPerspectivesTypes tripleGetter))
    lift $ lift $ detectCycles $ TripleRef {subject: typeWithPerspectivesTypes id, predicate: name}
    pure $ ((typeWithPerspectivesTypes t) :: Triple s t)

-- | Construct a TripleGetter from an ObjectsGetter, that is supported by a Triple returned by an arbitrary
-- | TripleGetter. In this way we can insert a computed (rather than calculated by a query) Triple in the
-- | dependency tracking store and have it recomputed when the support changes value.
constructTripleGetterWithArbitrarySupport :: forall s o.
  Predicate ->
  (s -> MonadPerspectivesQuery (Array o)) ->
  TypedTripleGetter s o ->
  TypedTripleGetter s o
constructTripleGetterWithArbitrarySupport pn objectsGetter (TypedTripleGetter predicate supportGetter) = memorize tripleGetter pn where
  tripleGetter :: TripleGetter s o
  tripleGetter id = do
    (object :: Array o) <- objectsGetter id
    t <- liftEffect $ addToTripleIndex
      (typeWithPerspectivesTypes id)
      pn
      (typeWithPerspectivesTypes object)
      []
      [TripleRef {subject: (typeWithPerspectivesTypes id), predicate: predicate}]
      (typeWithPerspectivesTypes tripleGetter)
    pure $ ((typeWithPerspectivesTypes t) :: Triple s o)

-- | Construct a memorizing triple getter from an arbitrary ObjectsGetter. This function is used, a.o.,
-- | to construct getters for the properties of contexts and roles that are not roles or properties, such as
-- | psp:type, psp:binding, psp:label and psp:context. Furthermore, for psp:identity, psp:buitenRol, psp:binnenRol,
-- | psp:iedereRolInContext and psp:typeVanIedereRolInContext.

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
