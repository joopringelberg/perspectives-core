module Perspectives.QueryCombinators where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (cons, difference, head, null, tail, union)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Perspectives.Property (PropDefsEffects, ObjectsGetter)
import Perspectives.PropertyComposition (compose, unsafeHead)
import Perspectives.TripleAdministration (NamedFunction(..), Triple(..), TripleGetter, TripleRef(..), addDependency, addToTripleIndex, getRef, lookupInTripleIndex, memorize, registerTriple)
import Perspectives.TripleGetter (applyNamedFunction)
import Prelude (bind, id, join, map, not, pure, show, ($), (<$>), (<*>), (<<<), (<>), (>=>))

-- closure :: forall e.
--   NamedFunction (TripleGetter e) ->
--   NamedFunction (TripleGetter e)
-- closure tgetter@(NamedFunction nameOfp p) =
  -- NamedFunction name closure' where
  -- closure' id = do
  --   mt <- liftEff (lookupInTripleIndex id name)
  --   case mt of
  --     Nothing -> do
  --       x <- getter id
  --       liftEff (addToTripleIndex id name x [] [] getter)
  --     (Just t) -> pure t
  --
  --   where
  --     endResult :: TripleRef
  --     endResult = TripleRef{subject: id, predicate: name}
  --
  --     getter :: ObjectsGetter e
  --     getter id' = do
  --       resultOfP@(Triple{object}) <- p id'
  --       -- The end result (represented by: TripleRef{subject: id, predicate: name}) depends partly on the result of the first predicate:
  --       _ <- liftEff $ addDependency resultOfP endResult
  --       collect (Just (difference object [id]))
  --
  --     collect :: Maybe (Array String) -> Aff (PropDefsEffects e) (Array String)
  --     collect (Just fs) | not (null fs) = do
  --       t@(Triple{object}) <- applyNamedFunction (closure tgetter) (unsafeHead fs)
  --       _ <- liftEff $ addDependency t (TripleRef{subject: id, predicate: name})
  --       rest <- collect $ tail fs
  --       pure $ union (cons (unsafeHead fs) object) rest
  --     collect otherwise = pure []

closure :: forall e.
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e)
closure (NamedFunction nameOfp p) =
  memorize getter name
  where
    getter :: TripleGetter e
    getter id' = do
      t@(Triple{object : objectsOfP}) <- p id'
      (triples :: Array (Triple e)) <- traverse getter (difference objectsOfP [id'])
      objects <- pure $ join $ map (\(Triple{object}) -> object) triples
      pure $ Triple { subject: id'
                    , predicate : name
                    , object : objects
                    , dependencies : []
                    , supports : map getRef (cons t triples)
                    , tripleGetter : getter}

    name :: String
    name = "(closure " <>  nameOfp <> ")"

closure' :: forall e.
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e)
closure' p = compose p (closure' p)

filter :: forall e.
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e)
filter (NamedFunction nameOfc c) (NamedFunction nameOfp p) = NamedFunction name filter' where
  filter' :: TripleGetter e
  filter' id = do
    mt <- liftEff (lookupInTripleIndex id name)
    case mt of
      Nothing -> do
        x <- getter' id
        liftEff (addToTripleIndex id name x [] [] getter')
      (Just t) -> pure t

    where
      getter' :: ObjectsGetter e
      getter' id' = do
        resultOfP@(Triple{object : arr}) <- p id'
        -- The end result (represented by: TripleRef{subject: id, predicate: name}) depends partly on the result of the first predicate:
        _ <- liftEff $ addDependency resultOfP (TripleRef{subject: id, predicate: name})
        collect (Just (difference arr [id]))

      collect :: Maybe (Array String) -> Aff (PropDefsEffects e) (Array String)
      collect (Just candidates) | not (null candidates) = do
        t@(Triple{object: judgement}) <- c (unsafeHead candidates)
        _ <- liftEff $ addDependency t (TripleRef{subject: id, predicate: name})
        mcons <$> (addOrNot judgement) <*> (collect $ tail candidates)
          where
          addOrNot ab | not null ab = case unsafeHead ab of
            "true" -> pure $ (head candidates)
            otherwise -> pure Nothing
          addOrNot otherwise = pure Nothing
      collect otherwise = pure []

  name :: String
  name = "(closure " <>  nameOfp <> ")"

mcons :: forall a. Maybe a -> Array a -> Array a
mcons = maybe id cons

{-
concat :: forall e.
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e)
concat (NamedFunction nameOfp p) (NamedFunction nameOfq q) = NamedFunction name concat'
  where
    concat' :: TripleGetter e
    concat' id = do
      mt <- liftEff (lookupInTripleIndex id name)
      case mt of
        Nothing -> do
          objects <- getter id
          triple <- liftEff (addToTripleIndex id name objects [] [] getter)
          _ <- liftEff $ addDependency triple (TripleRef{subject: id, predicate: name})
          _ <- liftEff $ addDependency triple (TripleRef{subject: id, predicate: name})
          pure triple
        (Just t) -> pure t

    getter :: ObjectsGetter e
    getter id' = do
      pt@(Triple{object : ps}) <- p id'
      qt@(Triple{object : qs}) <- q id'
      pure (union ps qs)

    name = "(concat " <> nameOfp <> " " <> nameOfq <> ")"

-- | This function is not a TripleGetter. It can be used to turn a tripleGetter into another
-- | TripleGetter, that returns a boolean value. It does no dependency tracking,
-- | nor memorisation.
isNothing :: forall e. Triple e -> Aff (PropDefsEffects e) (Triple e)
isNothing (Triple r@{object}) = pure (Triple(r {object = [show (not $ null object)]}))

-- TODO: dependency tracking en memorisation!
-- | The generated function will have the specialized name "(hasValue <name of f>)".
hasValue :: forall e. NamedFunction (TripleGetter e) -> NamedFunction (TripleGetter e)
hasValue (NamedFunction nameOfp p) = NamedFunction name hasValue' where
  hasValue' id = do
    mt <- liftEff (lookupInTripleIndex id name)
    case mt of
      Nothing -> (p >=> isNothing >=> liftEff <<< registerTriple) id
      (Just t) -> pure t
  name = "(hasValue " <> nameOfp <> ")"
-}
