module Perspectives.QueryCombinators where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (cons, difference, head, null, tail, union)
import Data.Maybe (Maybe(..), maybe)
import Perspectives.Property (PropDefsEffects)
import Perspectives.PropertyComposition (unsafeHead)
import Perspectives.TripleAdministration (NamedFunction(..), Triple(..), TripleGetter, TripleRef(..), addDependency, addTriple, applyNamedFunction, lookupTriple, registerTriple)
import Prelude (bind, id, not, pure, show, ($), (<$>), (<*>), (<<<), (<>), (>=>))

closure :: forall e.
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e)
closure getter@(NamedFunction nameOfp p) = NamedFunction name closure' where
  closure' id = do
    t@(Triple{object} :: Triple) <- liftEff (lookupTriple id name)
    case null object of
      true -> do
        resultOfP@(Triple{object : arr}) <- p id
        -- The end result (represented by: TripleRef{subject: id, predicate: name}) depends partly on the result of the first predicate:
        _ <- liftEff $ addDependency resultOfP (TripleRef{subject: id, predicate: name})
        x <- collect (Just (difference arr [id]))
        liftEff (addTriple id name x [])
      false -> pure t

    where
      collect :: Maybe (Array String) -> Aff (PropDefsEffects e) (Array String)
      collect (Just fs) | not (null fs) = do
        t@(Triple{object}) <- applyNamedFunction (closure getter) (unsafeHead fs)
        _ <- liftEff $ addDependency t (TripleRef{subject: id, predicate: name})
        rest <- collect $ tail fs
        pure $ union (cons (unsafeHead fs) object) rest
      collect otherwise = pure []

  name :: String
  name = "(closure " <>  nameOfp <> ")"

filter :: forall e.
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e)
filter criterium@(NamedFunction nameOfc c) getter@(NamedFunction nameOfp p) = NamedFunction name filter' where
  filter' id = do
    t@(Triple{object} :: Triple) <- liftEff (lookupTriple id name)
    case null object of
      true -> do
        resultOfP@(Triple{object : arr}) <- p id
        -- The end result (represented by: TripleRef{subject: id, predicate: name}) depends partly on the result of the first predicate:
        _ <- liftEff $ addDependency resultOfP (TripleRef{subject: id, predicate: name})
        x <- collect (Just (difference arr [id]))
        liftEff (addTriple id name x [])
      false -> pure t

    where
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

concat :: forall e.
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e)
concat criterium@(NamedFunction nameOfp p) getter@(NamedFunction nameOfq q) = NamedFunction name concat'
  where
    concat' id = do
      t@(Triple{object} :: Triple) <- liftEff (lookupTriple id name)
      case null object of
        true -> do
          pt@(Triple{object : ps}) <- p id
          qt@(Triple{object : qs}) <- q id
          triple <- liftEff (addTriple id name (union ps qs) [])
          _ <- liftEff $ addDependency triple (TripleRef{subject: id, predicate: name})
          _ <- liftEff $ addDependency triple (TripleRef{subject: id, predicate: name})
          pure triple
        false -> pure t

    name = "(concat " <> nameOfp <> " " <> nameOfq <> ")"

-- | This function is not a TripleGetter. It can be used to turn a tripleGetter into another
-- | TripleGetter, that returns a boolean value. It does no dependency tracking,
-- | nor memorisation.
isNothing :: forall e. Triple -> Aff (PropDefsEffects e) Triple
isNothing (Triple r@{object}) = pure (Triple(r {object = [show (not $ null object)]}))

-- TODO: dependency tracking en memorisation!
-- | The generated function will have the specialized name "(hasValue <name of f>)".
hasValue :: forall e. NamedFunction (TripleGetter e) -> NamedFunction (TripleGetter e)
hasValue (NamedFunction nameOfp p) = NamedFunction name hasValue' where
  hasValue' id = do
    t@(Triple{object} :: Triple) <- liftEff (lookupTriple id name)
    case null object of
      true -> (p >=> isNothing >=> liftEff <<< registerTriple) id
      false -> pure t
  name = "(hasValue " <> nameOfp <> ")"
