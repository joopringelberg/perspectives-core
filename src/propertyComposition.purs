module Perspectives.PropertyComposition where


import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Writer (Writer, WriterT(..), runWriter, runWriterT)
import Data.Array (cons, elemIndex, head, tail)
import Data.Eq (class Eq)
import Data.Maybe (Maybe(..), maybe)
import Perspectives.ObjectCollection (empty)
import Perspectives.Property (AsyncPropDefsM)
import Perspectives.ResourceTypes (Resource)
import Perspectives.TripleAdministration (NamedFunction(..), Triple(..), TripleGetter, TripleRef(..), addDependency, addTriple)
import Perspectives.Triples (NamedSingleTripleGetter)
import Prelude (bind, const, id, pure, ($), (<$>), (<*>), (<>), (>=>), (>>=))

-- sTos :: forall a b c m. Monad m =>
--   (Location (Maybe a) -> m (Location (Maybe b)))
--   -> (b -> m (Maybe c))
--   -> (Location (Maybe a) -> m (Location (Maybe c)))
-- | This composition operator does not memorize. The memorizing is done entirely by its arguments.
sTos' :: forall e. NamedSingleTripleGetter e -> NamedSingleTripleGetter e -> NamedSingleTripleGetter e
sTos' (NamedFunction nameOfp p) (NamedFunction nameOfq q) = NamedFunction name (p >=> (\(Triple{object}) -> q object)) where
  name :: String
  name = "(" <>  nameOfp <> " >-> " <> nameOfq <> ")"

-- sTos :: forall e.
--   NamedFunction (TripleGetter e Maybe) ->
--   NamedFunction (TripleGetter e Maybe) ->
--   NamedFunction (TripleGetter e Maybe)
sTos :: forall e. NamedSingleTripleGetter e -> NamedSingleTripleGetter e -> NamedSingleTripleGetter e
sTos (NamedFunction nameOfp p) (NamedFunction nameOfq q) = NamedFunction name combination where

  combination mr@(Just id) = do
    first@(Triple{object: mo}) <- p mr
    second@(Triple{object : ma}) <- q mo
    _ <- liftEff (addDependency first (TripleRef{subject: id, predicate: name }))
    _ <- liftEff (addDependency second (TripleRef{subject: id, predicate: name }))
    _ <- liftEff (addTriple id name ma [] [])
    pure (Triple{ subject: id,
      predicate: name
      , object: ma
      , dependencies: []})
  combination Nothing = pure (Triple{ subject: ""
          , predicate: name
          , object: empty
          , dependencies: []
          })

  name :: String
  name = "(" <>  nameOfp <> " >-> " <> nameOfq <> ")"

infixl 0 sTos as >->
-- sTop :: forall a b c m. Monad m =>
--   (Location (Maybe a) -> m (Location (Maybe b)))
--   -> (b -> m (Array c))
--   -> (Location (Maybe a) -> m (Location (Array c)))
-- | This composition operator does not memorize. The memorizing is done entirely by its arguments.
sTop :: forall e.
  NamedFunction (TripleGetter e Maybe) ->
  NamedFunction (TripleGetter e Array) ->
  NamedFunction (TripleGetter e Array)
sTop (NamedFunction nameOfp p) (NamedFunction nameOfq q) = NamedFunction name combination where

  combination mr@(Just id) = do
    first@(Triple{object: mo}) <- p mr
    second@(Triple{object : ma}) <- q mo
    _ <- liftEff (addDependency first (TripleRef{subject: id, predicate: name }))
    _ <- liftEff (addDependency second (TripleRef{subject: id, predicate: name }))
    _ <- liftEff (addTriple id name ma [] [])
    pure (Triple{ subject: id,
      predicate: name
      , object: ma
      , dependencies: []})
  combination Nothing = pure (Triple{ subject: ""
          , predicate: name
          , object: empty
          , dependencies: []
          })

  name :: String
  name = "(" <>  nameOfp <> " >->> " <> nameOfq <> ")"

-- sTop p q = nameFunction name (p >=> q) where
--   name :: String
--   name = functionName p <> ">->" <> functionName q

infixl 0 sTop as >->>

-- pTos :: forall a b c m.  Monad m => Eq c =>
--   (Location (Maybe a) -> m (Location (Array b)))
--   -> (b -> m (Maybe c))
--   -> (Location (Maybe a) -> m (Location (Array c)))
-- | This composition operator does memorize the result of applying the second argument to the result of the first.
-- pTos :: forall e.
--   NamedFunction (TripleGetter e Array) ->
--   NamedFunction (TripleGetter e Maybe) ->
--   NamedFunction (TripleGetter e Array)
-- pTos f g r = f r >>= (\arr -> pTos' (Just arr)) where
--   pTos' :: forall e. Maybe (Array String) -> AsyncPropDefsM e (Triple Array)
--   pTos' Nothing = pure []
--   pTos' (Just fs) = mconsUniques <$> (g $ head fs) <*> (pTos' $ tail fs)

pTos :: forall e.
  NamedFunction (TripleGetter e Array) ->
  NamedFunction (TripleGetter e Maybe) ->
  NamedFunction (TripleGetter e Array)
pTos (NamedFunction nameOfp p) (NamedFunction nameOfq q) = NamedFunction name
  (\r -> do
    case r of
      Nothing ->
        pure  (Triple{ subject: ""
              , predicate: name
              , object: []
              , dependencies: []
              })
      (Just id) -> do
        (Triple{object : arr}) <- p r
        x <- pTos' (Just arr)
        triple <-
          pure (Triple{ subject: id
                , predicate: name
                , object: arr
                , dependencies: []
                })
        _ <- liftEff $ addDependency triple (TripleRef{subject: id, predicate: name})
        pure triple)
  where

  -- pTos' :: Maybe (Array String) -> AsyncPropDefsM e (Array String)
  pTos' :: Maybe (Array String) -> WriterT Dependencies (AsyncPropDefsM e) (Array String)
  pTos' Nothing = pure []
  pTos' (Just fs) = do
    (Triple{object: mnext}) <- q $ head fs
    rest <- runWriterT $ pTos' $ tail fs
    pure $ (maybe id cons) mnext rest

  pTos'' :: Maybe (Array String) -> WriterT Dependencies (AsyncPropDefsM e) (Array String)
  pTos'' Nothing = pure []
  pTos'' (Just fs) =
    mconsUniques <*> (q >=> (\(Triple{object: mnext}) -> pure mnext)) $ head fs <$> pTos'' $ tail fs

  name :: String
  name = "(" <>  nameOfp <> " >-> " <> nameOfq <> ")"

infixl 0 pTos as >>->

type Dependencies = Array TripleRef

-- NOTE: if we make mcons point free, it will effectively not have a name when applied.
mcons :: forall a. (Maybe a) -> (Array a) -> (Array a)
mcons e a = (maybe id cons) e a

-- NOTE: because of the class constraint, mconsUniques compiles to a function that returns a function.
-- This latter function will NOT have a name. Hence it is necessary to name the function on each application...
mconsUniques :: forall a. Eq a => Maybe a -> Array a -> Array a
mconsUniques (Just el) arr | (maybe true (const false)) $ elemIndex el arr = cons el arr
mconsUniques otherwise arr = arr

{-
-- pTop :: forall a b c m.  Monad m => Eq c =>
--   (Location (Maybe a) -> m (Location (Array b)))
--   -> (b -> m (Array c))
--   -> (Location (Maybe a) -> m (Location (Array c)))
-- | This composition operator does memorize the result of applying the second argument to the result of the first.
pTop :: forall e a. Eq a =>
  NamedFunction (TripleGetter e Array) ->
  NamedFunction (TripleGetter e Array) ->
  NamedFunction (TripleGetter e Array)
pTop f g r = f r >>= (\arr -> pTop' (Just arr)) where
  pTop' :: forall e. Maybe (Array Resource) -> AsyncPropDefsM e (Triple Array)
  pTop' Nothing = pure []
  pTop' (Just (fs :: Array Resource)) = union <$> (g $ head fs) <*> (pTop' $ tail fs)

infixl 0 pTop as >>->>
-}
