module Temp.TweeLagen where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Array (singleton)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (class Traversable, traverse)

type AffMaybe e a = Aff e (Maybe a)

compose1 :: forall a b c e.
  (a -> Aff e (Maybe b))
  -> (b -> Aff e (Maybe c))
  -> (a -> Aff e (Maybe c))
compose1 f g = (\a -> do
  x <- f a
  case x of
    Nothing -> pure Nothing
    Just v -> g v)

type AffArray e a = Aff e (Array a)

compose2 :: forall a b c e.
  (a -> Aff e (Array b))
  -> (b -> Aff e (Array c))
  -> (a -> Aff e (Array c))
compose2 f g = (\a -> do
  x <- f a
  y <- traverse g x
  pure (join y))

-- traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b)
-- a = b
-- b = Array c
-- m = Aff e
-- t = Array
-- (b -> Aff e (Array c))
-- -> Array b
-- -> Aff e (Array (Array c))

compose3 :: forall a b c trav e. Traversable trav => Bind trav =>
  (a -> Aff e (trav b))
  -> (b -> Aff e (trav c))
  -> (a -> Aff e (trav c))
compose3 f g = (\a -> do
  x <- f a
  y <- traverse g x
  pure (join y))

f1 :: forall e. Int -> Aff e (Maybe Int)
f1 n | n < 1 = pure Nothing
f1 n = pure (Just n)

f2 :: forall e. Int -> Aff e (Maybe Int)
f2 n | n > 3 = pure Nothing
f2 n = pure (Just (n * n))

f1f1 = f1 `compose3` f2

f3 :: forall e. Int -> Aff e (Array Int)
f3 n | n > 3 = pure []
f3 n = pure [n, n]

compose4 :: forall a b c trav bind e. Traversable trav => Bind bind => Traversable bind =>
  (trav b -> bind b)
  -> (a -> Aff e (trav b))
  -> (b -> Aff e (bind c))
  -> (a -> Aff e (bind c))
compose4 fTog f g = (\a -> do
  x <- f a
  y <- traverse g (fTog x)
  pure (join y))

f1f1' = compose4 id f1 f1

maybeToArray :: forall a. Maybe a -> Array a
maybeToArray = maybe [] singleton

f1f3 = compose4 maybeToArray f1 f3

-- f3f1 = compose4 id f3 f1
