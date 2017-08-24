module MaybeArray where

import Control.Bind (bindFlipped, composeKleisli)
import Control.Monad.Aff (Aff)
import Data.Array (cons, foldr, singleton)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Prelude (class Applicative, class Bind, class Monad, bind, id, map, pure, (+), (<#>), (<$>), (<<<), (=<<), (>=>), (>>=), (>>>))

f :: forall a. a -> Maybe a
f x = Just x

g :: forall a. a -> Maybe a
g x = Just x

fBound :: forall a. Maybe a -> Maybe a
fBound = bindFlipped f

gBound :: forall a. Maybe a -> Maybe a
gBound = bindFlipped f

composed :: forall a. Maybe a -> Maybe a
composed = fBound <<< gBound

composed2 :: forall a. Maybe a -> Maybe a
composed2 = bindFlipped f >>> bindFlipped g

applied :: forall a. Maybe a -> Maybe a
applied a = f =<< g =<< a

p :: forall a. a -> Array a
p y = [y]

q :: forall a. a -> Array a
q z = [z]

pBound :: forall a. Array a -> Array a
pBound = bindFlipped p

composedA :: forall a. Array a -> Array a
composedA = bindFlipped p <<< bindFlipped q

s2p :: forall a. Maybe a -> Array a
s2p Nothing = []
s2p (Just a) = [a]

s2p' = maybe [] singleton

x :: forall a. Maybe a -> Array a
x = bindFlipped f >>> s2p >>> bindFlipped p

m :: forall a. Array a -> Maybe (Array a)
m = bindFlipped p >>> traverse f

m2a Nothing = []
m2a (Just a) = a

m' = bindFlipped p >>> traverse f >>> m2a
m'' = bindFlipped p >>> (\x -> maybe [] id (traverse f x))
m''' = bindFlipped p >>> traverse f >>> (maybe [] id)


------------------------------------------------------------------------
----- THREE INFIX OPERATORS
------------------------------------------------------------------------

xTox :: forall a b c m. Bind m => (a -> m b) -> (b -> m c) -> a -> m c
xTox = composeKleisli
-- xTox f g = f >=> g

-- xTox' :: forall a b c m ct. Monad m => Applicative ct =>
--   (a -> m (Either String (ct b))) ->
--   (b -> m (Either String (ct c))) ->
--   (a -> m (Either String (ct c)))
xTox' f g a = f a >>= (either (pure <<< Left) (map g))

specialCompose :: forall a b c e m. Bind m =>
  (a -> Aff e (Either String (Maybe b)))
  -> (b -> Aff e (Either String (Maybe c)))
  -> (a -> Aff e (Either String (Maybe c)))
specialCompose f g a = do
  r <- f a
  case r of
    (Left err) -> pure (Left err)
    (Right ft) -> case ft of
      Nothing -> pure (Right Nothing)
      (Just c) -> g c

xTox''' f g a = do
  r <- f a
  case r of
    (Left err) -> pure (Left err)
    (Right ft) -> traverse g ft

{-
I have a function f:
  f :: forall e a b. a -> Aff e (Either String (Array b))
Now I would like to apply that functon to a value of type Array a and produce, again, the same return type. In other words, I need a function of type:
  forall e a b. Array a -> Aff e (Either Error (Array b))
I break my head over how to construct the latter function out of f.

I actually want to compose functions like f. This puzzle derives from that.

I could solve a similar puzzle, that is composing functions like g:
  g :: forall e a b. a -> Aff e (Either String (Maybe b))
The following function will compose g with itself:
  specialCompose :: forall a b c e m. Bind m =>
    (a -> Aff e (Either String (Maybe b)))
    -> (b -> Aff e (Either String (Maybe c)))
    -> (a -> Aff e (Either String (Maybe c)))
  specialCompose f g a = do
    r <- f a
    case r of
      (Left err) -> pure (Left err)
      (Right ft) -> case ft of
        Nothing -> pure (Right Nothing)
        (Just c) -> g c
-}

{-
f heeft als type:
  a -> m (Either String (ct b))
composeKleisli past f toe en past de tweede functie toe op de inner value van het resultaat daarvan.
Deze tweede functie, die ik construeer, moet dus dit type hebben:
  (Either String (ct b)) -> m (Either String (ct c))
Maar g heeft dit type:
  b -> m (Either String (ct c))
In:
  either (pure <<< Left) x
moet x als type hebben:
  ct b -> m (Either String (ct c))
De functie
  map g
heeft als type:
  ct b -> ct (m (Either String (ct c)))
dus dat is niet OK.
-}

sTop :: forall a b c. (a -> Maybe b) -> (b -> Array c) -> (a -> Array c)
-- sTop a b = a >>> maybe [] singleton >>> bindFlipped b
sTop a b = a >>> (maybe [] b)
infix 0 sTop as >=>>

pTos :: forall a b c. (a -> Array b) -> (b -> Maybe c) -> (a -> Array c)
pTos h i = h >>> map i >>> (foldr (maybe id cons) [])
infix 0 pTos as >>=>

testXtoX = f >=> g
testStoP = f >=>> p
testPtoS = p >>=> f
testAll = f >=> f >=>> p >=> p >>=> f

------------------------------------------------------------------------
----- A MISTAKEN EXPERIMENT
------------------------------------------------------------------------
-- xTox' :: forall a b c m. Monad m =>
--   (a -> m (Either String b))
--   -> (b -> m (Either String c))
--   -> (m (Either String a) -> m (Either String c))
-- xTox' c d = bindFlipped (applyProperty c) >>> bindFlipped (applyProperty d)
--
-- sTop' a b = bindFlipped (applyProperty a) >>> maybe [] singleton >>> bindFlipped (applyProperty b)

------------------------------------------------------------------------
----- GETTERS
------------------------------------------------------------------------
-- type Getter e a m. Monad m = Resource -> AsyncPropDefs e (Either String (m a))
--
-- applyProperty :: forall a b m. Monad m =>
--   (a -> m (Either String b))
--   ->( Either String a
--       -> m (Either String b) )
-- applyProperty getter = either (pure <<< Left) getter
--
-- bindFlipped :: forall m a b. Bind m => (a -> m b) -> m a -> m b
-- bindFlipped = flip bind

-- composeKleisli :: forall a b c m. Bind m => (a -> m b) -> (b -> m c) -> a -> m c
-- composeKleisli f g a = f a >>= g
-- infixr 1 composeKleisli as >=>

myCompose :: forall a b c m. Monad m =>
  (a -> m (Either String b))
  -> (b -> m (Either String c))
  -> (a -> m (Either String c))
myCompose a b = a >=> (either (pure <<< Left) b)

applyProperty :: forall a b m. Monad m =>
  (a -> m (Either String b))
  ->( Either String a
      -> m (Either String b) )
applyProperty a = either (pure <<< Left) a

------------------------------------------------------------------------
----- NESTED MONADS
------------------------------------------------------------------------
d1 :: Either String (Maybe Int)
d1 = Right (Just 1)

add1 :: Int -> Either String (Maybe Int)
add1 arg = Right (Just (arg + 1))

k :: Either String (Either String Int) -> Either String (Maybe Int)
k es = es >>= (\mi -> mi >>= (\i -> add1 i))
