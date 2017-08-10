module MaybeArray where

import Control.Bind (bindFlipped)
import Data.Array (singleton)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Prelude (map, (<#>), (<$>), (<<<), (=<<), (>>>), id)

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

xTox :: forall a b c. (a -> Maybe b) -> (b -> Maybe c) -> (Maybe a -> Maybe c)
xTox c d = bindFlipped c >>> bindFlipped d
infix 0 xTox as >->

sTop :: forall a b c. (a -> Maybe b) -> (b -> Array c) -> (Maybe a -> Array c)
sTop a b = bindFlipped a >>> maybe [] singleton >>> bindFlipped b
infix 0 sTop as >->>

pTos :: forall a b c. (a -> Array b) -> (b -> Maybe c) -> (Array a -> Array c)
pTos h i = bindFlipped h >>> traverse i >>> (maybe [] id)
infix 0 pTos as >>->

testStoS = f >-> g
testStoP = f >->> p
testPtoS = p >>-> f
