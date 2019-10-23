module Research.NaturalTransformation where

import Prelude

import Data.Maybe (Maybe(..))

type NaturalTransformation' f g = forall a. f a -> g a

infixr 4 type NaturalTransformation' as ~~>


-- maybeToArray :: forall a. Maybe a -> Array a
maybeToArray :: forall a. Maybe ~~> Array
maybeToArray Nothing = []
maybeToArray (Just a) = [a]
