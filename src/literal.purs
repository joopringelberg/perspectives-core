module Perspectives.Literal where

import Prelude
import Data.Monoid
import Control.Fold (mconcat)
import Data.Array (foldM, foldMap)

foreign import data Literal :: Type -> Type

-- | Creates a Literal with a value.
foreign import literal :: forall a b. b -> Literal a

-----------------------------------------------------------------------------------------------
-- | TYPE CLASS INSTANCES
-----------------------------------------------------------------------------------------------

foreign import showLit :: forall a. Literal a -> String

instance showLiteral :: Show (Literal a) where
  show l = showLit l

-- | The map function of the Functor instance of Literal just applies the function to the content of the literal.
instance functorLiteral :: Functor Literal where
  map = mapLit

foreign import mapLit :: forall a b. (a -> b) -> Literal a -> Literal b

-- | The apply function of the Apply instance of Literal takes the function out of the first literal and
-- | applies it to the content of the second literal.
instance applyLiteral :: Apply Literal where
  apply = applyLit

foreign import applyLit :: forall a b. Literal (a -> b) -> Literal a -> Literal b

-- | For the Applicative instance of Literal we have pure wrap a value in a Literal.
instance applicativeLiteral :: Applicative Literal where
  pure = literal

--l1 :: Literal Int
l1 = literal "1" -- type: forall t19. Literal t19
l2 = map ((+)1) l1 -- type: Literal Int

l3 :: Literal Int
l3 = literal "2"

l4 = (+) <$> l1 <*> l2 -- type: Literal Int

l5 = (+) <$> l1

-- Zonder typering van b1 is het type van b3 Data.HeytingAlgebra.HeytingAlgebra t1 en daar is geen show voor.
b1 :: Literal Boolean
b1 = literal "true"
b2 = literal "false"
b3 = (&&) <$> b1 <*> b2

-- NB, dit is willekeurig. Je zou Literal Sum of ofzo moeten gebruiken.
instance semigroupLiteral :: Semigroup (Literal Int) where
  append lit1 lit2 = (+) <$> lit1 <*> lit2

instance monoidLiteral :: Monoid (Literal Int) where
  mempty = literal 0

s = foldMap show [l1, l2]
t = foldMap show [1, 2, 3, 4, 5]
--u = mconcat [1, 2]
