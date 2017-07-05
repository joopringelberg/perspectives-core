-- | A test with a network structure
module Test.Network2 where
import Prelude

type Name = String

newtype Node a = Node { payload :: a }

instance showNode :: Show a => Show (Node a) where
  show (Node { payload }) = "{payload: " <> show payload <> "}"

instance functorNetworkNode :: Functor Node where
  map fn n@(Node {payload}) = Node { payload: (fn payload) }

n1 = Node{ payload: 1}
test1 = map (add 1) n1

{-
instance applyNetworkNode :: Apply Node where
  apply :: forall a b. f (a -> b) -> f a -> f b
  apply (Node { payload: fn}) x = Node( {payload: fn <$> x })

instance bindNetworkNode :: Bind Node where
  bind (Node {payload}) f = f payload
-}
