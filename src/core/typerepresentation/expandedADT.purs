module Perspectives.Representation.ExpandedADT  where

import Data.Array (fold, intercalate)
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (unwrap)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')
import Prelude (class Eq, class Functor, class HeytingAlgebra, class Ord, class Show, map, show, ($), (<$>), (<*>), (<<<), (<>))


---- EXPANDED ADT
--------------------------------------------------------------------------------------------------
data ExpandedADT a =
  EST a 
  -- The first (ExpandedADT a) functions as a label and is understood to be an EST value.
  -- This value is also included in the second (ExpandedADT a).
  | ECT (ExpandedADT a) (ExpandedADT a)
  | ESUM (Array (ExpandedADT a)) 
  | EPROD (Array (ExpandedADT a)) 

derive instance Generic (ExpandedADT a) _

instance (Eq a) => Eq (ExpandedADT a) where
  eq b1 b2 = genericEq b1 b2

derive instance (Ord a) => Ord (ExpandedADT a)

instance Functor ExpandedADT where
  map f (EST a) = EST $ f a
  map f (ECT label a) = ECT (map f label) (map f a)
  map f (ESUM adts) = ESUM (map (map f) adts)
  map f (EPROD adts) = EPROD (map (map f) adts)

-- The foldmap function of this Foldable instance folds over SUM and PROD in the same way.
-- For a function that does justice to the notion of `sum` and `product`, see foldMapADT.
instance Foldable ExpandedADT where
  foldMap f adt = case adt of 
    EST a -> f a
    ECT label a -> foldMap f a <> foldMap f a
    EPROD as -> fold (foldMap f <$> as)
    ESUM as -> fold (foldMap f <$> as)
  foldr a = foldrDefault a
  foldl a = foldlDefault a

instance Traversable ExpandedADT where
  traverse f adt = case adt of
    EST a -> EST <$> f a
    ECT label a -> ECT <$> traverse f label <*> traverse f a
    EPROD as -> EPROD <$> (traverse (traverse f) as)
    ESUM as -> ESUM <$> (traverse (traverse f) as)
  sequence a = sequenceDefault a

instance (Show a) => Show (ExpandedADT a) where
  show (EST a) = "(" <> "EST" <> " " <> show a <> ")"
  show (ECT label a) = "(" <> "ECT " <> show label <> " " <> show a <> ")"
  show (ESUM adts) = "(" <> "ESUM" <> show adts <> ")"
  show (EPROD adts) = "(" <> "EPROD" <> show adts <> ")"

instance (Show a) => PrettyPrint (ExpandedADT a) where 
  prettyPrint' t a@(EST _) = t <> show a
  prettyPrint' t (ECT label a) = t <> "(ECT " <> show label <> "\n" <> prettyPrint' (t <> "  ") a <> ")"
  prettyPrint' t (EPROD terms) = t <> "(EPROD [\n" <> (intercalate ",\n" $ prettyPrint' (t <> "  ") <$> terms) <> "\n" <> t <> "  ])"
  prettyPrint' t (ESUM terms) = t <> "(ESUM [\n" <> (intercalate ",\n" $ prettyPrint' (t <> "  ") <$> terms) <> "\n" <> t <> "  ])"

-- | This function is like Foldable foldMap, but it folds with Conj over PROD and with Disj over SUM.
foldMapExpandedADT :: forall a h. HeytingAlgebra h => (a -> h) -> ExpandedADT a -> h
foldMapExpandedADT f adt = case adt of
  EST a -> f a
  ECT label a -> foldMapExpandedADT f a
  EPROD as -> unwrap $ fold (Conj <<< foldMapExpandedADT f <$> as)
  ESUM as -> unwrap $ fold (Disj <<< foldMapExpandedADT f <$> as)

