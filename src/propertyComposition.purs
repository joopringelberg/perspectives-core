module Perspectives.PropertyComposition where


import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (null, tail, union)
import Data.Maybe (Maybe(..))
import Perspectives.Property (PropDefsEffects)
import Perspectives.TripleAdministration (NamedFunction(..), Triple(..), TripleGetter, TripleRef(..), addDependency, addTriple, lookupTriple)
import Prelude (bind, not, pure, ($), (<>))


compose :: forall e.
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e)
compose (NamedFunction nameOfp p) (NamedFunction nameOfq q) = NamedFunction name compose' where
  compose' id = do
    mt <- liftEff (lookupTriple id name)
    case mt of
      Nothing -> do
        resultOfP@(Triple{object : arr}) <- p id
        -- The end result (represented by: TripleRef{subject: id, predicate: name}) depends partly on the result of the first predicate:
        _ <- liftEff $ addDependency resultOfP (TripleRef{subject: id, predicate: name})
        x <- collect (Just arr)
        liftEff (addTriple id name x [])
      (Just t) -> pure t

    where
      collect :: Maybe (Array String) -> Aff (PropDefsEffects e) (Array String)
      collect (Just fs) | not (null fs) = do
        t@(Triple{object}) <- q $ unsafeHead fs
        _ <- liftEff $ addDependency t (TripleRef{subject: id, predicate: name})
        rest <- collect $ tail fs
        pure $ union object rest
      collect otherwise = pure []

  name :: String
  name = "(" <>  nameOfp <> " >-> " <> nameOfq <> ")"

infixl 9 compose as >->

foreign import unsafeHead :: forall a. Array a -> a
