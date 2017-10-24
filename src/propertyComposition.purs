module Perspectives.PropertyComposition where


import Control.Monad.Eff.Class (liftEff)
import Data.Array (null, tail, union)
import Data.Maybe (Maybe(..))
import Perspectives.Property (AsyncPropDefsM)
import Perspectives.TripleAdministration (NamedFunction(..), Triple(..), TripleGetter, TripleRef(..), addDependency, addTriple, lookup, tripleIndex)
import Prelude (bind, not, pure, ($), (<>))


compose :: forall e.
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e)
compose (NamedFunction nameOfp p) (NamedFunction nameOfq q) = NamedFunction name compose' where
  compose' id = do
    t@(Triple{object} :: Triple) <- liftEff (lookup tripleIndex id name)
    case null object of
      true -> do
        resultOfP@(Triple{object : arr}) <- p id
        -- The end result (represented by: TripleRef{subject: id, predicate: name}) depends partly on the result of the first predicate:
        _ <- liftEff $ addDependency resultOfP (TripleRef{subject: id, predicate: name})
        x <- collect (Just arr)
        liftEff (addTriple id name x [])
      false -> pure t

    where
      collect :: Maybe (Array String) -> (AsyncPropDefsM e) (Array String)
      collect (Just fs) | not (null fs) = do
        t@(Triple{object}) <- q $ head fs
        _ <- liftEff $ addDependency t (TripleRef{subject: id, predicate: name})
        rest <- collect $ tail fs
        pure $ union object rest
      collect otherwise = pure []

  name :: String
  name = "(" <>  nameOfp <> " >-> " <> nameOfq <> ")"

infixl 0 compose as >->

foreign import head :: forall a. Array a -> a
