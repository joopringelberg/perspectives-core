module Perspectives.PropertyComposition where


import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (null, tail, union)
import Data.Maybe (Maybe(..))
import Perspectives.Property (Getter, PropDefsEffects, addToGetterIndex)
import Perspectives.TripleAdministration (Triple(..), TripleRef(..), addDependency, addToTripleIndex, lookupInTripleIndex)
import Perspectives.TripleGetter (NamedFunction(..), TripleGetter)
import Prelude (bind, not, pure, ($), (<>))


compose :: forall e.
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e) ->
  NamedFunction (TripleGetter e)
compose (NamedFunction nameOfp p) (NamedFunction nameOfq q) = NamedFunction name compose' where
  compose' :: TripleGetter e
  compose' id = do
    mt <- liftEff (lookupInTripleIndex id name)
    case mt of
      Nothing -> do
        x <- getter id
        _ <- liftEff (addToGetterIndex name getter)
        liftEff (addToTripleIndex id name x [])
      (Just t) -> pure t

    where
      endResult :: TripleRef
      endResult = TripleRef{subject: id, predicate: name}

      getter :: Getter e
      getter id' = do
        t@(Triple{object : objectsOfP}) <- p id'
        -- The end result depends on the result of the first predicate.
        _ <- liftEff $ addDependency t endResult
        collect (Just objectsOfP)

      collect :: Maybe (Array String) -> Aff (PropDefsEffects e) (Array String)
      collect (Just fs) | not (null fs) = do
        let nextP = unsafeHead fs
        t@(Triple{object: objectsOfQ}) <- q nextP
        -- The end result depends, too, on the result of q applied to the next object of p.
        _ <- liftEff $ addDependency t endResult
        restOfObjects <- collect $ tail fs
        pure $ union objectsOfQ restOfObjects
      collect otherwise = pure []

  name :: String
  name = "(" <>  nameOfp <> " >-> " <> nameOfq <> ")"

infixl 9 compose as >->

foreign import unsafeHead :: forall a. Array a -> a
