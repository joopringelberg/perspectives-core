module Perspectives.QueryEffect where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Perspectives.Property (PropDefsEffects, ObjectsGetter)
import Perspectives.ResourceTypes (Resource)
import Perspectives.TripleAdministration (Triple(..), TripleRef(..), addDependency, addToTripleIndex)
import Perspectives.TripleGetter (NamedFunction(..), NamedTripleGetter)
import Prelude (Unit, bind, pure, ($), (<>))

type QueryEffect e1 e2= NamedFunction (Triple e2 -> Eff e1 Unit)

-- addEffectToQuery :: forall e1 e2.
--   NamedFunction (Resource -> Aff (PropDefsEffects e2) (Triple e1))
--     -> QueryEffect (PropDefsEffects e2)
--       -> NamedTripleGetter e2
addEffectToQuery (NamedFunction tgName tg) (NamedFunction effectName effect) =
  NamedFunction effectName addEffectToQuery' where

    -- addEffectToQuery' :: Resource -> Aff (PropDefsEffects e2) (Triple e2)
    addEffectToQuery' id = do
      t@(Triple{subject, predicate, object}) <- tg id
      _ <- liftEff $ effect object
      _ <- liftEff $ addDependency t endResult
      liftEff $ addToTripleIndex id name [] [] (effectFun t)
      where
        endResult :: TripleRef
        endResult = TripleRef{subject: id, predicate: name}

    -- effectFun :: Triple e2 -> ObjectsGetter e2
    effectFun (Triple{object}) id = do
      _ <- liftEff $ effect object
      pure object

    name :: String
    name = "(" <>  tgName <> " ~> " <> effectName <> ")"

infixl 9 addEffectToQuery as ~>
