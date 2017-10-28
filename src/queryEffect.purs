module Perspectives.QueryEffect where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Perspectives.Property (PropDefsEffects, ObjectsGetter)
import Perspectives.ResourceTypes (Resource)
import Perspectives.TripleAdministration (Triple(..), addToTripleIndex)
import Perspectives.TripleGetter (NamedFunction(..))
import Prelude (Unit, bind, pure, ($))

type QueryEffect e = NamedFunction (Array String -> Eff e Unit)

addEffectToQuery :: forall e1 e2 e3.
  NamedFunction (Resource -> Aff (PropDefsEffects e3) (Triple e2))
    -> QueryEffect (PropDefsEffects e1)
      -> NamedFunction (Resource -> Aff (PropDefsEffects e3) (Triple e1))
addEffectToQuery (NamedFunction tgName tg) (NamedFunction effectName effect) =
  NamedFunction effectName addEffectToQuery' where

    addEffectToQuery' :: Resource -> Aff (PropDefsEffects e3) (Triple e1)
    addEffectToQuery' id = do
      t@(Triple{subject, predicate, object}) <- tg id
      liftEff $ addToTripleIndex id effectName [] [] (effectFun object)

    -- endResult :: TripleRef
    -- endResult = TripleRef{subject: id, predicate: effectName}

    effectFun :: Array String -> ObjectsGetter e1
    effectFun objs id = do
      _ <- liftEff $ effect objs
      pure objs

infixl 9 addEffectToQuery as ~>
