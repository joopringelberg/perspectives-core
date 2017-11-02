module Perspectives.QueryEffect where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Perspectives.Property (PropDefsEffects)
import Perspectives.TripleAdministration (NamedFunction(..), Triple(..), TripleGetter, TripleRef(..), getRef, registerTriple)
import Perspectives.TripleGetter (NamedTripleGetter)
import Prelude (Unit, bind, pure, ($), (<>))

type QueryEffect e = NamedFunction (Array String -> Eff (PropDefsEffects e) Unit)

addEffectToQuery :: forall e. NamedTripleGetter e -> QueryEffect e -> NamedTripleGetter e
addEffectToQuery (NamedFunction tgName tg) (NamedFunction effectName effect) =
  NamedFunction effectName addEffectToQuery' where

    addEffectToQuery' :: TripleGetter e
    addEffectToQuery' id = do
      t@(Triple{subject, predicate, object}) <- tg id
      et <- effectFun t id
      liftEff $ registerTriple et
      where
        endResult :: TripleRef
        endResult = TripleRef{subject: id, predicate: name}

    -- propagateTheoryDeltas will use the tripleGetter to recompute,
    -- i.e. to sort the effect again and it will use the resulting triple to
    --  - set new dependencies based on its supports;
    --  - copy its supports to the triple administration (in the old effect triple)
    effectFun :: Triple e -> TripleGetter e
    effectFun queryResult@(Triple{subject, object}) id = do
      _ <- liftEff $ effect object
      pure $ Triple { subject: subject
                    , predicate : name
                    , object : object
                    , dependencies : []
                    , supports : [getRef queryResult]
                    , tripleGetter : (effectFun queryResult)}

    name :: String
    name = "(" <>  tgName <> " ~> " <> effectName <> ")"

infixl 9 addEffectToQuery as ~>
