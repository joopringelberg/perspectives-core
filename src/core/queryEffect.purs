module Perspectives.QueryEffect where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF)
import Perspectives.CoreTypes (NamedFunction(..), Triple(..), TripleGetter)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.TripleAdministration (getRef, registerTriple)
import Perspectives.TripleGetter (NamedTripleGetter)
import Prelude (Unit, bind, pure, ($), (<>))

type QueryEffect e = NamedFunction (Array String -> Eff (AjaxAvarCache (ref :: REF | e)) Unit)

-- | Make an effect function (QueryEffect) dependent on the objects of a tripleGetter.
-- | The result of the function (a Triple) should be unsubscribed from the triple index in order to
-- | make the effect function no longer dependent (using unRegisterTriple).
addEffectToQuery :: forall e. NamedTripleGetter e -> QueryEffect e -> NamedTripleGetter e
addEffectToQuery (NamedFunction tgName tg) (NamedFunction effectName effect) =
  NamedFunction effectName addEffectToQuery' where

    addEffectToQuery' :: TripleGetter e
    addEffectToQuery' id = do
      t@(Triple{subject, predicate, object}) <- tg id
      et <- effectFun t id
      liftEff $ registerTriple et
      -- To unsubscribe the effect, de-register the effect triple.

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
