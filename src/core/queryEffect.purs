module Perspectives.QueryEffect where

import Control.Monad.Trans.Class (lift)
import Effect.Class (liftEffect)
import Perspectives.CoreTypes (type (**>), MonadPerspectivesQuery, NamedFunction(..), Triple(..), TripleGetter, TypedTripleGetter(..), MonadPerspectives)
import Perspectives.PerspectivesTypes (typeWithPerspectivesTypes)
import Perspectives.TripleAdministration (getRef, registerTriple)
import Prelude (Unit, bind, pure, ($), (<>))

type QueryEffect a = NamedFunction (PerspectivesEffect a)

type PerspectivesEffect a = Array a -> MonadPerspectives Unit

-- | Make an effect function (QueryEffect) dependent on the objects of a TypedTripleGetter.
-- | Results in a TypedTripleGetter.
-- | Remove the effect function's dependency on the tripleGetter by using unsubscribeFromObjects.
pushesObjectsTo :: forall s o.
  (s **> o) ->
  QueryEffect String ->
  (s **> o)
pushesObjectsTo (TypedTripleGetter tgName tg) (NamedFunction effectName effect) =
  TypedTripleGetter name pushesObjectsTo' where

    pushesObjectsTo' :: TripleGetter s o
    pushesObjectsTo' id = do
      et <- effectFun id
      -- Now register the effect triple et as a dependency of t:
      _ <- liftEffect $ registerTriple (typeWithPerspectivesTypes et)
      pure et
      -- To unsubscribe the effect, de-register the effect triple. Find the triple
      -- by its subject (=id) and predicate (=name)

    -- propagateTheoryDeltas will use the tripleGetter to recompute,
    -- i.e. to sort the effect again and it will use the resulting triple to
    --  - set new dependencies based on its supports;
    --  - copy its supports to the triple administration (in the old effect triple)
    effectFun :: s -> MonadPerspectivesQuery (Triple s o)
    effectFun id = do
      queryResult@(Triple{subject, object}) <- tg id
      _ <- lift $ effect (typeWithPerspectivesTypes object)
      pure $ Triple { subject: subject
                    , predicate : name
                    , object : object
                    , dependencies : []
                    , supports : [getRef $ typeWithPerspectivesTypes queryResult]
                    , tripleGetter : effectFun}

    name :: String
    name = tgName <> " ~> " <> effectName

-- high precedence!
infixl 9 pushesObjectsTo as ~>
