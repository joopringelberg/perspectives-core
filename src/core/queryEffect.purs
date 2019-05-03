module Perspectives.QueryEffect where

import Effect (Effect)
import Effect.Class (liftEffect)
import Perspectives.CoreTypes (NamedFunction(..), Triple(..), TripleGetter, TypedTripleGetter(..), MonadPerspectivesQuery, type (**>))

import Perspectives.PerspectivesTypes (typeWithPerspectivesTypes)
import Perspectives.TripleAdministration (getRef, registerTriple)
import Prelude (Unit, bind, const, pure, ($), (<>))

type QueryEffect = NamedFunction (Array String -> Effect Unit)

-- | Make an effect function (QueryEffect) dependent on the objects of a TypedTripleGetter.
-- | Results in a TypedTripleGetter.
-- | Remove the effect function's dependency on the tripleGetter by using unsubscribeFromObjects.
pushesObjectsTo :: forall s o.
  (s **> o) ->
  QueryEffect ->
  (s **> o)
pushesObjectsTo (TypedTripleGetter tgName tg) (NamedFunction effectName effect) =
  TypedTripleGetter effectName pushesObjectsTo' where

    pushesObjectsTo' :: TripleGetter s o
    pushesObjectsTo' id = do
      t <- tg id
      et <- effectFun t
      _ <- liftEffect $ registerTriple (typeWithPerspectivesTypes et)
      pure et
      -- To unsubscribe the effect, de-register the effect triple.

    -- propagateTheoryDeltas will use the tripleGetter to recompute,
    -- i.e. to sort the effect again and it will use the resulting triple to
    --  - set new dependencies based on its supports;
    --  - copy its supports to the triple administration (in the old effect triple)
    effectFun :: Triple s o -> MonadPerspectivesQuery (Triple s o)
    effectFun queryResult@(Triple{subject, object}) = do
      _ <- liftEffect $ effect (typeWithPerspectivesTypes object)
      pure $ Triple { subject: subject
                    , predicate : name
                    , object : object
                    , dependencies : []
                    , supports : [getRef $ typeWithPerspectivesTypes queryResult]
                    , tripleGetter : const (effectFun queryResult)}

    name :: String
    name = "(" <>  tgName <> " ~> " <> effectName <> ")"

-- high precedence!
infixl 9 pushesObjectsTo as ~>
