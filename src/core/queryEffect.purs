module Perspectives.QueryEffect where

import Control.Monad.Trans.Class (lift)
import Effect.Class (liftEffect)
import Perspectives.ApiTypes (CorrelationIdentifier, Response(..))
import Perspectives.ApiTypes (convertResponse, ApiEffect) as Api
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesQuery, NamedFunction(..), StringTriple, StringTypedTripleGetter, Triple(..), TypedTripleGetter(..), StringTripleGetter)
import Perspectives.TripleAdministration (getRef, registerTriple)
import Prelude (Unit, bind, pure, ($))
import Unsafe.Coerce (unsafeCoerce)

type QueryEffect a = NamedFunction (PerspectivesEffect a)

type PerspectivesEffect a = Array a -> MonadPerspectives Unit

-- | Make a PerspectivesEffect from an ApiEffect.
-- | In pushesObjectsTo we tie a TripleGetter to a PerspectivesEffect.
-- | So we need a function that converts the ApiEffect callback we receive through the API into a
-- | PerspectivesEffect.
sendResult :: CorrelationIdentifier -> Api.ApiEffect -> PerspectivesEffect String
sendResult corrId pe as = liftEffect $ (unsafeCoerce pe) (Api.convertResponse $ Result corrId as)

-- | Apply an ApiEffect to a Response, in effect sending it through the API to the caller.
sendResponse :: Response -> Api.ApiEffect -> MonadPerspectives Unit
sendResponse r ae = liftEffect $ (unsafeCoerce ae) (Api.convertResponse r)

-- | Make an effect function (QueryEffect = named PerspectivesEffect) dependent on the objects of a TypedTripleGetter.
-- | Results in a TypedTripleGetter.
-- | Remove the effect function's dependency on the tripleGetter by using unsubscribeFromObjects.
pushesObjectsTo ::
  StringTypedTripleGetter ->
  QueryEffect String ->
  StringTypedTripleGetter
pushesObjectsTo (TypedTripleGetter tgName tg) (NamedFunction effectName effect) =
  TypedTripleGetter effectName pushesObjectsTo' where

    pushesObjectsTo' :: StringTripleGetter
    pushesObjectsTo' id = do
      et <- effectFun id
      -- Now register the effect triple et as a dependency of t:
      _ <- liftEffect $ registerTriple ( et)
      pure et
      -- To unsubscribe the effect, de-register the effect triple. Find the triple
      -- by its subject (=id) and predicate (=effectName)

    -- propagateTheoryDeltas will use the tripleGetter to recompute,
    -- i.e. to sort the effect again and it will use the resulting triple to
    --  - set new dependencies based on its supports;
    --  - copy its supports to the triple administration (in the old effect triple)
    effectFun :: String -> MonadPerspectivesQuery StringTriple
    effectFun id = do
      queryResult@(Triple{subject, object}) <- tg id
      _ <- lift $ effect ( object)
      pure $ Triple { subject: subject
                    , predicate : effectName
                    , object : object
                    , dependencies : []
                    , supports : [getRef $  queryResult]
                    , tripleGetter : effectFun}

-- high precedence!
infixl 9 pushesObjectsTo as ~>
