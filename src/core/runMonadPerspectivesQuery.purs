module Perspectives.RunMonadPerspectivesQuery where

import Control.Monad.Error.Class (throwError)
import Control.Monad.State (evalStateT)
import Control.Monad.Writer (runWriterT)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign.Object (singleton)
import Perspectives.CoreTypes (type (**>), MonadPerspectives, MonadPerspectivesQuery, Triple(..), TripleGetter, TripleRef(..), TypedTripleGetter(..), Assumption, tripleObjects)
import Perspectives.TripleAdministration (addToTripleIndex)
import Prelude (flip, bind, ($), (>>=), (<<<), pure, (<>))
import Unsafe.Coerce (unsafeCoerce)

runMonadPerspectivesQuery :: forall s o.
  s
  -> (s -> MonadPerspectivesQuery o)
  -> (MonadPerspectives (Tuple o (Array Assumption)))
runMonadPerspectivesQuery a f = runWriterT (f a)

evalMonadPerspectivesQuery :: forall s o.
  s
  -> (s -> MonadPerspectivesQuery o)
  -> (MonadPerspectives o)
evalMonadPerspectivesQuery a f = do
    (Tuple result assumptions) <- runMonadPerspectivesQuery a f
    pure result

{------------------------------------------------------------------------------------------------------------------------
-- OBTAIN A TRIPLE
------------------------------------------------------------------------------------------------------------------------
-- Run the TypedTripleGetter in a QueryEnvironment that has Subject as the value of "#start".
runTypedTripleGetter :: forall s o.
  (s **> o)
  -> s
  -> MonadPerspectives (Triple s o)
runTypedTripleGetter (TypedTripleGetter _ f) a = evalMonadPerspectivesQuery a f

runQuery :: forall s o.
  s
  -> (s **> o)
  -> (MonadPerspectives) (Triple s o)
runQuery = (flip runTypedTripleGetter)

-- low precedence!
infix 0 runQuery as ##

------------------------------------------------------------------------------------------------------------------------
-- OBTAIN AN ARRAY OF IDS (##=)
------------------------------------------------------------------------------------------------------------------------
runTypedTripleGetterToObjects :: forall s o. s -> (s **> o) -> (MonadPerspectives) (Array o)
runTypedTripleGetterToObjects id ttg = runTypedTripleGetter ttg id >>= pure <<< tripleObjects

infix 0 runTypedTripleGetterToObjects as ##=

------------------------------------------------------------------------------------------------------------------------
-- OBTAIN A MAYBE ID (##>)
------------------------------------------------------------------------------------------------------------------------
runTypedTripleGetterToMaybeObject :: forall s o. s -> (s **> o) -> (MonadPerspectives) (Maybe o)
runTypedTripleGetterToMaybeObject id ttg = runTypedTripleGetter ttg id >>= pure <<< head <<< tripleObjects

infix 0 runTypedTripleGetterToMaybeObject as ##>

------------------------------------------------------------------------------------------------------------------------
-- OBTAIN AN ID, MAYBE AN ERROR (##>>)
------------------------------------------------------------------------------------------------------------------------
runTypedTripleGetterToObject :: forall s o. s -> (s **> o) -> (MonadPerspectives) o
runTypedTripleGetterToObject id ttg@(TypedTripleGetter n _) = runTypedTripleGetter ttg id >>= \(Triple({object})) ->
  case head object of
  Nothing -> throwError $ error $ "TypedTripleGetter '" <> n <> "' returns no values for '" <> (unsafeCoerce id) <> "'."
  (Just obj) -> pure obj

infix 0 runTypedTripleGetterToObject as ##>>
-}
