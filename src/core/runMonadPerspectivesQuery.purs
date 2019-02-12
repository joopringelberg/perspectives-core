module Perspectives.RunMonadPerspectivesQuery where

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (evalStateT)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.StrMap (singleton)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesQuery, Triple(..), TripleGetter, TripleRef(..), TypedTripleGetter(..), tripleObjects)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (Subject, ID)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Perspectives.TripleAdministration (addToTripleIndex)
import Prelude (flip, bind, ($), (>>=), (<<<), pure, (<>))

-- | Run the function in a QueryEnvironment that has Subject as the value of "#start".
runMonadPerspectivesQuery :: forall e a.
  Subject
  -> (Subject -> MonadPerspectivesQuery (gm :: GLOBALMAP | e) a)
  -> (MonadPerspectives (gm :: GLOBALMAP | e) a)
runMonadPerspectivesQuery a f = do
  _ <- liftEff $ addToTripleIndex a "model:Perspectives$start" [a] [] [] tripleGetter
  evalStateT (f a) (singleton "#start" tref)
  where
    tref :: TripleRef
    tref = TripleRef
          { subject: a
          , predicate: "model:Perspectives$start"
        }
    tripleGetter :: TripleGetter e
    tripleGetter id = liftEff (addToTripleIndex id "model:Perspectives$start" [a] [] [] tripleGetter)

------------------------------------------------------------------------------------------------------------------------
-- OBTAIN A TRIPLE
------------------------------------------------------------------------------------------------------------------------
-- Run the TypedTripleGetter in a QueryEnvironment that has Subject as the value of "#start".
runTypedTripleGetter :: forall e.
  TypedTripleGetter e
  -> Subject
  -> MonadPerspectives (AjaxAvarCache e) (Triple e)
runTypedTripleGetter (TypedTripleGetter _ f) a = runMonadPerspectivesQuery a f

runQuery :: forall e.
  Subject
  -> TypedTripleGetter e
  -> (MonadPerspectives (AjaxAvarCache e)) (Triple e)
runQuery = (flip runTypedTripleGetter)

-- low precedence!
infix 0 runQuery as ##

------------------------------------------------------------------------------------------------------------------------
-- OBTAIN AN ARRAY OF IDS (##=)
------------------------------------------------------------------------------------------------------------------------
runTypedTripleGetterToObjects :: forall e. Subject -> TypedTripleGetter e -> (MonadPerspectives (AjaxAvarCache e)) (Array ID)
runTypedTripleGetterToObjects id ttg = runTypedTripleGetter ttg id >>= pure <<< tripleObjects

infix 0 runTypedTripleGetterToObjects as ##=

------------------------------------------------------------------------------------------------------------------------
-- OBTAIN A MAYBE ID (##>)
------------------------------------------------------------------------------------------------------------------------
runTypedTripleGetterToMaybeObject :: forall e. Subject -> TypedTripleGetter e -> (MonadPerspectives (AjaxAvarCache e)) (Maybe ID)
runTypedTripleGetterToMaybeObject id ttg = runTypedTripleGetter ttg id >>= pure <<< head <<< tripleObjects

infix 0 runTypedTripleGetterToMaybeObject as ##>

------------------------------------------------------------------------------------------------------------------------
-- OBTAIN AN ID, MAYBE AN ERROR (##>>)
------------------------------------------------------------------------------------------------------------------------
runTypedTripleGetterToObject :: forall e. Subject -> TypedTripleGetter e -> (MonadPerspectives (AjaxAvarCache e)) ID
runTypedTripleGetterToObject id ttg@(TypedTripleGetter n _) = runTypedTripleGetter ttg id >>= \(Triple({object})) ->
  case head object of
  Nothing -> throwError $ error $ "TypedTripleGetter '" <> n <> "' returns no values for '" <> id <> "'."
  (Just obj) -> pure obj

infix 0 runTypedTripleGetterToObject as ##>>
