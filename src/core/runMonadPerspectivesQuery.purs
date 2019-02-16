module Perspectives.RunMonadPerspectivesQuery where

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (evalStateT)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.StrMap (singleton)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesQuery, Triple(..), TripleGetter, TripleRef(..), TypedTripleGetter(..), tripleObjects, type (**>))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Perspectives.PerspectivesTypes (typeWithPerspectivesTypes)
import Perspectives.TripleAdministration (addToTripleIndex)
import Prelude (flip, bind, ($), (>>=), (<<<), pure, (<>))

-- | Run the function in a QueryEnvironment that has Subject as the value of "#start".
runMonadPerspectivesQuery :: forall s o e.
  s
  -> (s -> MonadPerspectivesQuery (gm :: GLOBALMAP | e) o)
  -> (MonadPerspectives (gm :: GLOBALMAP | e) o)
runMonadPerspectivesQuery a f = do
  _ <- liftEff $ typeWithPerspectivesTypes $ addToTripleIndex (typeWithPerspectivesTypes a) "model:Perspectives$start" [typeWithPerspectivesTypes a] [] [] (typeWithPerspectivesTypes tripleGetter)
  evalStateT (f a) (singleton "#start" tref)
  where
    tref :: TripleRef
    tref = TripleRef
          { subject: (typeWithPerspectivesTypes a)
          , predicate: "model:Perspectives$start"
        }
    tripleGetter :: TripleGetter s o e
    tripleGetter id = liftEff (typeWithPerspectivesTypes $ addToTripleIndex (typeWithPerspectivesTypes id) "model:Perspectives$start" [typeWithPerspectivesTypes a] [] [] (typeWithPerspectivesTypes tripleGetter))

------------------------------------------------------------------------------------------------------------------------
-- OBTAIN A TRIPLE
------------------------------------------------------------------------------------------------------------------------
-- Run the TypedTripleGetter in a QueryEnvironment that has Subject as the value of "#start".
runTypedTripleGetter :: forall s o e.
  (s **> o) e
  -> s
  -> MonadPerspectives (AjaxAvarCache e) (Triple s o e)
runTypedTripleGetter (TypedTripleGetter _ f) a = runMonadPerspectivesQuery a f

runQuery :: forall s o e.
  s
  -> (s **> o) e
  -> (MonadPerspectives (AjaxAvarCache e)) (Triple s o e)
runQuery = (flip runTypedTripleGetter)

-- low precedence!
infix 0 runQuery as ##

------------------------------------------------------------------------------------------------------------------------
-- OBTAIN AN ARRAY OF IDS (##=)
------------------------------------------------------------------------------------------------------------------------
runTypedTripleGetterToObjects :: forall s o e. s -> (s **> o) e -> (MonadPerspectives (AjaxAvarCache e)) (Array o)
runTypedTripleGetterToObjects id ttg = runTypedTripleGetter ttg id >>= pure <<< tripleObjects

infix 0 runTypedTripleGetterToObjects as ##=

------------------------------------------------------------------------------------------------------------------------
-- OBTAIN A MAYBE ID (##>)
------------------------------------------------------------------------------------------------------------------------
runTypedTripleGetterToMaybeObject :: forall s o e. s -> (s **> o) e -> (MonadPerspectives (AjaxAvarCache e)) (Maybe o)
runTypedTripleGetterToMaybeObject id ttg = runTypedTripleGetter ttg id >>= pure <<< head <<< tripleObjects

infix 0 runTypedTripleGetterToMaybeObject as ##>

------------------------------------------------------------------------------------------------------------------------
-- OBTAIN AN ID, MAYBE AN ERROR (##>>)
------------------------------------------------------------------------------------------------------------------------
runTypedTripleGetterToObject :: forall s o e. s -> (s **> o) e -> (MonadPerspectives (AjaxAvarCache e)) o
runTypedTripleGetterToObject id ttg@(TypedTripleGetter n _) = runTypedTripleGetter ttg id >>= \(Triple({object})) ->
  case head object of
  Nothing -> throwError $ error $ "TypedTripleGetter '" <> n <> "' returns no values for '" <> (typeWithPerspectivesTypes id) <> "'."
  (Just obj) -> pure obj

infix 0 runTypedTripleGetterToObject as ##>>
