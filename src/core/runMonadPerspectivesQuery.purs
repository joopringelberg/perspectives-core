module Perspectives.RunMonadPerspectivesQuery where

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (evalStateT)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap (singleton)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesQuery, Triple(..), TripleGetter, TripleRef(..), TypedTripleGetter(..), tripleObjects)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Perspectives.PerspectivesTypesInPurescript (RolDef(..))
import Perspectives.TripleAdministration (addToTripleIndex)
import Prelude (flip, bind, ($), (>>=), (<<<), pure, (<>))

-- | Run the function in a QueryEnvironment that has Subject as the value of "#start".
runMonadPerspectivesQuery :: forall e a s p.
  Newtype s String =>
  s
  -> (s -> MonadPerspectivesQuery (gm :: GLOBALMAP | e) a)
  -> (MonadPerspectives (gm :: GLOBALMAP | e) a)
runMonadPerspectivesQuery a f = do
  _ <- liftEff $ addToTripleIndex a (RolDef "model:Perspectives$start") [a] [] [] tripleGetter
  evalStateT (f a) (singleton "#start" tref)
  where
    tref :: TripleRef
    tref = TripleRef
          { subject: unwrap a
          , predicate: "model:Perspectives$start"
        }
    tripleGetter :: TripleGetter s RolDef s e
    tripleGetter id = liftEff (addToTripleIndex id (RolDef "model:Perspectives$start") [a] [] [] tripleGetter)

------------------------------------------------------------------------------------------------------------------------
-- OBTAIN A TRIPLE
------------------------------------------------------------------------------------------------------------------------
runTripleGetter :: forall s p o e.
  Newtype s String =>
  (s -> MonadPerspectivesQuery  (AjaxAvarCache e) (Triple s p o e))
  -> s
  -> MonadPerspectives (AjaxAvarCache e) (Triple s p o e)
runTripleGetter f a = runMonadPerspectivesQuery a f

-- Run the TypedTripleGetter in a QueryEnvironment that has Subject as the value of "#start".
runTypedTripleGetter :: forall s p o e.
  Newtype s String =>
  TypedTripleGetter s p o e
  -> s
  -> MonadPerspectives (AjaxAvarCache e) (Triple s p o e)
runTypedTripleGetter (TypedTripleGetter _ f) a = runMonadPerspectivesQuery a f

runQuery :: forall s p o e.
  Newtype s String =>
  s
  -> TypedTripleGetter s p o e
  -> (MonadPerspectives (AjaxAvarCache e)) (Triple s p o e)
runQuery = (flip runTypedTripleGetter)

-- low precedence!
infix 0 runQuery as ##

------------------------------------------------------------------------------------------------------------------------
-- OBTAIN AN ARRAY OF IDS (##=)
------------------------------------------------------------------------------------------------------------------------
runTypedTripleGetterToObjects :: forall s p o e.
  Newtype s String =>
  s ->
  TypedTripleGetter s p o e
  -> MonadPerspectives (AjaxAvarCache e) (Array o)
runTypedTripleGetterToObjects id ttg = runTypedTripleGetter ttg id >>= pure <<< tripleObjects

infix 0 runTypedTripleGetterToObjects as ##=

------------------------------------------------------------------------------------------------------------------------
-- OBTAIN A MAYBE ID (##>)
------------------------------------------------------------------------------------------------------------------------
runTypedTripleGetterToMaybeObject :: forall s p o e.
  Newtype s String =>
  s ->
  TypedTripleGetter s p o e
  -> MonadPerspectives (AjaxAvarCache e) (Maybe o)
runTypedTripleGetterToMaybeObject id ttg = runTypedTripleGetter ttg id >>= pure <<< head <<< tripleObjects

infix 0 runTypedTripleGetterToMaybeObject as ##>

------------------------------------------------------------------------------------------------------------------------
-- OBTAIN AN ID, MAYBE AN ERROR (##>>)
------------------------------------------------------------------------------------------------------------------------
runTypedTripleGetterToObject :: forall s p o e.
  Newtype s String =>
  s ->
  TypedTripleGetter s p o e
  -> MonadPerspectives (AjaxAvarCache e) o
runTypedTripleGetterToObject id ttg@(TypedTripleGetter n _) = runTypedTripleGetter ttg id >>= \(Triple({object})) ->
  case head object of
  Nothing -> throwError $ error $ "TypedTripleGetter '" <> n <> "' returns no values for '" <> unwrap id <> "'."
  (Just obj) -> pure obj

infix 0 runTypedTripleGetterToObject as ##>>
