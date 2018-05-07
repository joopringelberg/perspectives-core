module Perspectives.RunMonadPerspectivesQuery where

import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.State (evalStateT, lift)
import Data.StrMap (singleton)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesQuery, Triple, TripleRef(..), TypedTripleGetter(..), TripleGetter)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (Subject)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Perspectives.TripleAdministration (addToTripleIndex)
import Prelude (flip, bind, ($))

-- | Run the function in a QueryEnvironment that has Subject as the value of "#start".
runMonadPerspectivesQuery :: forall e a.
  Subject
  -> (Subject -> MonadPerspectivesQuery (gm :: GLOBALMAP | e) a)
  -> (MonadPerspectives (gm :: GLOBALMAP | e) a)
runMonadPerspectivesQuery a f = do
  _ <- lift $ liftAff $ liftEff $ addToTripleIndex a "model:Perspectives$start" [a] [] [] tripleGetter
  evalStateT (f a) (singleton "#start" tref)
  where
    tref :: TripleRef
    tref = TripleRef
          { subject: a
          , predicate: "model:Perspectives$start"
        }
    tripleGetter :: TripleGetter e
    tripleGetter id = lift $ liftAff $ liftEff (addToTripleIndex id "model:Perspectives$start" [a] [] [] tripleGetter)

-- Run the TypedTripleGetter in a QueryEnvironment that has Subject as the value of "#start".
runTypedTripleGetter :: forall e.
  TypedTripleGetter e
  -> Subject
  -> (MonadPerspectives (AjaxAvarCache e)) (Triple e)
runTypedTripleGetter (TypedTripleGetter _ f) a = runMonadPerspectivesQuery a f

runQuery :: forall e.
  Subject
  -> TypedTripleGetter e
  -> (MonadPerspectives (AjaxAvarCache e)) (Triple e)
runQuery = (flip runTypedTripleGetter)

infix 0 runQuery as ##
