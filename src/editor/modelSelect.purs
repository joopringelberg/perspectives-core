module Perspectives.Editor.ModelSelect where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Monad.Except.Trans (catchError)
import Data.Array (cons, head, index)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX)
import Perspectives.DomeinCache (documentNamesInDatabase)
import Perspectives.PerspectivesState (MonadPerspectives)

type State = { models :: Array String, index :: Maybe Int, selectedModel :: Maybe String }

data ModelSelectQuery a
  = Initialize a
  | Finalize a
  | Change Int a
  | Reload a

-- No input to this component.
type Input = Unit

newtype ModelSelected = ModelSelected String

modelSelect :: forall e. H.Component HH.HTML ModelSelectQuery Input ModelSelected (MonadPerspectives (ajax :: AJAX | e))
modelSelect =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Just (H.action Finalize)
    , receiver: const Nothing
    }
  where

    initialState :: State
    initialState = {models : [], index: Nothing, selectedModel: Nothing}

    render :: State -> H.ComponentHTML ModelSelectQuery
    render state =
      HH.select
        [ HE.onSelectedIndexChange (HE.input Change)]
        (cons (HH.option [HP.value ""] [HH.text "..."])
          (map (\v -> HH.option [HP.value v] [HH.text v]) state.models))

    eval :: ModelSelectQuery ~> H.ComponentDSL State ModelSelectQuery ModelSelected (MonadPerspectives (ajax :: AJAX | e))
    eval = case _ of
      Initialize next -> do
        (models :: Array String) <- H.liftAff $ catchError (documentNamesInDatabase "perspect_models")
          \_ -> pure []
        H.put { models: models, index: Just 0, selectedModel: head models }
        pure next
      Reload next -> do
        (models :: Array String) <- H.liftAff $ documentNamesInDatabase "perspect_models"
        H.put { models: models, index: Just 0, selectedModel: head models }
        pure next
      Finalize next -> pure next
      Change i next -> do
        models <- H.gets _.models
        H.modify (_ { index = Just (i - 1), selectedModel = index models (i - 1) })
        case index models (i - 1) of
          Nothing -> pure next
          (Just m) -> do
            H.raise $ ModelSelected m
            pure next
