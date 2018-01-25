module Perspectives.Editor.ModelSelect where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import Data.Array (head, index)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX)
import Perspectives.DomeinCache (documentNamesInDatabase)

type State = { models :: Array String, index :: Maybe Int, selectedModel :: Maybe String }

data ModelSelectQuery a
  = Initialize a
  | Finalize a
  | Change Int a

-- No input to this component.
type Input = Unit

newtype ModelSelected = ModelSelected String

modelSelect :: forall e. H.Component HH.HTML ModelSelectQuery Input ModelSelected (Aff (ajax :: AJAX | e))
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
        (map (\v -> HH.option [HP.value v] [HH.text v]) state.models)

    eval :: ModelSelectQuery ~> H.ComponentDSL State ModelSelectQuery ModelSelected (Aff (ajax :: AJAX | e))
    eval = case _ of
      Initialize next -> do
        (models :: Array String) <- H.liftAff $ documentNamesInDatabase "perspect_models"
        H.put { models: models, index: Just 0, selectedModel: head models }
        pure next
      Finalize next -> pure next
      Change i next -> do
        models <- H.gets _.models
        H.modify (_ { index = Just i, selectedModel = index models i })
        case index models i of
          Nothing -> pure next
          (Just m) -> do
            H.raise $ ModelSelected m
            pure next
