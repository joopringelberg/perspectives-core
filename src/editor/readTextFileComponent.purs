module Perspectives.Editor.ReadTextFile where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Except.Trans (lift)
import DOM (DOM)
import DOM.Classy.Event (target)
import DOM.Classy.Node (fromNode)
import DOM.Event.Types (Event)
import DOM.File.FileList (item)
import DOM.File.FileReader.Aff (readAsText)
import DOM.File.Types (FileList, fileToBlob)
import DOM.HTML.HTMLInputElement (files)
import DOM.HTML.Types (HTMLInputElement)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Network.HTTP.Affjax (AJAX)

type State = {}

data ReadTextFileQuery a
  = FileSet Event a

-- No input to this component.
type Input = Unit

-- Message from this component.
newtype TextFileRead = TextFileRead String

readTextFile :: forall e. H.Component HH.HTML ReadTextFileQuery Input TextFileRead (Aff (ajax :: AJAX, dom :: DOM |e))
readTextFile = H.component
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  }
  where

    initialState :: State
    initialState = {}

    render :: State -> H.ComponentHTML ReadTextFileQuery
    render state =
      HH.input
        [ HP.type_ HP.InputFile
        , HP.prop (wrap "accept")  "*.crl"
        , HE.onChange (HE.input FileSet)
        ]

    eval :: ReadTextFileQuery ~> H.ComponentDSL State ReadTextFileQuery TextFileRead (Aff (ajax :: AJAX, dom :: DOM | e))
    eval = case _ of
      FileSet evt next -> do
        (result :: Either String String) <- lift $ runExceptT do
          let (targetNode :: Maybe HTMLInputElement) = fromNode $ target evt
          (fileList :: FileList) <- do
            case targetNode of
              Nothing -> throwError "ReadTextFile component could not convert event target to input element"
              (Just inputElement) -> do
                onNothing "no file found" =<< (lift <<< H.liftEff <<< files $ inputElement)

          case (item 0 fileList) of
            Nothing -> throwError "ReadTextFile component did not receive a file"
            (Just file) -> lift $ readAsText (fileToBlob file)
        case result of
          (Left m) -> pure next -- We should do something with this error.
          (Right (text :: String)) -> do
            H.raise $ TextFileRead text
            pure next
      where
        onNothing :: forall m. Monad m => String -> Maybe ~> ExceptT String m
        onNothing s = maybe (throwError s) pure
