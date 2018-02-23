module Perspectives.Editor.ReadTextFile where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
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
import Perspectives.PerspectivesState (MonadPerspectives)

type State = {}

data ReadTextFileQuery a
  = FileSet Event a

-- No input to this component.
type Input = Unit

-- Message from this component.
newtype TextFileRead = TextFileRead String

readTextFile :: forall e. H.Component HH.HTML ReadTextFileQuery Input TextFileRead (MonadPerspectives (ajax :: AJAX, dom :: DOM |e))
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
        , HP.prop (wrap "accept")  ".crl"
        -- By using HE.input rather than HE.input_ we will receive an argument, in this case the event object.
        , HE.onChange (HE.input FileSet)
        ]

    eval :: ReadTextFileQuery ~> H.ComponentDSL State ReadTextFileQuery TextFileRead (MonadPerspectives (ajax :: AJAX, dom :: DOM | e))
    eval = case _ of
      FileSet evt next -> do
        -- In order to be able to throw exceptions, we use runExcepT.
        (result :: Either String String) <- lift $ lift $ runExceptT do
          -- The function target will give us a Node as defined in DOM.Classy. However, to retrieve the files
          -- we need a typed HTMLInputElement. This we get by using fromNode. It is part of a Class of which
          -- all typed HTML nodes are a member.
          let (targetNode :: Maybe HTMLInputElement) = fromNode $ target evt
          (fileList :: FileList) <- do
            case targetNode of
              Nothing -> throwError "ReadTextFile component could not convert event target to input element"
              (Just inputElement) -> do
                -- The attribute files of an HTMLInputElement gives us a typed (!) FileList.
                onNothing "no file found" =<< (lift <<< H.liftEff <<< files $ inputElement)

          -- item is here a function in the DOM.File.FileList module.
          case (item 0 fileList) of
            Nothing -> throwError "ReadTextFile component did not receive a file"
            -- readAsText is a convenience as defined in DOM.File.FileReader.Aff.
            (Just file) -> lift $ readAsText (fileToBlob file)
        case result of
          (Left m) -> pure next -- We should do something with this error, such as show it to the user.
          (Right (text :: String)) -> do
            H.raise $ TextFileRead text
            pure next
      where
        onNothing :: forall m. Monad m => String -> Maybe ~> ExceptT String m
        onNothing s = maybe (throwError s) pure
