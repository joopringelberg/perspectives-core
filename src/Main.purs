module Main where

import Prelude
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Network.HTTP.Affjax as AX
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen.VDom.Driver (runUI)
import PerspectAceComponent (AceEffects, AceOutput(..), AceQuery(..), aceComponent)
import Perspectives.ContextRoleParser (sourceText) as CRP
import Perspectives.IndentParser (runIndentParser)
import Perspectives.PrettyPrinter (prettyPrint, sourceText)
import Perspectives.Property (PerspectEffects)
import Perspectives.Resource (getContext)

-- | Run the app!
main :: Eff (HA.HalogenEffects (AceEffects (PerspectEffects ()))) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body

-- | The application state, which in this case just stores the current text in
-- | the editor.
type State = { text :: String }

-- | The query algebra for the app.
data Query a
  = ClearText a
  | HandleAceUpdate String a
  | Load a

-- | The slot address type for the Ace component.
data AceSlot = AceSlot Int
derive instance eqAceSlot :: Eq AceSlot
derive instance ordAceSlot :: Ord AceSlot

-- | The main UI component definition.
ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (AceEffects (PerspectEffects eff)))
ui =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { text: "" }

  render :: State -> H.ParentHTML Query AceQuery AceSlot (Aff (AceEffects (PerspectEffects eff)))
  render { text: text } =
    HH.div_
      [ HH.h1_
          [ HH.text "Perspectives editor" ]
      , HH.div_
          [ HH.p_
              [ HH.button
                  [ HE.onClick (HE.input_ ClearText) ]
                  [ HH.text "Clear" ]
              ]
            , HH.p_
                [ HH.button
                    [ HE.onClick (HE.input_ Load) ]
                    [ HH.text "Load" ]
                ]
          ]
      , HH.div_
          [
          ]
      , HH.div_
          [ HH.slot (AceSlot 1) (aceComponent "ace/mode/perspectives" "ace/theme/perspectives") unit handlePerspectOutput ]
      , HH.div_
          [ HH.slot (AceSlot 2) (aceComponent "ace/mode/perspectives" "ace/theme/perspectives") unit (const Nothing) ]
      , HH.pre_
          [ HH.text ("Current text: " <> text) ]
      ]

  eval :: Query ~> H.ParentDSL State Query AceQuery AceSlot Void (Aff (AceEffects (PerspectEffects eff)))
  eval (ClearText next) = do
    _ <- H.query (AceSlot 1) $ H.action (ChangeText "")
    pure next
  eval (HandleAceUpdate text next) = do
    parseResult <- H.liftAff $ runIndentParser text CRP.sourceText
    case parseResult of
      (Right textName) -> do
        maybeContext <- H.liftAff $ getContext textName
        case maybeContext of
          Nothing -> do
            H.modify (_ { text = "Cannot find the contex that represents this text." })
            pure next
          (Just c) -> do
            t <- H.liftAff $ prettyPrint c sourceText
            -- _ <- H.query (AceSlot 2) $ H.action (ChangeText t)
            H.modify (_ { text = t })
            pure next
      (Left e) -> do
        H.modify (_ { text = show e })
        pure next
  eval (Load next) = do
    response <- H.liftAff $ AX.get ("http://www.pureperspectives.nl/src/editor/perspectives.psp")
    H.modify (_ { text = response.response })
    _ <- H.query (AceSlot 1) $ H.action (ChangeText response.response)
    pure next


  handlePerspectOutput :: AceOutput -> Maybe (Query Unit)
  handlePerspectOutput (TextChanged text) = Just $ H.action $ HandleAceUpdate text
