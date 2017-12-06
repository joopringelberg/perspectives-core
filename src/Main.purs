module Main where

import Prelude
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Network.HTTP.Affjax as AX
import Ace.Types (ACE)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen.VDom.Driver (runUI)
import PerspectAceComponent (AceEffects, AceOutput(..), AceQuery(..), aceComponent)
import Perspectives.ContextRoleParser (context)
import Perspectives.IndentParser (runIndentParser)
import Perspectives.Syntax2 (NamedEntityCollection(..))


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
ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (AceEffects (ajax :: AX.AJAX | eff)))
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

  render :: State -> H.ParentHTML Query AceQuery AceSlot (Aff (AceEffects (ajax :: AX.AJAX | eff)))
  render { text: text } =
    HH.div_
      [ HH.h1_
          [ HH.text "ace editor" ]
      , HH.div_
          [ HH.p_
              [ HH.button
                  [ HE.onClick (HE.input_ ClearText) ]
                  [ HH.text "Clear" ]
              ]
          ]
      , HH.div_
          [ HH.p_
              [ HH.button
                  [ HE.onClick (HE.input_ Load) ]
                  [ HH.text "Load" ]
              ]
          ]
      , HH.div_
          [ HH.slot (AceSlot 1) (aceComponent "ace/mode/perspectives" "ace/theme/perspectives") unit handlePerspectOutput ]
      , HH.div_
          [ HH.slot (AceSlot 2) (aceComponent "ace/mode/json" "ace/theme/perspectives") unit (const Nothing) ]
      , HH.pre_
          [ HH.text ("Current text: " <> text) ]
      ]

  eval :: Query ~> H.ParentDSL State Query AceQuery AceSlot Void (Aff (AceEffects (ajax :: AX.AJAX | eff)))
  eval (ClearText next) = do
    _ <- H.query (AceSlot 1) $ H.action (ChangeText "")
    pure next
  eval (HandleAceUpdate text next) = do
    H.modify (_ { text = text })
    -- Here we want to pass the text to the aceComponent in AceSlot 2.
    _ <- H.query (AceSlot 2) $ H.action (ChangeText text)
    pure next
  eval (Load next) = do
    response <- H.liftAff $ AX.get ("http://www.pureperspectives.nl/src/editor/perspectives.psp")
    H.modify (_ { text = response.response })
    _ <- H.query (AceSlot 1) $ H.action (ChangeText response.response)
    pure next


  handlePerspectOutput :: AceOutput -> Maybe (Query Unit)
  handlePerspectOutput (TextChanged text) = Just $ H.action $ HandleAceUpdate (parse text)

parse :: String -> String
parse source = case runIndentParser source context of
  (Right (NamedEntityCollection _ j)) -> show j
  (Left m) -> show m

-- | Run the app!
main :: Eff (HA.HalogenEffects (ace :: ACE, console :: CONSOLE, ajax :: AX.AJAX )) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
