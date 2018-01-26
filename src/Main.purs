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
import Data.Either.Nested (Either2)
import Data.Function (const)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Halogen.Component.ChildPath (cp1, cp2)
import Halogen.VDom.Driver (runUI)
import PerspectAceComponent (AceEffects, AceOutput(..), AceQuery(..), aceComponent)
import Perspectives.ContextRoleParser (enclosingContext) as CRP
import Perspectives.DomeinCache (storeDomeinFileInCouchdb)
import Perspectives.Editor.ModelSelect (ModelSelectQuery, ModelSelected(..), modelSelect)
import Perspectives.IndentParser (runIndentParser)
import Perspectives.PrettyPrinter (prettyPrint, enclosingContext)
import Perspectives.Property (PerspectEffects)
import Perspectives.Resource (domeinFileFromContext, getContext, storeCouchdbResourceInCouchdb)
import Perspectives.Syntax (PerspectContext)

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
  | LoadContext String a
  | Save a

-- | The query algebra for the children
type ChildQuery = Coproduct2 AceQuery ModelSelectQuery
type ChildSlot = Either2 AceSlot Unit

-- | The slot address type for the Ace component.
data AceSlot = AceSlot Int
derive instance eqAceSlot :: Eq AceSlot
derive instance ordAceSlot :: Ord AceSlot

data ModelSelectSlot = ModelSelectSlot Int
derive instance eqModelSelectSlot :: Eq ModelSelectSlot
derive instance ordModelSelectSlot :: Ord ModelSelectSlot

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

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (AceEffects (PerspectEffects eff)))
  render { text: text } =
    HH.div_
      [ HH.h1_
          [ HH.text "Perspectives editor" ]
      , HH.div_
          [ HH.p_
              [ HH.button
                  [ HE.onClick (HE.input_ ClearText) ]
                  [ HH.text "Clear" ]
              , HH.button
                  [ HE.onClick (HE.input_ Load) ]
                  [ HH.text "Load" ]
              , HH.button
                  [ HE.onClick (HE.input_ Save) ]
                  [ HH.text "Save" ]
              , HH.slot' cp2 unit modelSelect unit handleModelSelect
              ]
          ]
      , HH.div_
          [
          ]
      , HH.div_
          [ HH.slot' cp1 (AceSlot 1) (aceComponent "ace/mode/perspectives" "ace/theme/perspectives") unit handlePerspectOutput ]
      , HH.div_
          [ HH.slot' cp1 (AceSlot 2) (aceComponent "ace/mode/perspectives" "ace/theme/perspectives") unit (const Nothing) ]
      , HH.pre_
          [ HH.text ("Current text: " <> text) ]
      ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (AceEffects (PerspectEffects eff)))
  eval (ClearText next) = do
    _ <- H.query' cp1 (AceSlot 1) $ H.action (ChangeText "")
    pure next
  eval (HandleAceUpdate text next) = do
    parseResult <- H.liftAff $ runIndentParser text CRP.enclosingContext
    case parseResult of
      (Right textName) -> do
        (maybeContext :: Maybe PerspectContext) <- H.liftAff $ getContext textName
        case maybeContext of
          Nothing -> do
            H.modify (_ { text = "Cannot find the context that represents this text." })
            pure next
          (Just (c :: PerspectContext)) -> do
            t <- H.liftAff $ prettyPrint c enclosingContext
            _ <- H.query' cp1 (AceSlot 2) $ H.action (ChangeText t)
            H.modify (_ { text = text })
            pure next
      (Left e) -> do
        H.modify (_ { text = show e })
        pure next
  eval (Load next) = do
    response <- H.liftAff $ AX.get ("http://www.pureperspectives.nl/src/parser/CRL definitie.crl")
    H.modify (_ { text = response.response })
    _ <- H.query' cp1 (AceSlot 1) $ H.action (ChangeText response.response)
    pure next
  eval (LoadContext id next) = do
    (maybeContext :: Maybe PerspectContext) <- H.liftAff $ getContext id
    case maybeContext of
      Nothing -> do
        H.modify (_ { text = "Cannot find this context: "  <> id })
        pure next
      (Just (c :: PerspectContext)) -> do
        t <- H.liftAff $ prettyPrint c enclosingContext
        _ <- H.query' cp1 (AceSlot 1) $ H.action (ChangeText t)
        pure next
  eval (Save next) = do
    text <- H.gets _.text
    parseResult <- H.liftAff $ runIndentParser text CRP.enclosingContext
    case parseResult of
      (Right textName) -> do
        -- save it
        mCtxt <- H.liftAff $ getContext textName
        case mCtxt of
          Nothing -> do
            H.modify (_ { text = "Cannot find context " <> textName })
            pure next
          (Just ctxt) -> do
            df <- H.liftAff $ domeinFileFromContext ctxt
            H.liftAff $ storeDomeinFileInCouchdb df
            H.modify (_ { text = show textName })
            pure next
      (Left e) -> do
        H.modify (_ { text = show e })
        pure next

  handlePerspectOutput :: AceOutput -> Maybe (Query Unit)
  handlePerspectOutput (TextChanged text) = Just $ H.action $ HandleAceUpdate text

  handleModelSelect :: ModelSelected -> Maybe (Query Unit)
  handleModelSelect (ModelSelected modelname) = Just $ H.action $ LoadContext modelname
