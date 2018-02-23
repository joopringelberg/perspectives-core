module Main where

import Prelude
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (newRef)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location (search)
import DOM.HTML.Window (location)
import Data.Either (Either(..))
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..))
import Data.StrMap (fromFoldable, lookup) 
import Data.URI.Query (Query(..), parser) as URI
import Halogen.Component.ChildPath (cp1, cp2, cp3)
import Halogen.VDom.Driver (runUI)
import PerspectAceComponent (AceEffects, AceOutput(..), AceQuery(..), aceComponent)
import Perspectives.ContextRoleParser (enclosingContext) as CRP
import Perspectives.DomeinCache (storeDomeinFileInCouchdb)
import Perspectives.Editor.ModelSelect (ModelSelectQuery(..), ModelSelected(..), modelSelect)
import Perspectives.Editor.ReadTextFile (ReadTextFileQuery, TextFileRead(..), readTextFile)
import Perspectives.Effects (AvarCache)
import Perspectives.IndentParser (runIndentParser)
import Perspectives.PerspectivesState (MonadPerspectives, newPerspectivesState)
import Perspectives.PrettyPrinter (prettyPrint, enclosingContext)
import Perspectives.Resource (domeinFileFromContext, getPerspectEntiteit)
import Perspectives.Syntax (PerspectContext)
import Perspectives.User (setCouchdbBaseURL, setCouchdbPassword, setUser)
import Text.Parsing.StringParser (runParser)

-- | Run the app!
main :: Eff (HA.HalogenEffects (AceEffects ())) Unit
main = HA.runHalogenAff do
  -- usr <- userFromLocation
  usr <- pure "admin"
  pw <- pure "geheim"
  url <- pure "http://127.0.0.1:5984/"
  body <- HA.awaitBody
  rf <- liftEff' $ newRef $ newPerspectivesState {userName: usr, couchdbPassword: pw, couchdbBaseURL: url}
  runUI (H.hoist (flip runReaderT rf) ui) unit body

-- userFromLocation :: forall e. Aff (AvarCache (dom :: DOM | e)) (Maybe String)
-- userFromLocation = do
--   parseResult <- liftEff' ((runParser URI.parser) <$> (window >>= location >>= search))
--   case parseResult of
--     (Left m) -> pure Nothing
--     (Right (URI.Query kvp)) -> pure $ lookup "user" $ fromFoldable kvp

-- | The application state, which in this case just stores the current text in
-- | the editor.
type State = { text :: String }

-- | The query algebra for the app.
data Query a
  = ClearText a
  | HandleAceUpdate String a
  | Load String a
  | LoadContext String a
  | Save a

-- | The query algebra for the children
type ChildQuery = Coproduct3 AceQuery ModelSelectQuery ReadTextFileQuery
type ChildSlot = Either3 AceSlot Unit Unit

-- | The slot address type for the Ace component.
data AceSlot = AceSlot Int
derive instance eqAceSlot :: Eq AceSlot
derive instance ordAceSlot :: Ord AceSlot

data ModelSelectSlot = ModelSelectSlot Int
derive instance eqModelSelectSlot :: Eq ModelSelectSlot
derive instance ordModelSelectSlot :: Ord ModelSelectSlot

data ReadTextFileSlot = ReadTextFileSlot Int
derive instance eqReadTextFileSlot :: Eq ReadTextFileSlot
derive instance ordReadTextFileSlot :: Ord ReadTextFileSlot


-- | The main UI component definition.
ui :: forall eff. H.Component HH.HTML Query Unit Void (MonadPerspectives (AceEffects eff))
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

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (MonadPerspectives (AceEffects eff))
  render { text: text } =
    HH.div_
      [ HH.h1_
          [ HH.text "Perspectives editor" ]
      , HH.div_
          [ HH.p_
              [ HH.button
                  [ HE.onClick (HE.input_ ClearText) ]
                  [ HH.text "Clear" ]
              , HH.slot' cp3 unit readTextFile unit handleTextFileRead
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

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (MonadPerspectives (AceEffects eff))
  eval (ClearText next) = do
    _ <- H.query' cp1 (AceSlot 1) $ H.action (ChangeText "")
    pure next
  eval (HandleAceUpdate text next) = do
    parseResult <- lift $ runIndentParser text CRP.enclosingContext
    case parseResult of
      (Right textName) -> do
        (maybeContext :: Maybe PerspectContext) <- lift $ getPerspectEntiteit textName
        case maybeContext of
          Nothing -> do
            H.modify (_ { text = "Cannot find the context that represents this text." })
            pure next
          (Just (c :: PerspectContext)) -> do
            t <- lift $ prettyPrint c enclosingContext
            _ <- H.query' cp1 (AceSlot 2) $ H.action (ChangeText t)
            H.modify (_ { text = text })
            pure next
      (Left e) -> do
        H.modify (_ { text = show e })
        pure next
  eval (Load text next) = do
    H.modify (_ { text = text })
    _ <- H.query' cp1 (AceSlot 1) $ H.action (ChangeText text)
    pure next
  eval (LoadContext id next) = do
    (maybeContext :: Maybe PerspectContext) <- lift $ getPerspectEntiteit id
    case maybeContext of
      Nothing -> do
        H.modify (_ { text = "Cannot find this context: "  <> id })
        pure next
      (Just (c :: PerspectContext)) -> do
        t <- lift $ prettyPrint c enclosingContext
        _ <- H.query' cp1 (AceSlot 1) $ H.action (ChangeText t)
        pure next
  eval (Save next) = do
    text <- H.gets _.text
    parseResult <- lift $ runIndentParser text CRP.enclosingContext
    case parseResult of
      (Right textName) -> do
        -- save it
        mCtxt <- lift $ getPerspectEntiteit textName
        case mCtxt of
          Nothing -> do
            H.modify (_ { text = "Cannot find context " <> textName })
            pure next
          (Just ctxt) -> do
            df <- lift $ domeinFileFromContext ctxt
            lift $ storeDomeinFileInCouchdb df
            H.modify (_ { text = show textName })
            -- notify the ModelSelect
            _ <- H.query' cp2 unit $ H.action Reload
            pure next
      (Left e) -> do
        H.modify (_ { text = show e })
        pure next

  handlePerspectOutput :: AceOutput -> Maybe (Query Unit)
  handlePerspectOutput (TextChanged text) = Just $ H.action $ HandleAceUpdate text

  handleModelSelect :: ModelSelected -> Maybe (Query Unit)
  handleModelSelect (ModelSelected modelname) = Just $ H.action $ LoadContext modelname

  handleTextFileRead :: TextFileRead -> Maybe (Query Unit)
  handleTextFileRead (TextFileRead text) = Just $ H.action $ Load text

  -- Ik denk dat dit een filelist produceert die de handler in kan.
  -- handleFileSelect target =
  -- fileList = unsafeReadTagged "FileList" =<< Foreign.prop "files" (toForeign target)
