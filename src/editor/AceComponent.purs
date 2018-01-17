module PerspectAceComponent (AceEffects, AceQuery(..), Mode, Theme, AceOutput(..), aceComponent) where

import Prelude
import Ace as Ace
import Ace.EditSession as Session
import Ace.Editor as Editor
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ace.BackgroundTokenizer (setTokenizer) as BackgroundTokenizer
import Ace.Document (getLine, onChange) as Document
import Ace.EditSession (clearAnnotations, getDocument, getLine, setAnnotations, setUseSoftTabs)
import Ace.Types (ACE, Document, DocumentEvent(..), DocumentEventType(..), EditSession, Editor, Position(..), Tokenizer, TokenInfo)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Array (dropEnd, cons, snoc, length) as AR
import Data.Array.Partial (head, last, tail) as AP
import Data.Either (Either(..))
import Data.Function.Uncurried (mkFn3)
import Data.Maybe (Maybe(..), fromJust)
import Data.String (drop, splitAt, take, length)
import Data.Tuple (Tuple(..))
import Halogen.Query (liftAff)
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextRoleParser (contextDeclaration, contextName) as CRP
import Perspectives.IndentParser (runIndentParser)
import Perspectives.Parser (AceError, errorsIn)
import Perspectives.ResourceTypes (DomeinFileEffects)
import Perspectives.Syntax (ContextDeclaration(..), Expanded(..))

-- | As long as the user edits a contextname, we keep the original name here:
type ContextNameInText = Maybe { name :: String, line :: Int}

-- | The state for the ace component - we only need a reference to the editor,
-- | as Ace editor has its own internal state that we can query instead of
-- | replicating it within Halogen.
type AceState = { editor :: Maybe Editor, editedContextName :: ContextNameInText }

type Theme = String
type Mode = String

-- | A basic query algebra for the Ace component.
data AceQuery a
  = Initialize Mode Theme a
  | Finalize a
  | ChangeText String a
  | HandleChange (H.SubscribeStatus -> a) -- This is a Request, where: type Request f a = (a -> a) -> f a
  | SetAnnotations (Array AceError) a
  | ClearAnnotations a
  | HandleContextRenaming DocumentEvent (H.SubscribeStatus -> a)

data AceOutput = TextChanged String

-- | Effects embedding the Ace editor requires.
type AceEffects eff = (ace :: ACE, avar :: AVAR, console :: CONSOLE | eff)

-- | The Ace component definition.
aceComponent ::  forall eff. Mode -> Theme -> H.Component HH.HTML AceQuery Unit AceOutput (Aff (AceEffects (DomeinFileEffects eff)))
aceComponent mode theme =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action (Initialize mode theme))
    , finalizer: Just (H.action Finalize)
    , receiver: const Nothing
    }
  where

  initialState :: AceState
  initialState = { editor: Nothing, editedContextName: Nothing }

  -- As we're embedding a 3rd party component we only need to create a
  -- placeholder div here and attach the ref property which will let us reference
  -- the element in eval.
  render :: AceState -> H.ComponentHTML AceQuery
  render = const $ HH.div [ HP.ref (H.RefLabel "ace")] []

  -- The query algebra for the component handles the initialization of the Ace
  -- editor as well as responding to the `ChangeText` action that allows us to
  -- alter the editor's state.
  eval :: AceQuery ~> H.ComponentDSL AceState AceQuery AceOutput (Aff (AceEffects (DomeinFileEffects eff)))
  eval = case _ of
    Initialize mod them next -> do
      H.getHTMLElementRef (H.RefLabel "ace") >>= case _ of
        Nothing -> pure unit
        Just el' -> do
          editor <- H.liftEff $ Ace.editNode el' Ace.ace
          session <- H.liftEff $ Editor.getSession editor
          _ <- H.liftEff $ Session.setMode mod session
          _ <- H.liftEff $ Editor.setTheme them editor
          _ <- H.liftEff $ setUseSoftTabs false session
          H.modify (_ { editor = Just editor })
          document <- H.liftEff $ getDocument session
          -- (Document.onChange document) has the signature:
          -- DocumentEvent -> Eff(ace :: ACE | eff) a -> Eff(ace :: ACE | eff) Unit
          -- This fits with the eventSource signature's first argument. Hence, we have to
          -- supply a second argument with the signature:
          -- (a -> Maybe (f SubscribeStatus))
          -- where a is the event handler argument, in this case a DocumentEvent.
          -- (HandleContextRenaming docEvent) has type:
          -- (SubscribeStatus -> a) -> AceQuery a
          -- and (H.request $ HandleContextRenaming docEvent) is AceQuery a.
          -- We then merely have to apply Just to comply with the second argument type.
          H.subscribe $ H.eventSource
            (Document.onChange document)
            (\docEvent -> Just $ H.request $ HandleContextRenaming docEvent)
          -- selection <- liftEff $ Selection.create session
          -- _ <- liftEff $ Selection.onChangeCursor selection (showCursorEvent document selection)
          -- anchor <- liftEff $ Anchor.create document 0 0
          -- _ <- liftEff (Anchor.onChange anchor (showAnchorEvent session))
          H.subscribe $ H.eventSource_ (Session.onChange session) (H.request HandleChange)
      pure next
    Finalize next -> do
      -- Release the reference to the editor and do any other cleanup that a
      -- real world component might need.
      H.modify (_ { editor = Nothing })
      pure next
    ChangeText text next -> do
      maybeEditor <- H.gets _.editor
      case maybeEditor of
        Nothing -> pure unit
        Just editor -> do
          -- session <- H.liftEff $ Editor.getSession editor
          -- backgroundTokenizer <- H.liftEff $ Session.getBackgroundTokenizer session
          -- _ <- H.liftEff $ BackgroundTokenizer.setTokenizer tokenizer backgroundTokenizer
          current <- H.liftEff $ Editor.getValue editor
          when (text /= current) do
            void $ H.liftEff $ Editor.setValue text Nothing editor
      H.raise $ TextChanged text
      pure next
    HandleChange reply -> do
      maybeEditor <- H.gets _.editor
      case maybeEditor of
        Nothing -> pure unit
        Just editor -> do
          session <- H.liftEff $ Editor.getSession editor
          (Position {row :r, column}) <- H.liftEff $ Editor.getCursorPosition editor
          line <- H.liftEff $ getLine r session
          previousLine <- case r == 0 of
            true -> pure ""
            otherwise -> H.liftEff $ getLine (r-1) session
          errors <- H.liftAff $ errorsIn previousLine line
          case errors of
            (Just annotations) -> H.liftEff $ setAnnotations ((\a@{row} -> a {row = row + r}) <$> annotations) session
            Nothing -> do
              _ <- H.liftEff $ clearAnnotations session
              text <- H.liftEff (Editor.getValue editor)
              H.raise $ TextChanged text
      pure (reply H.Listening)
    SetAnnotations annotations next -> do
      maybeEditor <- H.gets _.editor
      case maybeEditor of
        Nothing -> pure next
        Just editor -> do
          session <- H.liftEff $ Editor.getSession editor
          _ <- H.liftEff $ setAnnotations annotations session
          pure next
    ClearAnnotations next -> do
      maybeEditor <- H.gets _.editor
      case maybeEditor of
        Nothing -> pure next
        Just editor -> do
          session <- H.liftEff $ Editor.getSession editor
          _ <- H.liftEff $ clearAnnotations session
          pure next
    HandleContextRenaming event@(DocumentEvent{action, lines}) reply -> do
      maybeEditor <- H.gets _.editor
      case maybeEditor of
        Nothing -> pure (reply H.Listening)
        Just editor -> do
          session <- H.liftEff $ Editor.getSession editor
          document <- H.liftEff $ getDocument session
          (originalLines :: Array String) <- H.liftEff $ recoverOriginalLines document event
          parseResult <- liftAff $ runIndentParser (unsafePartial $ AP.head $ originalLines) CRP.contextDeclaration
          case parseResult of
            (Right (ContextDeclaration _ (Expanded _ contextName) _)) -> do
              case action of
                Insert -> case AR.length lines == 1 of
                  true -> H.modify (_ { editedContextName = Just {name: contextName, line: indexOfFirstOriginalLine event} })
                  false -> do
                    H.liftAff $ rename contextName document event (indexOfFirstOriginalLine event)
                    H.modify (_ { editedContextName = Nothing })
                Remove -> do
                  H.modify (_ { editedContextName = Just {name: contextName, line: indexOfFirstOriginalLine event} })
              pure (reply H.Listening)
            otherwise -> do
              editedContextName <- H.gets _.editedContextName
              case editedContextName of
                Nothing -> pure (reply H.Listening)
                (Just { name, line}) -> case line == indexOfFirstEditedLine event of
                  true -> pure (reply H.Listening)
                  false -> do
                    _ <- H.liftAff $ rename name document event line
                    H.modify (_ { editedContextName = Nothing })
                    pure (reply H.Listening)

      where
        tokenizer :: Tokenizer
        tokenizer = makeTokenizer {getLineTokens : mkFn3 getLineTokens}
        getLineTokens :: String -> String -> Int ->  { tokens :: Array AceTokenInfo, state :: String }
        getLineTokens line state row = { tokens: [{type: "text", value: line}], state: "start"}
        recoverOriginalLines :: forall e. Document -> DocumentEvent -> Eff (ace :: ACE | e) (Array String)
        recoverOriginalLines doc (DocumentEvent {action, start: (Position{row, column}), end: (Position{row: r, column: c}), lines}) =
          case action of
            Insert -> do
              lb <- Document.getLine row doc
              a <- pure $ take column lb
              le <- Document.getLine r doc
              b <- pure $ drop c le
              pure [a <> b]
            Remove -> do -- de eerste regel is toegevoegd als laatste regel in het resultaat.
              l <- Document.getLine row doc
              {before, after} <- pure $ if (length l == column) then {before: l, after: ""} else unsafePartial $ fromJust $ splitAt column l
              firstLine <- pure $ unsafePartial $ AP.head lines
              lines' <- pure $ AR.cons (before <> firstLine) (unsafePartial AP.tail lines)
              lastLine <- pure $ unsafePartial $ AP.last lines'
              pure $ AR.snoc (AR.dropEnd 1 lines') (lastLine <> after)
        indexOfFirstOriginalLine :: DocumentEvent -> Int
        indexOfFirstOriginalLine (DocumentEvent {start: (Position{row})}) = row
        indexOfFirstEditedLine :: DocumentEvent -> Int
        indexOfFirstEditedLine (DocumentEvent {action, start: (Position{row}), end: (Position{row: r})}) =
          case action of
            Insert -> r
            Remove -> row
        rename :: forall e. String -> Document -> DocumentEvent -> Int -> Aff (AceEffects (DomeinFileEffects e)) Unit
        rename contextName document event line = do
          modifiedDeclaration <- H.liftEff $ Document.getLine line document
          -- TODO We need to apply runIndentParser to the current default namespace here!
          parseResult' <- liftAff $ runIndentParser modifiedDeclaration CRP.contextDeclaration
          case parseResult' of
            (Right (ContextDeclaration _ (Expanded _ newName) _)) -> do
              qualifiedName <- composeFullName (indexOfFirstOriginalLine event) newName
              log $ "The fully qualified new name is: " <> qualifiedName
              -- actually rename contextName newName
              pure unit
            otherwise -> pure unit
          where
            composeFullName :: Int -> String -> Aff (AceEffects (DomeinFileEffects e)) String
            composeFullName row name = do
              parseResult <- runIndentParser name CRP.contextName
              case parseResult of
                (Right (Expanded _ name')) -> pure name'
                (Left _) -> do
                  maybeName <- findPreviousContextDeclaration row
                  case maybeName of
                    (Just (Tuple previousRow namespacingName)) -> composeFullName previousRow (namespacingName <> name)
                    Nothing -> pure "" -- we should throw an error here.
            -- TODO. Stop at the line that is not indexed. Then use its namespace, too!
            findPreviousContextDeclaration :: Int -> Aff (AceEffects (DomeinFileEffects e)) (Maybe (Tuple Int String))
            findPreviousContextDeclaration row | row < 0 = pure Nothing
            findPreviousContextDeclaration row | otherwise = do
              previousLine <- liftEff $ Document.getLine row document
              -- TODO We need to apply runIndentParser to the current default namespace here!
              parseResult <- runIndentParser previousLine CRP.contextDeclaration
              case parseResult of
                (Right (ContextDeclaration _ (Expanded _ cname) _)) -> pure $ Just (Tuple row cname)
                otherwise -> findPreviousContextDeclaration (row - 1)

-- showAnchorEvent :: forall e. EditSession ->  AnchorEvent -> Eff (ace :: ACE, console :: CONSOLE | e) Unit
-- showAnchorEvent session {old: (Position{row, column}), value: (Position{row: row1, column: column1})} = do
--   line <- getLine row1 session
--   log ("Current line is: " <> show row1 <> ", its text is: " <> line)
--
-- showCursorEvent :: forall e. Document -> Selection -> Eff (ace :: ACE, console :: CONSOLE | e) Unit
-- showCursorEvent document selection = do
--   (Position{row, column}) <- Selection.getCursor selection
--   line <- Document.getLine row document
--   log ("xxxCurrent line is: " <> show row <> ", its text is: " <> line)
--
-- -- The lines are an array of the mutation. It may be a number of lines in case of paste or delete selection.
-- -- It will be a single character when typing.
-- showDocumentEvent :: forall e. DocumentEvent -> Eff (ace :: ACE, console :: CONSOLE | e) Unit
-- showDocumentEvent (DocumentEvent {action, start: (Position{row, column}), end: (Position{row: r, column: c}), lines}) =
--   log ( showDocumentEventType action <> ": starting at (" <> show row <> "," <> show column <> "), the lines are: " <> show lines <> ". It ends at: ("  <> show r <> "," <> show c <> ")" )

foreign import makeTokenizer :: forall a. a -> Tokenizer

type AceTokenInfo = { type :: String, value :: String}
