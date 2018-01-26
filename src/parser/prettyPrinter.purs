module Perspectives.PrettyPrinter where

import Control.Monad (class Monad)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.State (StateT, get, modify)
import Control.Monad.State.Trans (evalStateT)
import Control.Monad.Writer (WriterT)
import Control.Monad.Writer.Trans (runWriterT, tell)
import Data.Array (catMaybes, elemIndex, replicate, sortBy)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.StrMap (StrMap, foldM, values)
import Data.String (fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (snd)
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (compareOccurrences, context_binnenRol, context_buitenRol, context_comments, context_displayName, context_id, context_pspType, context_rolInContext, rol_binding, rol_comments, rol_context, rol_id, rol_properties, rol_pspType)
import Perspectives.Identifiers (isInNamespace, roleIndexNr)
import Perspectives.Property (PropDefsEffects)
import Perspectives.PropertyComposition ((>->))
import Perspectives.QueryCombinators (ignoreCache)
import Perspectives.Resource (getContext, getRole)
import Perspectives.Syntax (Comment, Comments(..), ID, PerspectContext, PerspectRol(..), PropertyName, PropertyValueWithComments, propertyValue)
import Perspectives.SystemQueries (binding, rolContext, rolTypen)
import Perspectives.TripleAdministration (Triple(..), tripleObjects)
import Perspectives.TripleGetter (constructRolGetter, (##))
import Prelude (Unit, bind, discard, id, join, pure, unit, ($), (*>), (+), (-), (<<<), (<>), (==))

type IndentLevel = Int

type PerspectText' e = StateT IndentLevel (WriterT String (Aff (PropDefsEffects e)))

type PerspectText e = PerspectText' e Unit

type PrettyPrinter a e = a -> PerspectText e

-- | Indents the line and prints the string s.
identifier :: forall e. PrettyPrinter String e
identifier s = do
  i <- get
  tell $ (fromCharArray (replicate i '\t')) <> s <> " "

-- | Just prints the string s, followed by a blank space.
identifier' :: forall e. PrettyPrinter String e
identifier' s = tell $ s <> " "

simpleValue :: forall e. PrettyPrinter (Array String) e
simpleValue [v] = identifier' v
simpleValue a = do
  _ <- traverse identifier' a
  pure unit

space :: forall e. PerspectText e
space = tell " "

newline :: forall e. PerspectText e
newline = tell "\n"

-- | Indents and prints the comment, followed by a newline.
comment :: forall e. PrettyPrinter Comment e
comment c = identifier ( "--" <> c) *> newline

-- | Just prints the comment, preceded by a space and "--".
comment' :: forall e. PrettyPrinter Comment e
comment' c = identifier' ( "--" <> c)

-- prettyPrintContext :: PerspectContext -> String
-- prettyPrintContext c = snd (unwrap (runWriterT $ evalStateT (context c) 0))

prettyPrint :: forall a e. a -> PrettyPrinter a e -> Aff (PropDefsEffects e) String
prettyPrint t pp = prettyPrint' $ pp t

prettyPrint' :: forall e. PerspectText e -> Aff (PropDefsEffects e) String
prettyPrint' t = do
  x <- runWriterT (evalStateT t 0)
  pure $ snd x

indent :: forall a e. PrettyPrinter a e -> PrettyPrinter a e
indent p a = do
  _ <- modify (\i -> i + 1)
  result <- p a
  _ <- modify (\i -> i - 1)
  pure result

withComments :: forall a e f. (a -> Comments f) -> PrettyPrinter a e -> PrettyPrinter a e
withComments getComments p el = do
  let (Comments{commentBefore, commentAfter}) = getComments el
  traverse_ comment commentBefore
  p el
  space
  traverse_ comment' commentAfter
  newline

-- Prints each of the comments before on a separate line. Then executes the given PerspectText
-- and follows that with the comment after, terminated by a newline.
withComments' :: forall e f. Comments f -> PerspectText e -> PerspectText e
withComments' (Comments{commentBefore, commentAfter}) p = do
  traverse_ comment commentBefore
  p
  traverse_ comment' commentAfter
  newline

getCommentBefore :: forall f. Comments f -> Array Comment
getCommentBefore (Comments {commentBefore}) = commentBefore

getCommentAfter :: forall f. Comments f -> Array Comment
getCommentAfter (Comments {commentAfter}) = commentAfter

property :: forall e. PerspectText e -> PropertyName -> PropertyValueWithComments -> PerspectText e
property keyword prop = indent (\pvcomments -> do
  withComments' pvcomments
    (keyword *> identifier' (prop <> " =") *> simpleValue (propertyValue pvcomments)))

publicProperty :: forall e. PropertyName -> PropertyValueWithComments -> PerspectText e
publicProperty = property (identifier "extern")

privateProperty :: forall e. PropertyName -> PropertyValueWithComments -> PerspectText e
privateProperty = property (identifier "intern")

roleProperty :: forall e. PropertyName -> PropertyValueWithComments -> PerspectText e
roleProperty = property (pure unit)

contextDeclaration :: forall e. PerspectContext -> PerspectText e
contextDeclaration x = identifier (context_pspType x) *> identifier' (context_displayName x)

context :: forall e. Array ID -> PrettyPrinter PerspectContext e
context definedResources c = do
  withComments context_comments contextDeclaration c
  publicProperties
  privateProperties
  -- Sort the roles according to, first, their type, second, their occurrence.
  (bindings :: Array (Maybe (PerspectRol))) <- traverse (liftAff <<< getRole) (join (values (context_rolInContext c)))
  traverse_ (indent roleBinding) (sortBy compareOccurrences (catMaybes bindings))
  where

    publicProperties = do
      -- LET OP: de buitenrol is geen integraal onderdeel van de context!
      maybeBuitenRol <- liftAff $ getRole $ context_buitenRol c
      case maybeBuitenRol of
        (Just buitenRol) -> strMapTraverse_ publicProperty (rol_properties buitenRol)
        Nothing -> pure unit

    -- TODO: vervang de pattern matching zodra de binnenRol een 'echte' rol is.
    privateProperties = strMapTraverse_ privateProperty (rol_properties (context_binnenRol c))

    roleBinding :: PerspectRol -> PerspectText e
    roleBinding role = do
      -- NB: This is the role of the context - not yet its binding!
      let (binding :: ID) = (unsafePartial $ fromJust (rol_binding role))
      let (occurrence :: String) = maybe "" id (roleIndexNr (rol_id role))
      case elemIndex binding definedResources of
        Nothing -> do -- binding is not a BuitenRol of a context defined at top level in the Text.
          maybeBinding <- liftAff $ getRole binding
          case maybeBinding of
            Nothing -> comment $ "Binding does not exist: " <> binding -- Error situation!
            (Just binding@(PerspectRol bindingProperties)) -> do
              case rol_pspType binding == "model:Perspectives$BuitenRol" of
                true -> do -- The binding is a BuitenRol of some context.
                  maybeContext <- liftAff $ getContext $ rol_context binding
                  case maybeContext of
                    Nothing -> reference role
                    (Just contxt) -> do
                      withComments' (rol_comments role) (identifier $ (rol_pspType role) <> " => ")
                      indent (context definedResources) contxt
                false -> reference binding -- The binding is a RoleInContext of some context.
        otherwise -> do
          maybeBinding <- liftAff $ getRole binding
          case maybeBinding of
            Nothing -> comment $ "Binding does not exist: " <> binding
            (Just buitenRol) -> withComments' (rol_comments role) (identifier $ (rol_pspType role) <> " => " <> rol_context buitenRol)

      strMapTraverse_ roleProperty (rol_properties role)

    reference :: PerspectRol -> PerspectText e
    reference r = withComments' (rol_comments r) (identifier $ (rol_pspType r) <> " => " <> (maybe "" id (rol_binding r)))

strMapTraverse_ :: forall a m. Monad m => (String -> a -> m Unit) -> StrMap a -> m Unit
strMapTraverse_ f map = foldM (\z s a -> f s a) unit map

enclosingContext :: forall e. PrettyPrinter PerspectContext e
enclosingContext theText = do
  -- TODO. Merk op dat we hier niet over de prefixes beschikken. Dat zijn namelijk eigenschappen van de tekst!
  withComments' (context_comments theText) (identifier( "Context " <> (context_displayName theText)))
  newline
  sectionIds <- liftAff ((context_id theText) ## (ignoreCache rolTypen))
  traverse_ section (tripleObjects sectionIds)

  where
    section :: ID -> PerspectText e
    section sectionId = do
      identifier' "Section"
      identifier sectionId
      newline
      newline
      (Triple{object: definedContexts}) <- liftAff ((context_id theText) ## ignoreCache ((constructRolGetter sectionId) >-> binding)) -- These are all a buitenRol.
      (contextIds :: Triple e) <- liftAff ((context_id theText) ## ignoreCache ((constructRolGetter sectionId) >-> binding >-> rolContext)) -- For each of these buitenRollen, this is the context represented by it.
      traverse_ (ppContext definedContexts) (tripleObjects contextIds)

    -- TODO We willen alleen de contexten expanderen die lexicaal genest zijn ten opzicht van theText.
    -- Van andere willen we alleen de declaratie tonen.
    ppContext :: Array ID -> ID -> PerspectText e
    ppContext definedContexts id = do
      mc <- liftAff $ getContext id
      case mc of
        (Just c) -> do
          if isInNamespace (context_id theText) id
            then do
              withComments context_comments contextDeclaration c
              newline
            else do
              context definedContexts c
              newline
        Nothing -> pure unit
