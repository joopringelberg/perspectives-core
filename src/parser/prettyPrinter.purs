module Perspectives.PrettyPrinter where

import Control.Monad (class Monad)
import Control.Monad.State (StateT, get, modify, lift)
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
import Perspectives.ContextAndRole (compareOccurrences, context_Namespace, context_binnenRol, context_buitenRol, context_comments, context_displayName, context_id, context_pspType, context_rolInContext, rol_binding, rol_comments, rol_context, rol_id, rol_properties, rol_pspType)
import Perspectives.CoreTypes (Triple(..), MonadPerspectives, tripleObjects)
import Perspectives.RunMonadPerspectivesQuery ((##))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (Comment, ID, PropertyName)
import Perspectives.Identifiers (isInNamespace, roleIndexNr)
import Perspectives.TripleGetterComposition ((>->))
import Perspectives.QueryCombinators (ignoreCache)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.Syntax (Comments(..), PerspectContext, PerspectRol(..), PropertyValueWithComments(..), propertyValue)
import Perspectives.DataTypeTripleGetters (binding, context, typeVanIedereRolInContextM) as DTG
import Perspectives.TripleGetterConstructors (constructRolGetter)
import Prelude (Unit, bind, discard, id, join, pure, unit, ($), (*>), (+), (-), (<<<), (<>), (==), (||))

type IndentLevel = Int

type PerspectTextM m = StateT IndentLevel (WriterT String m)

type PerspectText' e = PerspectTextM (MonadPerspectives (AjaxAvarCache e))

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

prettyPrint :: forall a e. a -> PrettyPrinter a e -> MonadPerspectives (AjaxAvarCache e) String
prettyPrint t pp = runPerspectText $ pp t

runPerspectText :: forall e. PerspectText e -> MonadPerspectives (AjaxAvarCache e) String
runPerspectText t = do
  x <- runWriterT (evalStateT t 0)
  pure $ snd x

indent :: forall a e. PrettyPrinter a e -> PrettyPrinter a e
indent p a = do
  _ <- modify (\i -> i + 1)
  result <- p a
  _ <- modify (\i -> i - 1)
  pure result

withComments :: forall a e. (a -> Comments) -> PrettyPrinter a e -> PrettyPrinter a e
withComments getComments p el = do
  let (Comments{commentBefore, commentAfter}) = getComments el
  traverse_ comment commentBefore
  p el
  space
  traverse_ comment' commentAfter
  newline

-- Prints each of the comments before on a separate line. Then executes the given PerspectText
-- and follows that with the comment after, terminated by a newline.
withComments' :: forall e. Comments -> PerspectText e -> PerspectText e
withComments' (Comments{commentBefore, commentAfter}) p = do
  traverse_ comment commentBefore
  p
  traverse_ comment' commentAfter
  newline

getCommentBefore :: Comments -> Array Comment
getCommentBefore (Comments {commentBefore}) = commentBefore

getCommentAfter :: Comments -> Array Comment
getCommentAfter (Comments {commentAfter}) = commentAfter

property :: forall e. PerspectText e -> PropertyName -> PropertyValueWithComments -> PerspectText e
property keyword prop = indent (\pvcomments ->
  do
    withPVComments pvcomments
      (keyword *> identifier' (prop <> " =") *> simpleValue (propertyValue pvcomments)))
  where
    withPVComments :: PropertyValueWithComments -> PerspectText e -> PerspectText e
    withPVComments (PropertyValueWithComments{commentBefore, commentAfter}) p = do
      traverse_ comment commentBefore
      p
      traverse_ comment' commentAfter
      newline

publicProperty :: forall e. PropertyName -> PropertyValueWithComments -> PerspectText e
publicProperty = property (identifier "extern")

privateProperty :: forall e. PropertyName -> PropertyValueWithComments -> PerspectText e
privateProperty = property (identifier "intern")

roleProperty :: forall e. PropertyName -> PropertyValueWithComments -> PerspectText e
roleProperty = property (identifier "")

contextDeclaration :: forall e. PerspectContext -> PerspectText e
contextDeclaration x = identifier (context_pspType x) *> identifier' ("$" <> (context_displayName x))

fullContextDeclaration :: forall e. PerspectContext -> PerspectText e
fullContextDeclaration x = identifier (context_pspType x) *> identifier' (context_Namespace x <> "$" <> (context_displayName x))

context :: forall e. Array ID -> PrettyPrinter PerspectContext e
context definedResources c = do
  withComments context_comments contextDeclaration c
  publicProperties
  privateProperties
  -- Sort the roles according to, first, their type, second, their occurrence.
  (bindings :: Array (Maybe (PerspectRol))) <- traverse (lift <<< lift <<< getPerspectEntiteit) (join (values (context_rolInContext c)))
  traverse_ (indent roleBinding) (sortBy compareOccurrences (catMaybes bindings))
  where

    publicProperties = do
      -- LET OP: de buitenrol is geen integraal onderdeel van de context!
      maybeBuitenRol <- lift $ lift $ getPerspectEntiteit $ context_buitenRol c
      case maybeBuitenRol of
        (Just buitenRol) -> strMapTraverse_ publicProperty (rol_properties buitenRol)
        Nothing -> pure unit

    privateProperties = strMapTraverse_ privateProperty (rol_properties (context_binnenRol c))

    roleBinding :: PerspectRol -> PerspectText e
    roleBinding role = do
      -- NB: This is the role of the context - not yet its binding!
      let (binding :: ID) = (unsafePartial $ fromJust (rol_binding role))
      let (occurrence :: String) = maybe "" id (roleIndexNr (rol_id role))
      case elemIndex binding definedResources of
        -- binding is NOT a BuitenRol of a context defined at top level in the Text.
        Nothing -> do
          maybeRol <- lift $ lift $ getPerspectEntiteit binding
          case maybeRol of
            Nothing -> comment $ "Binding does not exist: " <> binding -- Error situation!
            (Just boundRol@(PerspectRol bindingProperties)) -> do
              case rol_pspType boundRol == "model:Perspectives$Context$buitenRol" of
                -- boundRol is a BuitenRol of some context.
                true -> if isInNamespace (context_id c) (rol_context boundRol)
                  -- boundRol is in the namespace of context c
                  then
                    do
                      maybeContext <- lift $ lift $ getPerspectEntiteit $ rol_context boundRol
                      case maybeContext of
                        Nothing -> reference role boundRol
                        (Just contxt) -> do
                          withComments' (rol_comments role) (identifier $ (rol_pspType role) <> " => ")
                          -- Only if in namespace of c!
                          indent (context definedResources) contxt
                  -- boundRol is not in the namespace of context c.
                  else reference role boundRol
                false -> reference role boundRol -- The boundRol is a RoleInContext of some context.
        -- binding is a BuitenRol of a context defined at top level in the Text.
        otherwise -> do
          maybeRol <- lift $ lift $ getPerspectEntiteit binding
          case maybeRol of
            Nothing -> comment $ "Binding does not exist: " <> binding
            (Just buitenRol) -> reference role buitenRol

      strMapTraverse_ roleProperty (rol_properties role)

    reference :: PerspectRol -> PerspectRol -> PerspectText e
    reference role binding = withComments' (rol_comments role) (identifier $ (rol_pspType role) <> " => " <> (rol_context binding))

strMapTraverse_ :: forall a m. Monad m => (String -> a -> m Unit) -> StrMap a -> m Unit
strMapTraverse_ f map = foldM (\z s a -> f s a) unit map

enclosingContext :: forall e. PrettyPrinter PerspectContext e
enclosingContext theText = do
  -- TODO. Merk op dat we hier niet over de prefixes beschikken. Dat zijn namelijk eigenschappen van de tekst!
  withComments' (context_comments theText) (identifier ("Context " <> (context_displayName theText)))
  newline
  sectionIds <- lift $ lift ((context_id theText) ## (ignoreCache DTG.typeVanIedereRolInContextM))
  traverse_ section (tripleObjects sectionIds)

  where
    section :: ID -> PerspectText e
    section sectionId = do
      identifier' "Section"
      identifier sectionId
      newline
      newline
      (Triple{object: definedContexts}) <- lift $ lift ((context_id theText) ## ignoreCache ((constructRolGetter sectionId) >-> DTG.binding)) -- These are all a buitenRol.
      (contextIds :: Triple e) <- lift $ lift ((context_id theText) ## ignoreCache ((constructRolGetter sectionId) >-> DTG.binding >-> DTG.context)) -- For each of these buitenRollen, this is the ID of the context represented by it.
      traverse_ (ppContext definedContexts) (tripleObjects contextIds)

    ppContext :: Array ID -> ID -> PerspectText e
    ppContext definedContexts id = do
      mc <- lift $ lift $ getPerspectEntiteit id
      case mc of
        (Just c) -> do
          if id `isInNamespace` (context_id theText) || id `isInNamespace` "model:User"
            then do
              context definedContexts c
              newline
            else do
              withComments context_comments fullContextDeclaration c
              newline
        Nothing -> pure unit
