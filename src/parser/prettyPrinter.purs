module Perspectives.PrettyPrinter where

import Control.Monad (class Monad)
import Control.Monad.State (StateT, get, modify, lift)
import Control.Monad.State.Trans (evalStateT)
import Control.Monad.Writer (WriterT)
import Control.Monad.Writer.Trans (runWriterT, tell)
import Data.Array (elemIndex, replicate, sortBy)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (unwrap)
import Foreign.Object (Object, foldM, values) as F
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (snd)
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (compareOccurrences, context_Namespace, context_binnenRol, context_buitenRol, context_comments, context_displayName, context_id, context_pspType, context_rolInContext, rol_binding, rol_comments, rol_context, rol_id, rol_properties, rol_pspType)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DataTypeTripleGetters (binding, context, typeVanIedereRolInContext) as DTG

import Perspectives.EntiteitAndRDFAliases (Comment, ID, PropertyName)
import Perspectives.Identifiers (isInNamespace, roleIndexNr)
import Perspectives.PerspectivesTypes (BuitenRol(..), RolDef(..))
import Perspectives.QueryCombinators (ignoreCache)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Perspectives.Syntax (Comments(..), PerspectContext, PerspectRol(..), PropertyValueWithComments(..), propertyValue)
import Perspectives.TripleGetterComposition ((>->))
import Perspectives.TripleGetterConstructors (getContextRol)
import Prelude (Unit, bind, discard, identity, join, pure, unit, ($), (*>), (+), (-), (<<<), (<>), (==), (||))

type IndentLevel = Int

type PerspectTextM m = StateT IndentLevel (WriterT String m)

type PerspectText' = PerspectTextM MonadPerspectives

type PerspectText = PerspectText' Unit

type PrettyPrinter a = a -> PerspectText

-- | Indents the line and prints the string s.
identifier :: PrettyPrinter String
identifier s = do
  i <- get
  tell $ (fromCharArray (replicate i '\t')) <> s <> " "

-- | Just prints the string s, followed by a blank space.
identifier' :: PrettyPrinter String
identifier' s = tell $ s <> " "

simpleValue :: PrettyPrinter (Array String)
simpleValue [v] = identifier' v
simpleValue a = do
  _ <- traverse identifier' a
  pure unit

space :: PerspectText
space = tell " "

newline :: PerspectText
newline = tell "\n"

-- | Indents and prints the comment, followed by a newline.
comment :: PrettyPrinter Comment
comment c = identifier ( "--" <> c) *> newline

-- | Just prints the comment, preceded by a space and "--".
comment' :: PrettyPrinter Comment
comment' c = identifier' ( "--" <> c)

-- prettyPrintContext :: PerspectContext -> String
-- prettyPrintContext c = snd (unwrap (runWriterT $ evalStateT (context c) 0))

prettyPrint :: forall a. a -> PrettyPrinter a -> MonadPerspectives  String
prettyPrint t pp = runPerspectText $ pp t

runPerspectText :: PerspectText -> MonadPerspectives  String
runPerspectText t = do
  x <- runWriterT (evalStateT t 0)
  pure $ snd x

indent :: forall a. PrettyPrinter a -> PrettyPrinter a
indent p a = do
  _ <- modify (\i -> i + 1)
  result <- p a
  _ <- modify (\i -> i - 1)
  pure result

withComments :: forall a. (a -> Comments) -> PrettyPrinter a -> PrettyPrinter a
withComments getComments p el = do
  let (Comments{commentBefore, commentAfter}) = getComments el
  traverse_ comment commentBefore
  p el
  space
  traverse_ comment' commentAfter
  newline

-- Prints each of the comments before on a separate line. Then executes the given PerspectText
-- and follows that with the comment after, terminated by a newline.
withComments' :: Comments -> PerspectText -> PerspectText
withComments' (Comments{commentBefore, commentAfter}) p = do
  traverse_ comment commentBefore
  p
  traverse_ comment' commentAfter
  newline

getCommentBefore :: Comments -> Array Comment
getCommentBefore (Comments {commentBefore}) = commentBefore

getCommentAfter :: Comments -> Array Comment
getCommentAfter (Comments {commentAfter}) = commentAfter

property :: PerspectText -> PropertyName -> PropertyValueWithComments -> PerspectText
property keyword prop = indent (\pvcomments ->
  do
    withPVComments pvcomments
      (keyword *> identifier' (prop <> " =") *> simpleValue (propertyValue pvcomments)))
  where
    withPVComments :: PropertyValueWithComments -> PerspectText -> PerspectText
    withPVComments (PropertyValueWithComments{commentBefore, commentAfter}) p = do
      traverse_ comment commentBefore
      p
      traverse_ comment' commentAfter
      newline

publicProperty :: PropertyName -> PropertyValueWithComments -> PerspectText
publicProperty = property (identifier "extern")

privateProperty :: PropertyName -> PropertyValueWithComments -> PerspectText
privateProperty = property (identifier "intern")

roleProperty :: PropertyName -> PropertyValueWithComments -> PerspectText
roleProperty = property (identifier "")

contextDeclaration :: PerspectContext -> PerspectText
contextDeclaration x = identifier (context_pspType x) *> identifier' ("$" <> (context_displayName x))

fullContextDeclaration :: PerspectContext -> PerspectText
fullContextDeclaration x = identifier (context_pspType x) *> identifier' (context_Namespace x <> "$" <> (context_displayName x))

context :: Array BuitenRol -> PrettyPrinter PerspectContext
context definedResources c = do
  withComments context_comments contextDeclaration c
  publicProperties
  privateProperties
  -- Sort the roles according to, first, their type, second, their occurrence.
  (bindings :: Array PerspectRol) <- traverse (lift <<< lift <<< getPerspectEntiteit) (join (F.values (context_rolInContext c)))
  traverse_ (indent roleBinding) (sortBy compareOccurrences bindings)
  where

    publicProperties = do
      -- LET OP: de buitenrol is geen integraal onderdeel van de context!
      buitenRol <- lift $ lift $ getPerspectEntiteit $ context_buitenRol c
      strMapTraverse_ publicProperty (rol_properties buitenRol)

    privateProperties = do
      binnenRol <- lift $ lift $ getPerspectEntiteit $ context_binnenRol c
      strMapTraverse_ privateProperty (rol_properties binnenRol)

    roleBinding :: PerspectRol -> PerspectText
    roleBinding role = do
      -- NB: This is the role of the context - not yet its binding!
      let (binding :: ID) = (unsafePartial $ fromJust (rol_binding role))
      let (occurrence :: String) = maybe "" identity (roleIndexNr (rol_id role))
      case elemIndex (BuitenRol binding) definedResources of
        -- binding is NOT a BuitenRol of a context defined at top level in the Text.
        Nothing -> do
              boundRol@(PerspectRol bindingProperties) <- lift $ lift $ getPerspectEntiteit binding
              case rol_pspType boundRol == "model:Perspectives$Context$buitenRol" of
                -- boundRol is a BuitenRol of some context.
                true -> if isInNamespace (context_id c) (rol_context boundRol)
                  -- boundRol is in the namespace of context c
                  then
                    do
                      contxt <- lift $ lift $ getPerspectEntiteit $ rol_context boundRol
                      withComments' (rol_comments role) (identifier $ (rol_pspType role) <> " => ")
                      -- Only if in namespace of c!
                      indent (context definedResources) contxt
                  -- boundRol is not in the namespace of context c.
                  else reference role boundRol
                false -> reference role boundRol -- The boundRol is a RoleInContext of some context.
        -- binding is a BuitenRol of a context defined at top level in the Text.
        otherwise -> do
          buitenRol <- lift $ lift $ getPerspectEntiteit binding
          reference role buitenRol

      strMapTraverse_ roleProperty (rol_properties role)

    reference :: PerspectRol -> PerspectRol -> PerspectText
    reference role binding = withComments' (rol_comments role) (identifier $ (rol_pspType role) <> " => " <> (rol_context binding))

strMapTraverse_ :: forall a m. Monad m => (String -> a -> m Unit) -> F.Object a -> m Unit
strMapTraverse_ f map = F.foldM (\z s a -> f s a) unit map

enclosingContext :: PrettyPrinter PerspectContext
enclosingContext theText = do
  -- TODO. Merk op dat we hier niet over de prefixes beschikken. Dat zijn namelijk eigenschappen van de tekst!
  withComments' (context_comments theText) (identifier ("Context " <> (context_displayName theText)))
  newline
  sectionIds <- lift $ lift ((context_id theText) ##= (ignoreCache DTG.typeVanIedereRolInContext))
  traverse_ (section <<< RolDef) sectionIds

  where
    section :: RolDef -> PerspectText
    section sectionId = do
      identifier' "Section"
      identifier $ unwrap sectionId
      newline
      newline
      (definedContexts :: Array BuitenRol) <- lift $ lift ((context_id theText) ##= ignoreCache ((getContextRol sectionId) >-> DTG.binding))
      contextIds <- lift $ lift ((context_id theText) ##= ignoreCache ((getContextRol sectionId) >-> DTG.binding >-> DTG.context)) -- For each of these buitenRollen, this is the ID of the context represented by it.
      traverse_ (ppContext definedContexts) contextIds

    ppContext :: Array BuitenRol -> ID -> PerspectText
    ppContext definedContexts id = do
      c <- lift $ lift $ getPerspectEntiteit id
      if id `isInNamespace` (context_id theText) || id `isInNamespace` "model:User"
        then do
          context definedContexts c
          newline
        else do
          withComments context_comments fullContextDeclaration c
          newline
