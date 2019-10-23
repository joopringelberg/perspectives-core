module Perspectives.CollectDomeinFile where

import Control.Monad.State (StateT, execStateT, lift, modify, get)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Foreign.Object (lookup)
import Perspectives.ContextAndRole (context_buitenRol, context_rev)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DataTypeTripleGetters (iedereRolInContext) as DTG
import Perspectives.DomeinFile (DomeinFile(..), defaultDomeinFile)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Representation.Class.Persistent (getPerspectType, addContextToDomeinFile, addEnumeratedRoleToDomeinFile)
import Perspectives.Representation.TypeIdentifiers (RoleType)
import Perspectives.Identifiers (isInNamespace)
import Perspectives.InstanceRepresentation (PerspectContext)
import Perspectives.Instances (getPerspectEntiteit)
import Perspectives.ModelBasedTripleGetters (boundContexts)
import Perspectives.PerspectivesTypes (ContextDef)
import Perspectives.Representation.Class.Persistent (identifier)
import Perspectives.Representation.Class.Revision (rev)
import Perspectives.Representation.Context (Context(..))
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Prelude (Unit, bind, const, discard, flip, ifM, pure, unit, void, ($), (<<<), (==), (>>=))

-- | From a context, create a DomeinFile.
domeinFileFromContext :: Context -> MonadPerspectives DomeinFile
domeinFileFromContext enclosingContext = do
  (DomeinFile df) <- execStateT (collect enclosingContext false) defaultDomeinFile
  pure $ DomeinFile $ df {_rev = (rev enclosingContext), _id = (context_id enclosingContext)}
  where
    context_id :: Context -> String
    context_id = unwrap <<< identifier

    collect :: Context -> Boolean -> StateT DomeinFile (MonadPerspectives) Unit
    collect c definedAtToplevel =
      ifM (saveContext c definedAtToplevel)
        do
          for_ (unwrap c).contextAspects
            flip saveContext false <<< getPerspectType
          for_ (unwrap c).rolInContext
            saveRol

          -- boundContexts <- lift $ ((context_id c) ##= boundContexts)
          -- for_ boundContexts recursiveCollect
        (pure unit)
      where
        saveRol :: RoleType -> StateT DomeinFile MonadPerspectives Unit
        saveRol rt = ifM (inDomeinFile (unwrap rt))
          (pure unit)
          (getPerspectType rt >>= addEnumeratedRoleToDomeinFile)

        -- On recursively collecting a bound context, it may happen that the binding is not available in cache
        -- and neither is a DomeinFile available that holds a definition. We then accept an error
        -- thrown by getPerspectEntiteit.
        recursiveCollect :: Context -> StateT DomeinFile (MonadPerspectives) Unit
        recursiveCollect subContextId = (lift $ getPerspectType subContextId) >>= (flip collect ((context_id c) == (context_id enclosingContext)))
        saveContext :: Context -> Boolean -> StateT DomeinFile (MonadPerspectives) Boolean
        saveContext ctxt definedAtToplevel' = if (context_id ctxt) `isInNamespace` (context_id enclosingContext)
          then
            ifM (inDomeinFile (context_id ctxt))
              (pure false)
              do
                void $ modify $ addContextToDomeinFile ctxt
                -- As a context and its buitenRol are always created together, we can safely assume the latter exists.
                void $ (lift $ getPerspectType (_.externeRol $ unwrap ctxt)) >>= (modify <<< \rol-> addEnumeratedRoleToDomeinFile rol)
                -- TODO: add all the other roles and the aspects.
                pure true
          else pure false

        inDomeinFile :: ID -> StateT DomeinFile (MonadPerspectives) Boolean
        inDomeinFile id = do
          (DomeinFile {contexts, enumeratedRoles}) <- get
          case lookup id contexts of
            (Just _) -> pure true
            Nothing -> case lookup id enumeratedRoles of
              (Just _) -> pure true
              otherwise -> pure false

        savedToCouchdb :: PerspectContext -> Boolean
        savedToCouchdb ctxt = maybe false (const true) (context_rev ctxt)
