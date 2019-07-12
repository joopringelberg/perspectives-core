module Perspectives.SaveUserData where

import Control.Monad.State (StateT)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for_)
import Effect.Class (liftEffect)
import Perspectives.Actions (removeRol, tearDownBotActions, updatePerspectEntiteit', updatePerspectEntiteitMember')
import Perspectives.ContextAndRole (context_id, removeRol_binding, removeRol_gevuldeRollen, rol_id)
import Perspectives.CoreTypes (MP, MonadPerspectives, TripleRef(..))
import Perspectives.DataTypeObjectGetters (iedereRolInContext)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Identifiers (binnenRol, buitenRol) as ID
import Perspectives.Instances (getPerspectEntiteit, removeEntiteit)
import Perspectives.Instances (saveEntiteitPreservingVersion)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol(..))
import Perspectives.TheoryChange (addToQueue, addTripleToQueue, tripleRefToTripleQueueElement, tripleToTripleQueueElement)
import Perspectives.TripleAdministration (unRegisterBasicTriple, unRegisterSubject)
import Prelude (Unit, bind, discard, map, pure, unit, void, ($), (>>=), (>>>), (<<<))

type UserDataState = Array ID

type MonadSaveUserData = StateT UserDataState MonadPerspectives

saveDomeinFileAsUserData :: DomeinFile -> MonadPerspectives Unit
saveDomeinFileAsUserData (DomeinFile{contexts, roles}) = do
  for_ contexts (context_id >>> saveEntiteitPreservingVersion :: ID -> MP PerspectContext)
  for_ roles (rol_id >>> saveEntiteitPreservingVersion :: ID -> MP PerspectRol)

saveUserContext :: ID -> MonadPerspectives Unit
saveUserContext id = do
  (_ :: PerspectContext) <- saveEntiteitPreservingVersion id
  rollen <- iedereRolInContext id
  for_ rollen \(rol :: String) -> saveEntiteitPreservingVersion rol :: MonadPerspectives PerspectRol
  (_ :: PerspectRol) <- saveEntiteitPreservingVersion (ID.buitenRol id)
  (_ :: PerspectRol) <- saveEntiteitPreservingVersion (ID.binnenRol id)
  pure unit

-- TODO:
-- * remove tripleAdministration.
removeUserContext :: ID -> MonadPerspectives Unit
removeUserContext id = do
  tearDownBotActions id
  (_ :: PerspectContext) <- getPerspectEntiteit id
  rollen <- iedereRolInContext id
  for_ rollen \(rol :: String) -> removeUserRol_ rol
  void $ removeUserRol_ (ID.buitenRol id)
  (_ :: PerspectRol) <- removeEntiteit (ID.binnenRol id)
  (_ :: PerspectContext) <- removeEntiteit id
  -- For this subject, for all predicates, unregister the BasicTriples and enter them
  -- into the TripleQueue to further propagate the consequences.
  void $ liftEffect $ unRegisterSubject id >>=
        map tripleRefToTripleQueueElement >>> addToQueue >>> pure

-- Removes the rol from the cache and from the database.
-- Removes the rol from the inverse administration of its binding.
-- Removes the rol as binding from all its binders.
removeUserRol_ :: String -> MonadPerspectives PerspectRol
removeUserRol_ pr = do
  -- Remove from couchdb, remove from the cache.
  rl@(PerspectRol{context, gevuldeRollen, binding, pspType}) <- removeEntiteit pr :: MonadPerspectives PerspectRol
  -- Remove the rol from the inverse administration of its binding.
  case binding of
    Nothing -> pure unit
    (Just ob) -> updatePerspectEntiteitMember' removeRol_gevuldeRollen ob pspType pr
  -- Now handle all Roles that have this Role as binding.
  -- For all Role types,
  forWithIndex_ gevuldeRollen \rol filledRollen ->
    -- for each filledRol instance:
    for_ filledRollen \filledRol -> do
      -- Remove the basic triple with the filledRol as subject and the binding from the Triple Administration.
      -- "model:Perspectives$binding" is used to track bindings in the Triple Administration.
      mt <- liftEffect $ unRegisterBasicTriple $ TripleRef{subject: filledRol, predicate: "model:Perspectives$binding"}
      maybe (pure unit) (addTripleToQueue <<< tripleToTripleQueueElement) mt
      -- Then remove the binding from the filledRol.
      updatePerspectEntiteit'
        ((\_ (r :: PerspectRol) -> removeRol_binding r) )
        filledRol
        ""
  -- Remove the triple that holds the Role as subject. Add all triples to the queue for propagation.
  void $ liftEffect $ unRegisterSubject pr >>=
        map tripleRefToTripleQueueElement >>> addToQueue >>> pure
  pure rl

removeUserRol :: String -> MonadPerspectives Unit
removeUserRol pr = do
  (PerspectRol{pspType, context}) <- removeUserRol_ pr
  void $ removeRol pspType pr context
