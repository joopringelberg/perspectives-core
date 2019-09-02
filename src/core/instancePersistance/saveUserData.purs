{-
  TODO. Restore the functionality to remove data once Actions is back on line.
-}
module Perspectives.SaveUserData where

import Control.Monad.State (StateT)
import Data.Array (nub)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Traversable (for_)
import Effect.Class (liftEffect)
import Foreign.Object (values)
import Perspectives.ContextAndRole (context_id, context_iedereRolInContext, removeRol_binding, removeRol_gevuldeRollen, rol_id)
import Perspectives.ContextRolAccessors (getContextMember)
import Perspectives.CoreTypes (MP, MonadPerspectives, type (##>))
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Identifiers (binnenRol, buitenRol) as ID
import Perspectives.Identifiers (deconstructBuitenRol)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol(..))
import Perspectives.Instances (getPerspectEntiteit, removeEntiteit)
import Perspectives.Instances (saveEntiteitPreservingVersion)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Prelude (Unit, bind, discard, join, map, pure, unit, void, ($), (<<<), (>>=), (>>>))

type UserDataState = Array ID

type MonadSaveUserData = StateT UserDataState MonadPerspectives

saveDomeinFileAsUserData :: Array ID -> MonadPerspectives Unit
saveDomeinFileAsUserData ids = do
  for_ ids (deconstructBuitenRol >>> saveEntiteitPreservingVersion :: ID -> MP PerspectContext)

saveUserContext :: ID -> MonadPerspectives Unit
saveUserContext id = do
  (_ :: PerspectContext) <- saveEntiteitPreservingVersion id
  rollen <- iedereRolInContext id
  for_ rollen \(rol :: String) -> saveEntiteitPreservingVersion rol :: MonadPerspectives PerspectRol
  (_ :: PerspectRol) <- saveEntiteitPreservingVersion (ID.buitenRol id)
  pure unit

iedereRolInContext :: (ContextInstance ##> RoleInstance)
iedereRolInContext = getContextMember \ctxt -> nub $ join $ values (context_iedereRolInContext ctxt)

-- * remove tripleAdministration.
removeUserContext :: ID -> MonadPerspectives Unit
removeUserContext id = do
  -- tearDownBotActions id
  (_ :: PerspectContext) <- getPerspectEntiteit id
  rollen <- iedereRolInContext id
  for_ rollen \(rol :: String) -> removeUserRol_ rol
  void $ removeUserRol_ (ID.buitenRol id)
  (_ :: PerspectContext) <- removeEntiteit id

  -- Update the queryAssumptionRegister in PerspectivesState.
  -- All assumptions that have this context as a resource, should be removed.
  -- Also, all queries that depend on at least one of these assumptions, should be recomputed.
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
    (Just ob) -> updatePerspectEntiteitMember' removeRol_gevuldeRollen ob (unwrap pspType) pr
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
  void $ removeRol (unwrap pspType) pr context
