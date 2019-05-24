module Perspectives.SaveUserData where

import Control.Monad.State (StateT)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for_)
import Effect.Class (liftEffect)
import Perspectives.Actions (tearDownBotActions, updatePerspectEntiteit', updatePerspectEntiteitMember')
import Perspectives.ContextAndRole (context_id, removeRol_binding, removeRol_gevuldeRollen, rol_id)
import Perspectives.CoreTypes (MP, MonadPerspectives, TripleRef(..))
import Perspectives.DataTypeObjectGetters (iedereRolInContext)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Identifiers (binnenRol, buitenRol) as ID
import Perspectives.Resource (getPerspectEntiteit, removeEntiteit)
import Perspectives.ResourceRetrieval (saveEntiteitPreservingVersion)
import Perspectives.Syntax (PerspectContext, PerspectRol(..))
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
  for_ rollen \(rol :: String) -> removeUserRol rol
  removeUserRol (ID.buitenRol id)
  (_ :: PerspectRol) <- removeEntiteit (ID.binnenRol id)
  (_ :: PerspectContext) <- removeEntiteit id
  -- For this subject, for all predicates, unregister the BasicTriples and enter them
  -- into the TripleQueue to further propagate the consequences.
  void $ liftEffect $ unRegisterSubject id >>=
        map tripleRefToTripleQueueElement >>> addToQueue >>> pure

-- Removes the rol from the cache and from the database.
-- Removes the rol from the inverse administration of its binding.
-- Removes the rol as binding from all its binders.
removeUserRol :: String -> MonadPerspectives Unit
removeUserRol pr = do
  (PerspectRol{context, gevuldeRollen, binding, pspType}) <- removeEntiteit pr :: MonadPerspectives PerspectRol
  case binding of
    Nothing -> pure unit
    (Just ob) -> updatePerspectEntiteitMember' removeRol_gevuldeRollen ob pspType pr
  forWithIndex_ gevuldeRollen \rol filledRollen ->
    for_ filledRollen \filledRol -> do
      -- wat is het triple dat de binding van filledRol representeert?
      -- de ref is natuurlijk TripleRef({subject: filledRol, predicate "model:Perspectives$binding"})
      mt <- liftEffect $ unRegisterBasicTriple $ TripleRef{subject: filledRol, predicate: "model:Perspectives$binding"}
      maybe (pure unit) (addTripleToQueue <<< tripleToTripleQueueElement) mt
      updatePerspectEntiteit'
        ((\_ (r :: PerspectRol) -> removeRol_binding r) )
        filledRol
        ""
  -- Remove the triple that holds the Rol as object. NOTE: when called from
  -- removeUserContext, this is no longer needed.
  -- Remove all triples that hold the Rol as subject.
  -- Add all these triples to the queue.
  t <- liftEffect $ unRegisterBasicTriple (TripleRef{subject: context, predicate: pspType})
  maybe (pure unit) (addTripleToQueue <<< tripleToTripleQueueElement) t
  void $ liftEffect $ unRegisterSubject pr >>=
        map tripleRefToTripleQueueElement >>> addToQueue >>> pure
