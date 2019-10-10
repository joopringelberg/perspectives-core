module Perspectives.Parsing.Arc.PhaseThree where

-- | Phase Three of the parser solves problems that arise due to forward reference.
-- | In a View, for example, the modeller can reference a property of a Role that
-- | has not yet been parsed in phase two (this may happen if that Role is filled with
-- | a role that is 'later' in the source text).
-- |

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (StateT, evalState, evalStateT, modify, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Lens (Traversal', set, traversed)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), snd)
import Foreign.Object (empty, insert, lookup, values)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, (###>), MP)
import Perspectives.DependencyTracking.Array.Trans (ArrayT, runArrayT)
import Perspectives.DomeinFile (DomeinFileRecord, defaultDomeinFileRecord)
import Perspectives.Identifiers (isQualifiedWithDomein)
import Perspectives.Parsing.Arc.PhaseTwo (PhaseTwo, PhaseTwoState, evalPhaseTwo_, getDF, modifyDF)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Representation.Action (Action(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.TypeIdentifiers (ActionType(..), CalculatedRoleType(..), EnumeratedRoleType(..), RoleType(..), roletype2string)
import Perspectives.Types.ObjectGetters (lookForRoleType)
import Prelude (Unit, bind, discard, pure, unit, void, (<<<), ($), (<>), (==))

-- | A Monad with state that indicates whether the Subject of an Action is a Bot, and allows exceptions.
type PhaseThree a = ExceptT PerspectivesError (StateT PhaseTwoState MonadPerspectives) a

-- | Run a computation in `PhaseThree`, returning Errors or a Tuple holding both the state and the result of the computation.
runPhaseThree :: forall a. PhaseThree a -> MP (Tuple (Either PerspectivesError a) PhaseTwoState)
runPhaseThree computation = runPhaseThree_ computation defaultDomeinFileRecord

runPhaseThree_ :: forall a. PhaseThree a -> DomeinFileRecord -> MP (Tuple (Either PerspectivesError a) PhaseTwoState)
runPhaseThree_ computation dfr = runStateT (runExceptT computation) {bot: false, dfr: dfr, namespaces: empty}

-- | Run a computation in `PhaseThree`, returning Errors or the result of the computation.
evalPhaseThree :: forall a. PhaseThree a -> MP (Either PerspectivesError a)
evalPhaseThree computation = evalPhaseThree_ computation defaultDomeinFileRecord

evalPhaseThree_ :: forall a. PhaseThree a -> DomeinFileRecord -> MonadPerspectives (Either PerspectivesError a)
evalPhaseThree_ computation drf = evalStateT (runExceptT computation) {bot: false, dfr: drf, namespaces: empty}

lift2 :: forall a. MonadPerspectives a -> PhaseThree a
lift2 = lift <<< lift

phaseThree :: DomeinFileRecord -> MP (Either PerspectivesError DomeinFileRecord)
phaseThree df = do
  (Tuple ei {dfr}) <- runPhaseThree_ (qualifyActionRoles df) df
  case ei of
    (Left e) -> pure $ Left e
    otherwise -> pure $ Right dfr

-- TODO. Deze code is overbodig; qualifyActionRoles doet en passant hetzelfde!
-- The Object and IndirectObject of the Actions in this Context are by default represented as EnumeratedRoleTypes.
-- Here we correct that if necessary.
correctAction :: DomeinFileRecord -> PhaseTwo DomeinFileRecord
correctAction {actions} = do
  for_ (values actions) (unsafePartial correctObject)
  {actions: actions'} <- getDF
  for_ (values actions') (unsafePartial correctIndirectObject)
  getDF

  where
    correctObject :: Partial => Action -> PhaseTwo Unit
    correctObject (Action ar@{_id: (ActionType objId), object: (ENR (EnumeratedRoleType obj))}) = do
      {calculatedRoles} <- getDF
      case lookup obj calculatedRoles of
        (Just _) -> void $ modifyDF (set (_object objId) (CR (CalculatedRoleType obj)))
        otherwise -> pure unit

    _object :: String -> Traversal' DomeinFileRecord RoleType
    _object objId = prop (SProxy :: SProxy "actions") <<< at objId <<< traversed <<< _Newtype <<< prop (SProxy :: SProxy "object")

    correctIndirectObject :: Partial => Action -> PhaseTwo Unit
    correctIndirectObject (Action ar@{_id: (ActionType objId), indirectObject: (Just (ENR (EnumeratedRoleType obj)))}) = do
      {calculatedRoles} <- getDF
      case lookup obj calculatedRoles of
        (Just _) -> modifyDF (set (_indirectObject objId) (Just (CR (CalculatedRoleType obj))))
        otherwise -> pure unit
      pure unit
    correctIndirectObject _ = pure unit

    _indirectObject :: String -> Traversal' DomeinFileRecord (Maybe RoleType)
    _indirectObject objId = prop (SProxy :: SProxy "actions") <<< at objId <<< traversed <<< _Newtype <<< prop (SProxy :: SProxy "indirectObject")

-- | Qualifies the identifiers used in the object- and indirectObject field of an Action.
-- | All Objects are by default constructed as enumerated; this function corrects that if
-- | applicable.
-- | Note that this function requires the DomeinFile to be available in the cache!
qualifyActionRoles :: DomeinFileRecord -> PhaseThree Unit
qualifyActionRoles {contexts, enumeratedRoles, actions, calculatedRoles} = for_ contexts
  \(Context{_id, gebruikerRol, rolInContext, contextRol}) -> for_ gebruikerRol
    \(EnumeratedRoleType ur) -> case lookup ur enumeratedRoles of
      Nothing -> throwError (Custom $ "Impossible error: cannot find '" <> ur <> "' in model.")
      (Just (EnumeratedRole {perspectives})) -> for_ (values perspectives)
        \acts -> for_ acts
          \(ActionType a) -> case lookup a actions of
            Nothing -> throwError (Custom $ "Impossible error: cannot find '" <> a <> "' in model.")
            (Just (Action ar@{_id: actId, object, indirectObject, pos})) -> do
              ar' <- if isQualifiedWithDomein (roletype2string object)
                then case lookup (roletype2string object) calculatedRoles of
                  Nothing -> pure ar
                  (Just (CalculatedRole{_id:id'})) -> pure $ ar {object = CR id'}
                else do
                  mtype <- lift2 $ _id ###> lookForRoleType (roletype2string object)
                  case mtype of
                    Nothing -> throwError $ RoleMissingInContext pos (roletype2string object)
                    (Just t) -> pure $ ar {object = t}
              -- TODO: indirectObject
              if ar' == ar
                then pure unit
                -- A change, so modify the DomeinFileRecord
                else modifyDF (\df@{actions: actions'} -> df {actions = insert (unwrap actId) (Action ar') actions'})
  where
    modifyDF :: (DomeinFileRecord -> DomeinFileRecord) -> PhaseThree Unit
    modifyDF f = void $ modify \s@{dfr} -> s {dfr = f dfr}
