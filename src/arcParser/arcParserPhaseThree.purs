module Perspectives.Parsing.Arc.PhaseThree where

import Control.Monad.Error.Class (throwError)
import Data.Either (Either)
import Data.Foldable (for_)
import Data.Lens (Traversal', set, traversed)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Foreign.Object (lookup, values)
import Partial.Unsafe (unsafePartial)
import Perspectives.DomeinFile (DomeinFileRecord)
import Perspectives.Parsing.Arc.PhaseTwo (PhaseTwo, evalPhaseTwo_, getDF, modifyDF)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Representation.Action (Action(..))
import Perspectives.Representation.TypeIdentifiers (ActionType(..), CalculatedRoleType(..), EnumeratedRoleType(..), RoleType(..))
import Prelude (Unit, bind, discard, pure, unit, void, (<<<), ($))

phaseThree :: DomeinFileRecord -> (Either PerspectivesError DomeinFileRecord)
phaseThree df = evalPhaseTwo_ (correctAction df) df

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
