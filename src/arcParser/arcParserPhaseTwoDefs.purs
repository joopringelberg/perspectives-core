-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

module Perspectives.Parsing.Arc.PhaseTwoDefs where

import Control.Monad.Except (ExceptT, lift, runExceptT)
import Control.Monad.State (class MonadState, StateT, evalStateT, gets, modify, runStateT)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, empty)
import Foreign.Object (fromFoldable, union) as OBJ
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinFile (DomeinFileRecord, defaultDomeinFileRecord)
import Perspectives.Instances.Environment (Environment, _pushFrame)
import Perspectives.Instances.Environment (addVariable, empty, lookup) as ENV
import Perspectives.Names (defaultNamespaces, expandDefaultNamespaces_, expandNamespaces)
import Perspectives.Parsing.Arc.AST (ContextPart(..))
import Perspectives.Parsing.Messages (PerspectivesError)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)
import Prelude (class Monad, Unit, bind, discard, map, pure, void, ($), (<<<), (>>=))

-- TODO
-- (1) In a view, we need to indicate whether the property is calculated or enumerated.
-- However, we don't know when traversing the Arc AST.
-- (2) We need a way to indicate PRODUCT types for bindings.

type PhaseTwoState =
  { bot :: Boolean
  , dfr :: DomeinFileRecord
  , namespaces :: Object String
  , indexedNames :: Object String
  -- In PhaseTwoState, variables are bound to QueryFunctionDescriptions.
  -- In PerspectivesState, variables are bound to Strings.
  , variableBindings :: Environment QueryFunctionDescription
}

-- | A Monad with state that indicates whether the Subject of an Action is a Bot,
-- | and allows exceptions.
-- | It allows for a variable bottom of the monadic stack.
type PhaseTwo' a m = ExceptT PerspectivesError (StateT PhaseTwoState m) a

-- | Run a computation in `PhaseTwo`, returning Errors or a Tuple holding both the state and the result of the computation.
runPhaseTwo' :: forall a m. PhaseTwo' a m -> m (Tuple (Either PerspectivesError a) PhaseTwoState)
runPhaseTwo' computation = runPhaseTwo_' computation defaultDomeinFileRecord empty

runPhaseTwo_' :: forall a m. PhaseTwo' a m -> DomeinFileRecord -> Object String ->  m (Tuple (Either PerspectivesError a) PhaseTwoState)
runPhaseTwo_' computation dfr indexedNames = runStateT (runExceptT computation) {bot: false, dfr: dfr, namespaces: defaultNamespaces, indexedNames, variableBindings: ENV.empty}

-- | Run a computation in `PhaseTwo`, returning Errors or the result of the computation.
evalPhaseTwo' :: forall a m. Monad m => PhaseTwo' a m -> m (Either PerspectivesError a)
evalPhaseTwo' computation = evalPhaseTwo_' computation defaultDomeinFileRecord empty

evalPhaseTwo_' :: forall a m. Monad m => PhaseTwo' a m -> DomeinFileRecord -> Object String -> m (Either PerspectivesError a)
evalPhaseTwo_' computation drf indexedNames = evalStateT (runExceptT computation) {bot: false, dfr: drf, namespaces: defaultNamespaces, indexedNames, variableBindings: ENV.empty}

type PhaseTwo a = PhaseTwo' a Identity

-- | A Monad based on MonadPerspectives, with state that indicates whether the Subject of
-- | an Action is a Bot, and allows exceptions.
type PhaseThree a = PhaseTwo' a MonadPerspectives

lift2 :: forall a. MonadPerspectives a -> PhaseThree a
lift2 = lift <<< lift

subjectIsBot :: PhaseTwo Unit
subjectIsBot = lift $ void $ modify (\s -> s {bot = true})

subjectIsNotABot :: PhaseTwo Unit
subjectIsNotABot = lift $ void $ modify (\s -> s {bot = false})

isSubjectBot :: PhaseTwo Boolean
isSubjectBot = lift $ gets _.bot

modifyDF :: forall m. MonadState PhaseTwoState m => (DomeinFileRecord -> DomeinFileRecord) -> m Unit
modifyDF f = void $ modify \s@{dfr} -> s {dfr = f dfr}

getDF :: PhaseTwo DomeinFileRecord
getDF = lift $ gets _.dfr

getVariableBindings :: forall m. Monad m => PhaseTwo' (Environment QueryFunctionDescription) m
getVariableBindings = lift $ gets _.variableBindings

addBinding :: forall m. Monad m => String -> QueryFunctionDescription -> PhaseTwo' Unit m
addBinding varName qfd = void $ modify \s@{variableBindings} -> s {variableBindings = ENV.addVariable varName qfd variableBindings}

lookupVariableBinding :: forall m. Monad m => String -> PhaseTwo' (Maybe QueryFunctionDescription) m
lookupVariableBinding varName = getVariableBindings >>= pure <<< (ENV.lookup varName)

withFrame :: forall a m. Monad m => PhaseTwo' a m -> PhaseTwo' a m
withFrame computation = do
  old <- getVariableBindings
  void $ modify \s@{variableBindings} -> s {variableBindings = (_pushFrame old)}
  r <- computation
  void $ modify \s@{variableBindings} -> s {variableBindings = old}
  pure r

-- | withNamespaces only handles the `PREFIX` element of the `ContextPart` Sum.
withNamespaces :: forall a. Partial => List ContextPart -> PhaseTwo a -> PhaseTwo a
withNamespaces pairs pt = do
  x <- pure $ OBJ.fromFoldable $ map (\(PREFIX pre mod) -> Tuple pre mod) pairs
  ns <- lift $ gets _.namespaces
  -- replace keys in ns with values found in x.
  void $ modify \s -> s {namespaces = x `OBJ.union` ns}
  ctxt <- pt
  void $ modify \s -> s {namespaces = ns}
  pure ctxt

expandNamespace :: String -> PhaseTwo String
expandNamespace s = do
  namespaces <- lift $ gets _.namespaces
  pure $ expandNamespaces namespaces s

expandDefaultNamespaces :: String -> PhaseTwo String
expandDefaultNamespaces s = do
  namespaces <- lift $ gets _.namespaces
  indexedNames <- lift $ gets _.indexedNames
  pure $ expandDefaultNamespaces_ indexedNames namespaces s
