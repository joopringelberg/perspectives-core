-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
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
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

module Perspectives.Parsing.Arc.PhaseTwoDefs where

import Control.Monad.Except (ExceptT, lift, runExceptT)
import Control.Monad.Except (throwError) as EXCEPT
import Control.Monad.State (class MonadState, StateT, evalStateT, gets, modify, runStateT)
import Data.Array (cons, elemIndex, singleton, tail, union)
import Data.Either (Either)
import Data.List (List(..), filter)
import Data.Map (Map, empty, lookup) as MAP
import Data.Maybe (Maybe, isJust, maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Foreign.Object (Object, empty, values)
import Foreign.Object (fromFoldable, union, lookup) as OBJ
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinCache (removeDomeinFileFromCache, storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile, DomeinFileRecord, defaultDomeinFileRecord)
import Perspectives.Instances.Environment (Environment, _pushFrame)
import Perspectives.Instances.Environment (addVariable, empty, lookup) as ENV
import Perspectives.InvertedQuery.Storable (StoredQueries, StorableInvertedQuery)
import Perspectives.Names (defaultNamespaces, expandNamespaces)
import Perspectives.Parsing.Arc.AST (ContextPart(..), ScreenE, StateQualifiedPart)
import Perspectives.Parsing.Arc.Expression.AST (Step)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Parsing.Messages (PerspectivesError, MultiplePerspectivesErrors)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)
import Perspectives.Representation.Perspective (Perspective)
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), ContextType, DomeinFileId(..), EnumeratedRoleType, RoleType)
import Prelude (class Eq, class Monad, Unit, bind, discard, eq, identity, map, pure, show, void, ($), (<$>), (<<<), (<>), (>>=))

-- TODO
-- (1) In a view, we need to indicate whether the property is calculated or enumerated.
-- However, we don't know when traversing the Arc AST.
-- (2) We need a way to indicate PRODUCT types for bindings.

type PhaseTwoState =
  { bot :: Boolean
  , dfr :: DomeinFileRecord
  , namespaces :: Object String
  , referredModels :: Array DomeinFileId
  , indexedContexts :: Object ContextType
  , indexedRoles :: Object EnumeratedRoleType
  -- In PhaseTwoState, variables are bound to QueryFunctionDescriptions.
  -- In PerspectivesState, variables are bound to Strings.
  , variableBindings :: Environment QueryFunctionDescription
  , postponedStateQualifiedParts :: List StateQualifiedPart
  , screens :: List ScreenE
  , perspectives :: MAP.Map (Tuple RoleType Step) Perspective
  , loopdetection :: LoopDetection
  , invertedQueries :: StoredQueries
}

data CurrentlyCalculated = Prop CalculatedPropertyType | Role CalculatedRoleType
instance Eq CurrentlyCalculated where
  eq (Prop p1) (Prop p2) = eq p1 p2
  eq (Role p1) (Role p2) = eq p1 p2
  eq _ _ = false


type LoopDetection = Array CurrentlyCalculated

-- | A Monad with state that indicates whether the Subject of an Action is a Bot,
-- | and allows exceptions.
-- | It allows for a variable bottom of the monadic stack.
type PhaseTwo' m = ExceptT MultiplePerspectivesErrors (StateT PhaseTwoState m)

throwError :: forall a m. Monad m => PerspectivesError -> PhaseTwo' m a
throwError = EXCEPT.throwError <<< singleton

-- | Run a computation in `PhaseTwo`, returning Errors or a Tuple holding both the state and the result of the computation.
runPhaseTwo' :: forall a m. PhaseTwo' m a -> m (Tuple (Either MultiplePerspectivesErrors a) PhaseTwoState)
runPhaseTwo' computation = runPhaseTwo_' computation defaultDomeinFileRecord empty empty Nil

runPhaseTwo_' :: forall a m.
  PhaseTwo' m a ->
  DomeinFileRecord ->
  Object ContextType ->
  Object EnumeratedRoleType ->
  List StateQualifiedPart ->
  m (Tuple (Either MultiplePerspectivesErrors a) PhaseTwoState)
runPhaseTwo_' computation dfr indexedContexts indexedRoles postponedParts = runStateT (runExceptT computation)
  { bot: false
  , dfr: dfr
  , namespaces: defaultNamespaces
  , referredModels: []
  , indexedContexts
  , indexedRoles
  , variableBindings: ENV.empty
  , postponedStateQualifiedParts: postponedParts
  , screens: Nil
  , perspectives: MAP.empty
  , loopdetection: []
  , invertedQueries: []}

-- | Run a computation in `PhaseTwo`, returning Errors or the result of the computation.
-- | Used in the test modules.
evalPhaseTwo' :: forall a m. Monad m => PhaseTwo' m a -> m (Either MultiplePerspectivesErrors a)
evalPhaseTwo' computation = evalPhaseTwo_' computation defaultDomeinFileRecord empty empty

evalPhaseTwo_' :: forall a m. Monad m => PhaseTwo' m a -> DomeinFileRecord -> Object ContextType -> Object EnumeratedRoleType -> m (Either MultiplePerspectivesErrors a)
evalPhaseTwo_' computation drf indexedContexts indexedRoles = evalStateT (runExceptT computation)
  { bot: false
  , dfr: drf
  , namespaces: defaultNamespaces
  , referredModels: []
  , indexedContexts
  , indexedRoles
  , variableBindings: ENV.empty
  , postponedStateQualifiedParts: Nil
  , screens: Nil
  , perspectives: MAP.empty
  , loopdetection: []
  , invertedQueries: []
  }

-- type PhaseTwo a = PhaseTwo' a Identity
type PhaseTwo = PhaseTwo' Aff

-- | A Monad based on MonadPerspectives that allows PerspectivesErrors.
type PhaseThree = PhaseTwo' MonadPerspectives

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

-- | Get a part of the DomeinFileRecord from PhaseTwoState.
getsDF :: forall m n. MonadState PhaseTwoState m => (DomeinFileRecord -> n) -> m n
getsDF f = gets (f <<< _.dfr)

getLoopdetection :: PhaseThree LoopDetection
getLoopdetection = lift $ gets _.loopdetection

isBeingCalculated :: CurrentlyCalculated -> PhaseThree Boolean
isBeingCalculated c@(Prop (CalculatedPropertyType _)) = getLoopdetection >>= pure <<< isJust <<< elemIndex c 
isBeingCalculated c@(Role (CalculatedRoleType _)) = getLoopdetection >>= pure <<< isJust <<< elemIndex c 

loopErrorMessage :: CurrentlyCalculated -> ArcPosition -> ArcPosition -> String
loopErrorMessage def start end = 
  case def of
    (Prop (CalculatedPropertyType p)) -> "(RecursiveDefinition) Property " <> p <> " is defined in terms of itself (between " <> show start <> " and " <> show end <> ")."
    (Role (CalculatedRoleType r)) -> "(RecursiveDefinition) Role " <> r <> " is defined in terms of itself (between " <> show start <> " and " <> show end <> ")."

withCurrentCalculation :: forall a. CurrentlyCalculated -> PhaseThree a -> PhaseThree a
withCurrentCalculation cc computation = do
  void $ modify \s@{loopdetection} -> s {loopdetection = cons cc loopdetection}
  r <- computation
  void $ modify \s@{loopdetection} -> s {loopdetection = maybe [] identity (tail loopdetection)}
  pure r

getVariableBindings :: forall m. Monad m => PhaseTwo' m (Environment QueryFunctionDescription)
getVariableBindings = lift $ gets _.variableBindings

addBinding :: forall m. Monad m => String -> QueryFunctionDescription -> PhaseTwo' m Unit
addBinding varName qfd = void $ modify \s@{variableBindings} -> s {variableBindings = ENV.addVariable varName qfd variableBindings}

lookupVariableBinding :: forall m. Monad m => String -> PhaseTwo' m (Maybe QueryFunctionDescription)
lookupVariableBinding varName = getVariableBindings >>= pure <<< (ENV.lookup varName)

-- | Introduce a new scope.
withFrame :: forall a m. Monad m => PhaseTwo' m a -> PhaseTwo' m a
withFrame computation = do
  old <- getVariableBindings
  void $ modify \s@{variableBindings} -> s {variableBindings = (_pushFrame old)}
  r <- computation
  void $ modify \s@{variableBindings} -> s {variableBindings = old}
  pure r

-- | withNamespaces only handles the `PREFIX` element of the `ContextPart` Sum.
-- | Computes a value in PhaseTwo with the extra namespaces.
-- | Cumulates referredModels in PhaseTwoState, too.
withNamespaces :: forall a. List ContextPart -> PhaseTwo a -> PhaseTwo a
withNamespaces pairs pt = do
  -- x is an Object of all models declared with a 'use' statement. All models are qualified, by construction.
  x <- pure $ OBJ.fromFoldable $ map
    (unsafePartial \(PREFIX pre mod) -> Tuple pre mod)
    (filter (case _ of
        (PREFIX _ _) -> true
        otherwise -> false)
      pairs)
  -- x <- pure $ OBJ.fromFoldable $ map (\(PREFIX pre mod) -> Tuple pre mod) pairs
  ns <- lift $ gets _.namespaces
  -- replace keys in ns with values found in x.
  void $ modify \(s@{namespaces, referredModels}) -> s
    { namespaces = x `OBJ.union` namespaces
    , referredModels = referredModels `union` (DomeinFileId <$> values x)}
  ctxt <- pt
  void $ modify \s -> s {namespaces = ns}
  pure ctxt

-- | Expand prefixes to full model names.
expandNamespace :: forall m. Monad m => String -> (PhaseTwo' m) String
expandNamespace s = do
  namespaces <- lift $ gets _.namespaces
  pure $ expandNamespaces namespaces s

-- | From an expanded name like "model:System$MySystem" returns a Maybe Context.
-- | Note: returns indexed context names from PhaseTwoState.
isIndexedContext :: forall m. Monad m => String -> (PhaseTwo' m) (Maybe ContextType)
isIndexedContext n = do
  indexedContexts <- lift $ gets _.indexedContexts
  pure $ OBJ.lookup n indexedContexts

isIndexedRole :: forall m. Monad m => String -> (PhaseTwo' m) (Maybe EnumeratedRoleType)
isIndexedRole n = do
  indexedRoles <- lift $ gets _.indexedRoles
  pure $ OBJ.lookup n indexedRoles

-- | Find a perspective in PhaseTwoState.
findPerspective :: forall m. Monad m => RoleType -> Step -> PhaseTwo' m (Maybe Perspective )
findPerspective subject object = do
  perspectives <- lift $ gets _.perspectives
  pure $ MAP.lookup (Tuple subject object) perspectives

-- insertPerspective :: forall m. Monad m => Perspective -> PhaseTwo' Unit m
-- insertPerspective perspective@(Perspective{subject, object}) = void $ lift $ modify \r@{perspectives} -> r {perspectives = MAP.insert (Tuple (roletype2string subject) object) perspective perspectives}

withDomeinFile :: forall a. DomeinFileId -> DomeinFile -> PhaseThree a -> PhaseThree a
withDomeinFile ns df mpa = do
  void $ lift2 $ storeDomeinFileInCache ns df
  r <- mpa
  lift2 $ removeDomeinFileFromCache ns
  pure r

-- | Add a StorableInvertedQuery to PhaseTwo State.
addStorableInvertedQuery :: StorableInvertedQuery -> PhaseThree Unit
addStorableInvertedQuery siq = void $ modify \s@{invertedQueries} -> s {invertedQueries = cons siq invertedQueries}