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

module Perspectives.Query.Interpreter.Dependencies where

import Prelude

import Data.Array (union, cons) as ARR
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.List.NonEmpty (cons, singleton, snoc)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), maybe)
import Perspectives.Instances.Values (bool2Value, number2Value, value2Bool, value2Number)
import Perspectives.Query.QueryTypes (Domain(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..), RoleType(..), propertytype2string)

data Dependency = C ContextInstance | R RoleInstance | V String Value | CT ContextType | RT RoleType

derive instance genericDependency :: Generic Dependency _
instance eqDependency :: Eq Dependency where
  eq = genericEq

instance showDependency :: Show Dependency where
  show (C cid) = "C " <> show cid
  show (R rid) = "R " <> show rid
  show (V ptype val) = "V " <> show ptype <> " " <> show val
  show (CT ctype) = "CT " <> show ctype
  show (RT rtype) = "RT " <> show rtype

-- | Given information on a Domain, turn a string value into a Dependency.
-- | Caveats: Role types are construed to be Enumerated.
domain2Dependency :: Domain -> String -> Dependency
domain2Dependency (RDOM _) s = R $ RoleInstance s
domain2Dependency (CDOM _) s = C $ ContextInstance s
domain2Dependency (VDOM _ (Just pr)) s = V (propertytype2string pr) $ Value s
domain2Dependency (VDOM _ Nothing) s = V "" $ Value s
domain2Dependency ContextKind s = CT (ContextType s)
domain2Dependency RoleKind s = RT (ENR $ EnumeratedRoleType s)

-- | The head of a DependencyPath is the result of a query interpretation.
-- | The mainPath is the ordered sequence of dependencies whose leftmost element is the head (= the query result)
-- | and whose rightmost element is the source the query is applied to.
-- | supportingPaths are paths that are used in the computation but are not part of the mainPath, such
-- | as the paths to compute boolean values in a filter operation.
type DependencyPath =
  { head :: Dependency
  , mainPath :: Maybe (NonEmptyList Dependency)
  , supportingPaths :: Array (NonEmptyList Dependency)
  }

-- | As there is no empty DependencyPath, we need a singleton-like function.
singletonPath :: Dependency -> DependencyPath
singletonPath d = {head: d, mainPath: Just $ singleton d, supportingPaths: []}

-- | The new dependency becomes the head and is added to front of the mainPath (it becomes the first element).
consOnMainPath :: Dependency -> DependencyPath -> DependencyPath
consOnMainPath d p@{mainPath} = p {head = d, mainPath = Just $ maybe (singleton d) (cons d) mainPath}
-- consOnMainPath d p@{mainPath} = p {head = d, mainPath = cons d <$> mainPath}

-- | The new dependency is added to end of the mainPath (it becomes the last element).
-- | The head is untouched.
snocOnMainPath :: DependencyPath -> Dependency -> DependencyPath
snocOnMainPath p@{mainPath} d = p {mainPath = Just $ maybe (singleton d) (flip snoc d) mainPath}

addAsSupportingPaths :: Array (NonEmptyList Dependency) -> DependencyPath -> DependencyPath
addAsSupportingPaths paths dp@({supportingPaths}) = dp {supportingPaths = paths <> supportingPaths}

-- | All paths in the second argument are added as a supporting path to the first argument.
-- | In other words, the main path of the first argument is the main path of the result.
appendPaths :: DependencyPath -> DependencyPath -> DependencyPath
appendPaths dp1@{supportingPaths} dp2 = dp1 {supportingPaths = supportingPaths <> allPaths dp2}

-- | Compose two DependencyPaths in a way that reflects two consequtive steps in a query:
-- |  - the head is the head of the second DependencyPath.
-- |  - the mainPath is the concatenation of the second mainPath and the first (mp2.. mainPath);
-- |  - the supportingPaths are the concatenation of both supportingPaths.
-- | Compare composePaths to >>> rather than <<<. That is, the left path (first argument) is followed (left to right)
-- | by the right path (second argument). As the mainPath gives us dependencies from value to source, the
-- | second mainPath is put in front (to the left) of the first mainPath to construct the composition mainPath.
composePaths :: DependencyPath -> DependencyPath -> DependencyPath
composePaths {mainPath, supportingPaths} {head, mainPath:mp2, supportingPaths:sp2} =
  { head
  , mainPath: case mainPath of
      Nothing -> mp2
      otherwise -> map (<>) mp2 <*> mainPath
  , supportingPaths: supportingPaths <> sp2
}

infixr 9 composePaths as #>>

allPaths :: DependencyPath -> Array (NonEmptyList Dependency)
allPaths dp = case dp.mainPath of
  Nothing -> dp.supportingPaths
  Just d -> ARR.cons d dp.supportingPaths

-- | To apply a binary function like (+) converted with functionOnNumbers to two DependencyPaths.
-- | The function has to cater for all Depencency types. In practice that is just for Values;
-- | it returns Left Error otherwise.
-- | For example:
-- |    `applyValueFunction (functionOnNumbers (+))`
-- | Can be applied to two DependencyPaths.
applyValueFunction :: Partial => (Value -> Value -> Value) -> DependencyPath -> DependencyPath -> DependencyPath
applyValueFunction f dp1@{head: h1} dp2@{head: h2} =
  { head: (V "" (f (dependencyToValue h1) (dependencyToValue  h2)))
  , mainPath: Nothing
  , supportingPaths: allPaths dp1 `ARR.union` allPaths dp2
  }

-- TODO. Hoewel dit geen volledige functie is, kunnen we aannemen dat hij
-- alleen wordt toegepast als de Dependency een V is. De DescriptionCompiler zorgt
-- daarvoor.
dependencyToValue :: Partial => Dependency -> Value
dependencyToValue (V propName v) = v

-- | Wrap a binary function on Number values to become a binary function on Value values.
-- | Note that this is an UNSAFE function!
functionOnNumbers :: (Number -> Number -> Number) -> (Value -> Value -> Value)
functionOnNumbers f a b = number2Value (f (value2Number a) (value2Number b)) 

-- | Wrap a binary function on String values to become a binary function on Value values.
functionOnStrings :: (String -> String -> String) -> (Value -> Value -> Value)
functionOnStrings f (Value a) (Value b) = Value (f a b)

-- | Wrap a binary function on Boolean values to become a binary function on Value values.
functionOnBooleans :: (Boolean -> Boolean -> Boolean) -> (Value -> Value -> Value)
functionOnBooleans f a b = bool2Value (f (value2Bool a) (value2Bool b))
