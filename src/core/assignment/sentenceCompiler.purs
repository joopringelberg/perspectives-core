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

module Perspectives.Assignment.SentenceCompiler where

import Control.Monad.Error.Class (throwError)
import Data.Array (concat)
import Data.Traversable (intercalate, traverse)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives, evalMonadPerspectivesQuery, type (~~>))
import Perspectives.Query.QueryTypes (Calculation(..), QueryFunctionDescription)
import Perspectives.Query.UnsafeCompiler (context2string, role2string)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.Sentence (Sentence(..), SentencePart(..))
import Prelude (bind, flip, pure, show, ($), (<$>), (<>), (>>=), (<<<))

type CompiledSentence a = a -> MonadPerspectives String

-- | Compile a Sentence to a function that creates a string out of a RoleInstance.
compileSentence :: forall a.
  (QueryFunctionDescription -> MonadPerspectives (a ~~> String)) ->
  Sentence ->
  MonadPerspectives (CompiledSentence a)
compileSentence xToString (Sentence parts) = do
  compiledParts <- traverse
    case _ of
      HR s -> pure \_ -> pure [s]
      CP c -> case c of
        S step -> throwError (error $ "Sentence parts must be compiled, but found " <> show step)
        Q calc -> xToString calc >>= pure <<< flip evalMonadPerspectivesQuery
    parts
  pure \roleId ->
    intercalate " " <<< concat <$> traverse (\p -> p roleId) compiledParts

-- | From a Sentence, create a function that takes a ContextInstance and produces a String in MonadPerspectives.
compileContextSentence :: Sentence -> MonadPerspectives (CompiledSentence ContextInstance)
compileContextSentence = compileSentence context2string

-- | From a Sentence, create a function that takes a RoleInstance and produces a String in MonadPerspectives.
compileRoleSentence :: Sentence -> MonadPerspectives (CompiledSentence RoleInstance)
compileRoleSentence = compileSentence role2string
