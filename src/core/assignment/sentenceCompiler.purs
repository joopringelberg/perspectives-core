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

import Data.Array (concat)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Show (show)
import Data.String (Pattern(..), Replacement(..), replace)
import Data.Traversable (traverse)
import Perspectives.CoreTypes (MonadPerspectives, evalMonadPerspectivesQuery, type (~~>))
import Perspectives.ModelTranslation (translationOf)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)
import Perspectives.Query.UnsafeCompiler (context2string, role2string)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.Sentence (Sentence(..))
import Prelude (bind, flip, pure, (<$>), (<<<), (<>), (>=>))

type CompiledSentence a = a -> MonadPerspectives String

-- | Compile a Sentence to a function that creates a string out of a RoleInstance.
compileSentence :: forall a.
  String ->
  (QueryFunctionDescription -> MonadPerspectives (a ~~> String)) ->
  Sentence ->
  MonadPerspectives (CompiledSentence a)
compileSentence domain xToString (Sentence {sentence, parts}) = do
  compiledParts <- traverse (xToString >=> pure <<< flip evalMonadPerspectivesQuery)
    parts
  pure \roleId -> do
    (replacements :: Array String) <- concat <$> traverse (\p -> p roleId) compiledParts
    -- TODO: replace sentence with its translation before applying the expression value replacements to it.
    translatedSentence <- translationOf domain sentence
    pure (foldlWithIndex (\i s next -> replace (Pattern ("$" <> show i)) (Replacement next) s) sentence replacements)

-- | From a Sentence, create a function that takes a ContextInstance and produces a String in MonadPerspectives.
compileContextSentence :: String -> Sentence -> MonadPerspectives (CompiledSentence ContextInstance)
compileContextSentence domain = compileSentence domain context2string

-- | From a Sentence, create a function that takes a RoleInstance and produces a String in MonadPerspectives.
compileRoleSentence :: String -> Sentence -> MonadPerspectives (CompiledSentence RoleInstance)
compileRoleSentence domain = compileSentence domain role2string
