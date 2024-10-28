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

module Perspectives.TypePersistence.LoadArc where

import Control.Monad.Error.Class (catchError)
import Control.Monad.Trans.Class (lift)
import Data.Array (delete, null)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Foreign.Object (empty)
import Perspectives.Checking.PerspectivesTypeChecker (checkDomeinFile)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction)
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord, defaultDomeinFileRecord)
import Perspectives.InvertedQuery.Storable (StoredQueries)
import Perspectives.Parsing.Arc (domain)
import Perspectives.Parsing.Arc.AST (ContextE(..))
import Perspectives.Parsing.Arc.IndentParser (position2ArcPosition, runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwoState, runPhaseTwo_')
import Perspectives.Parsing.Messages (MultiplePerspectivesErrors, PerspectivesError(..))
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..))
import Perspectives.ResourceIdentifiers (takeGuid)
import Prelude (bind, discard, pure, show, ($), (<<<), (==))
import Parsing (ParseError(..))

-- | The functions in this module load Arc files and parse and compile them to DomeinFiles.
-- | Some functions expect a CRL file with the same name and add the instances found in them
-- | to the DomeinFile.
-- | Notice that these functions are more about creating DomeinFiles than about using them.
-- | A function to start using a particular model, by
-- |  * downloading the DomeinFile
-- |  * installing it in the local Couchdb installation
-- |  * and adding the Domain instances to the local Couchdb installation,
-- |  * can be found in the module Perspectives.Extern.Couchdb.

type Source = String

-- | Parses and compiles the ARC file to a DomeinFile. Does neither cache nor store the DomeinFile.
-- | However, will load, cache and store dependencies of the model.
loadAndCompileArcFile_ :: DomeinFileId -> Source -> MonadPerspectivesTransaction (Either (Array PerspectivesError) (Tuple DomeinFile StoredQueries))
loadAndCompileArcFile_ dfid@(DomeinFileId dfName) text = catchError
  do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} lift $ lift $ runIndentParser text domain
    case r of
      (Left e) -> pure $ Left [parseError2PerspectivesError e]
      (Right ctxt@(ContextE{id:sourceDfid, pos})) -> if sourceDfid == dfName
        then do
          (Tuple result state :: Tuple (Either MultiplePerspectivesErrors DomeinFile) PhaseTwoState) <- lift $ lift $ runPhaseTwo_' (traverseDomain ctxt) defaultDomeinFileRecord empty empty Nil
          case result of
            (Left e) -> pure $ Left e
            (Right (DomeinFile dr'@{id})) -> do
              dr''@{referredModels} <- pure dr' {referredModels = state.referredModels}
              -- We should load referred models if they are missing (but not the model we're compiling!).
              for_ (delete id state.referredModels) (lift <<< retrieveDomeinFile)
              (x' :: (Either MultiplePerspectivesErrors (Tuple DomeinFileRecord StoredQueries))) <- lift $ phaseThree dr'' state.postponedStateQualifiedParts state.screens
              case x' of
                (Left e) -> pure $ Left e
                (Right (Tuple correctedDFR@{referredModels:refModels} invertedQueries)) -> do
                  -- Run the type checker
                  typeCheckErrors <- lift $ checkDomeinFile (DomeinFile correctedDFR)
                  if null typeCheckErrors
                    then do
                      -- Remove the self-referral and add the source.
                      df <- pure $ DomeinFile correctedDFR
                        { referredModels = delete id refModels
                        , arc = text
                        , _id = takeGuid $ unwrap id
                        }
                      -- void $ lift $ storeDomeinFileInCache id df
                      pure $ Right $ Tuple df invertedQueries
                    else pure $ Left typeCheckErrors
        else pure $ Left [(DomeinFileIdIncompatible dfid (DomeinFileId sourceDfid) pos)]
  \e -> pure $ Left [Custom (show e)]

type Persister = String -> DomeinFile -> MonadPerspectives (Array PerspectivesError)

type ArcSource = String
type CrlSource = String

parseError2PerspectivesError :: ParseError -> PerspectivesError
parseError2PerspectivesError (ParseError message pos) = ParserError message (position2ArcPosition pos)
