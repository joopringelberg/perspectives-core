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

module Perspectives.TypePersistence.LoadArc.FS where

import Control.Monad.Error.Class (catchError)
import Control.Monad.Trans.Class (lift)
import Data.Array (delete)
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Foreign.Object (empty)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path as Path
import Node.Process (cwd)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinCache (storeDomeinFileInCache, storeDomeinFileInCouchdb)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord, defaultDomeinFileRecord)
import Perspectives.InvertedQuery.Storable (StoredQueries)
import Perspectives.Parsing.Arc (domain)
import Perspectives.Parsing.Arc.AST (ContextE)
import Perspectives.Parsing.Arc.IndentParser (position2ArcPosition, runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwoState, runPhaseTwo_')
import Perspectives.Parsing.Messages (PerspectivesError(..), MultiplePerspectivesErrors)
import Perspectives.Representation.TypeIdentifiers (DomeinFileId)
import Prelude (bind, pure, show, ($), (*>), (<>))
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

-- | Load an Arc file from a directory relative to the active process. Parse the file completely.
-- | Does neither cache nor save the model.
loadAndCompileArcFile :: String -> String -> MonadPerspectives (Either MultiplePerspectivesErrors (Tuple DomeinFile StoredQueries))
loadAndCompileArcFile fileName directoryName = do
  procesDir <- liftEffect cwd
  loadAndCompileArcFile_ (Path.concat [procesDir, directoryName, fileName <> ".arc"])

type FilePath = String

loadAndCompileArcFile_ :: FilePath -> MonadPerspectives (Either MultiplePerspectivesErrors (Tuple DomeinFile StoredQueries))
loadAndCompileArcFile_ filePath = catchError
  do
    text <- lift $ readTextFile UTF8 filePath
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} lift $ runIndentParser text domain
    case r of
      (Left e) -> pure $ Left [parseError2PerspectivesError e]
      (Right ctxt) -> do
        -- liftEffect $ log ((show ctxt) <> "\n\n\n")
        (Tuple result state :: Tuple (Either MultiplePerspectivesErrors DomeinFile) PhaseTwoState) <- {-pure $ unwrap $-} lift $ runPhaseTwo_' (traverseDomain ctxt) defaultDomeinFileRecord empty empty Nil
        case result of
          (Left e) -> pure $ Left e
          (Right (DomeinFile dr'@{id})) -> do
            -- log (show dr')
            dr'' <- pure dr' {referredModels = state.referredModels}
            -- logShow state.referredModels
            (x' :: (Either MultiplePerspectivesErrors (Tuple DomeinFileRecord StoredQueries))) <- phaseThree dr'' state.postponedStateQualifiedParts state.screens
            case x' of
              (Left e) -> do 
                pure $ Left e
              (Right (Tuple correctedDFR@{referredModels:refModels} invertedQueries)) -> do
                -- Remove the self-referral and add the source.
                df <- pure $ DomeinFile correctedDFR
                  { referredModels = delete id refModels
                  , arc = text
                  }
                pure $ Right $ Tuple df invertedQueries
  \e -> pure $ Left [Custom (show e)]

type Persister = DomeinFileId -> DomeinFile -> MonadPerspectives MultiplePerspectivesErrors

type ArcPath = String
type CrlPath = String

-- | Loads an .arc file and expects a .crl file with the same name. Adds the instances found in the .crl
-- | file to the DomeinFile. Adds the model description instance. Persists that DomeinFile.
-- | NOTICE THAT INVERTED QUERIES ARE NOT STORED!
loadAndPersistArcFile :: Boolean -> Persister -> String -> String -> MonadPerspectives MultiplePerspectivesErrors
loadAndPersistArcFile loadCRL persist fileName directoryName = do
  r <- loadAndCompileArcFile fileName directoryName
  case r of
    Left m -> pure m
    Right (Tuple df@(DomeinFile {id}) invertedQueries ) -> persist id df *> pure []

-- | Load an Arc file from a directory. Parse the file completely. Cache it.
-- | Loads an instance file, too. If not present, throws an error. Instances are added to the cache.
loadCompileAndCacheArcFile :: String -> String -> MonadPerspectives MultiplePerspectivesErrors
loadCompileAndCacheArcFile = loadAndPersistArcFile true \id df -> storeDomeinFileInCache id df *> pure []

-- | Load an Arc file from a directory. Parse the file completely. Cache it.
-- | Does not try to load an instance file.
loadCompileAndCacheArcFile' :: String -> String -> MonadPerspectives MultiplePerspectivesErrors
loadCompileAndCacheArcFile' = loadAndPersistArcFile false \id df -> storeDomeinFileInCache id df *> pure []

-- | Load an Arc file from a directory. Parse the file completely. Store in Couchdb.
-- | Loads an instance file, too. If not present, throws an error. Instances are added to the cache.
loadCompileAndSaveArcFile :: String -> String -> MonadPerspectives MultiplePerspectivesErrors
loadCompileAndSaveArcFile = loadAndPersistArcFile true \_ df -> storeDomeinFileInCouchdb df *> pure []

-- | Load an Arc file from a directory. Parse the file completely. Store in Couchdb.
-- | Does not try to load an instance file.
loadCompileAndSaveArcFile' :: String -> String -> MonadPerspectives MultiplePerspectivesErrors
loadCompileAndSaveArcFile' = loadAndPersistArcFile false \_ df -> storeDomeinFileInCouchdb df *> pure []

parseError2PerspectivesError :: ParseError -> PerspectivesError
parseError2PerspectivesError (ParseError message pos) = ParserError message (position2ArcPosition pos)
