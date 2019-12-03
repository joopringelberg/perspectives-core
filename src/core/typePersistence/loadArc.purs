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

module Perspectives.TypePersistence.LoadArc where

import Control.Monad.Error.Class (catchError)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path as Path
import Node.Process (cwd)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinCache (storeDomeinFileInCache, storeDomeinFileInCouchdb)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.Parsing.Arc (domain)
import Perspectives.Parsing.Arc.AST (ContextE)
import Perspectives.Parsing.Arc.IndentParser (position2ArcPosition, runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (evalPhaseTwo', traverseDomain)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Prelude (bind, pure, show, ($), (*>))
import Text.Parsing.Parser (ParseError(..))

-- | Load an Arc file from a directory relative to the active process. Parse the file completely.
-- | Does neither cache nor save the model.
loadArcFile :: String -> String -> MonadPerspectives (Either (Array PerspectivesError) DomeinFile)
loadArcFile fileName directoryName = do
  procesDir <- liftEffect cwd
  catchError do
      text <- lift $ readTextFile UTF8 (Path.concat [procesDir, directoryName, fileName])
      (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser text domain
      case r of
        (Left e) -> pure $ Left [parseError2PerspectivesError e]
        (Right ctxt) -> do
          case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
            (Left e) -> pure $ Left [e]
            (Right (DomeinFile dr')) -> do
              (x' :: (Either PerspectivesError DomeinFileRecord)) <- phaseThree dr'
              case x' of
                (Left e) -> pure $ Left [e]
                (Right correctedDFR) -> do
                  pure $ Right $ DomeinFile correctedDFR
    \e -> pure $ Left [Custom (show e)]

-- | Load an Arc file from a directory. Parse the file completely. Cache it.
loadAndCacheArcFile :: String -> String -> MonadPerspectives (Array PerspectivesError)
loadAndCacheArcFile fileName directoryName = do
  r <- loadArcFile fileName directoryName
  case r of
    Left m -> pure m
    Right df@(DomeinFile {_id}) -> storeDomeinFileInCache _id df *> pure []


-- | Load an Arc file from a directory. Parse the file completely. Store and cache it.
loadAndSaveArcFile :: String -> String -> MonadPerspectives (Array PerspectivesError)
loadAndSaveArcFile fileName directoryName = do
  r <- loadArcFile fileName directoryName
  case r of
    Left m -> pure m
    Right df -> (storeDomeinFileInCouchdb df) *> pure []

parseError2PerspectivesError :: ParseError -> PerspectivesError
parseError2PerspectivesError (ParseError message pos) = ParserError message (position2ArcPosition pos)
