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
import Data.Foldable (find, foldl)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Foreign.Object (Object, empty, keys, lookup)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path as Path
import Node.Process (cwd)
import Perspectives.ContextRoleParser (userData)
import Perspectives.CoreTypes (MonadPerspectives, (##=))
import Perspectives.DomeinCache (removeDomeinFileFromCache, storeDomeinFileInCache, storeDomeinFileInCouchdb)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord, defaultDomeinFileRecord)
import Perspectives.IndentParser (runIndentParser')
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Instances.ObjectGetters (binding, context, getRole)
import Perspectives.Parsing.Arc (domain)
import Perspectives.Parsing.Arc.AST (ContextE)
import Perspectives.Parsing.Arc.IndentParser (position2ArcPosition, runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwoState, runPhaseTwo_')
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..))
import Prelude (bind, discard, pure, show, void, ($), (*>), (<>), (==), (>=>), (<$>))
import Text.Parsing.Parser (ParseError(..))

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
loadAndCompileArcFile :: String -> String -> MonadPerspectives (Either (Array PerspectivesError) DomeinFile)
loadAndCompileArcFile fileName directoryName = do
  procesDir <- liftEffect cwd
  catchError do
      text <- lift $ readTextFile UTF8 (Path.concat [procesDir, directoryName, fileName <> ".arc"])
      (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser text domain
      case r of
        (Left e) -> pure $ Left [parseError2PerspectivesError e]
        (Right ctxt) -> do
          (Tuple result state :: Tuple (Either PerspectivesError DomeinFile) PhaseTwoState) <- pure $ unwrap $ runPhaseTwo_' (traverseDomain ctxt "model:") defaultDomeinFileRecord empty empty
          case result of
            (Left e) -> pure $ Left [e]
            (Right (DomeinFile dr')) -> do
              -- Add referredModels to DomeinFile
              dr'' <- pure dr' {referredModels = state.referredModels}
              (x' :: (Either PerspectivesError DomeinFileRecord)) <- phaseThree dr''
              case x' of
                (Left e) -> pure $ Left [e]
                (Right correctedDFR) -> do
                  pure $ Right $ DomeinFile correctedDFR
    \e -> pure $ Left [Custom (show e)]

type Persister = String -> DomeinFile -> MonadPerspectives (Array PerspectivesError)

-- | Loads an .arc file and expects a .crl file with the same name. Adds the .crl
-- | file to the DomeinFile. Adds the model description instance to the DomeinFile.
-- | NOTICE that the model instances are added to cache!
loadArcAndCrl :: String -> String -> MonadPerspectives (Either (Array PerspectivesError) DomeinFile)
loadArcAndCrl fileName directoryName = do
  r <- loadAndCompileArcFile fileName directoryName
  case r of
    Left m -> pure $ Left m
    Right df@(DomeinFile drf@{_id}) -> do
      void $ storeDomeinFileInCache _id df
      x <- addModelDescriptionAndCrl drf
      removeDomeinFileFromCache _id
      case x of
        (Left e) -> pure $ Left e
        (Right withInstances) -> do
          pure $ Right (DomeinFile withInstances)
  where
    addModelDescriptionAndCrl :: DomeinFileRecord -> MonadPerspectives (Either (Array PerspectivesError) DomeinFileRecord)
    addModelDescriptionAndCrl df = do
      procesDir <- liftEffect cwd
      source <- lift $ readTextFile UTF8 (Path.concat [procesDir, directoryName, fileName <> ".crl"])
      (Tuple parseResult {roleInstances, prefixes}) <- runIndentParser' source userData
      case parseResult of
        Left e -> pure $ Left $ [Custom (show e)]
        Right _ -> do
          modelDescription <- pure $ find (\(PerspectRol{pspType}) -> pspType == EnumeratedRoleType "model:System$Model$External") roleInstances
          indexedNames <- case modelDescription of
            Nothing -> pure []
            Just m -> collectIndexedNames (identifier m)
          pure $ Right (df
            { modelDescription = modelDescription
            , crl = foldl (replacePrefix prefixes) source (keys prefixes)
            , indexedNames = indexedNames})

    -- Prefixes are stored in ParserState with a colon appended.
    replacePrefix :: Object String -> String -> String -> String
    replacePrefix prefixes crl prefix = case lookup prefix prefixes of
      Nothing -> crl
      Just r -> replaceAll (Pattern prefix) (Replacement (r <> "$")) crl

    collectIndexedNames :: RoleInstance -> MonadPerspectives (Array String)
    collectIndexedNames modelDescription = do
      iroles <- modelDescription ##= context >=> getRole (EnumeratedRoleType "model:System$Model$IndexedRole") >=> binding
      icontexts <- modelDescription ##= context >=> getRole (EnumeratedRoleType "model:System$Model$IndexedContext") >=> binding >=> context
      pure $ (unwrap <$> iroles) <> (unwrap <$> icontexts)

-- | Loads an .arc file and expects a .crl file with the same name. Adds the instances found in the .crl
-- | file to the DomeinFile. Adds the model description instance. Persists that DomeinFile.
loadAndPersistArcFile :: Boolean -> Persister -> String -> String -> MonadPerspectives (Array PerspectivesError)
loadAndPersistArcFile loadCRL persist fileName directoryName = do
  r <- if loadCRL
    then loadArcAndCrl fileName directoryName
    else loadAndCompileArcFile fileName directoryName
  case r of
    Left m -> pure m
    Right df@(DomeinFile drf@{_id}) -> persist _id df *> pure []

-- | Load an Arc file from a directory. Parse the file completely. Cache it.
-- | Loads an instance file, too. If not present, throws an error. Instances are added to the cache.
loadCompileAndCacheArcFile :: String -> String -> MonadPerspectives (Array PerspectivesError)
loadCompileAndCacheArcFile = loadAndPersistArcFile true \id df -> storeDomeinFileInCache id df *> pure []

-- | Load an Arc file from a directory. Parse the file completely. Cache it.
-- | Does not try to load an instance file.
loadCompileAndCacheArcFile' :: String -> String -> MonadPerspectives (Array PerspectivesError)
loadCompileAndCacheArcFile' = loadAndPersistArcFile false \id df -> storeDomeinFileInCache id df *> pure []

-- | Load an Arc file from a directory. Parse the file completely. Store in Couchdb.
-- | Loads an instance file, too. If not present, throws an error. Instances are added to the cache.
loadCompileAndSaveArcFile :: String -> String -> MonadPerspectives (Array PerspectivesError)
loadCompileAndSaveArcFile = loadAndPersistArcFile true \_ df -> storeDomeinFileInCouchdb df *> pure []

-- | Load an Arc file from a directory. Parse the file completely. Store in Couchdb.
-- | Does not try to load an instance file.
loadCompileAndSaveArcFile' :: String -> String -> MonadPerspectives (Array PerspectivesError)
loadCompileAndSaveArcFile' = loadAndPersistArcFile false \_ df -> storeDomeinFileInCouchdb df *> pure []

parseError2PerspectivesError :: ParseError -> PerspectivesError
parseError2PerspectivesError (ParseError message pos) = ParserError message (position2ArcPosition pos)
