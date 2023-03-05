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
import Data.Array (delete, filterA, findIndex, head, null)
import Data.Either (Either(..))
import Data.Foldable (foldl, for_)
import Data.List (List(..))
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, empty, keys, lookup, values)
import Perspectives.Checking.PerspectivesTypeChecker (checkDomeinFile)
import Perspectives.ContextRoleParser (userData)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, (###=), (##=))
import Perspectives.DomeinCache (removeDomeinFileFromCache, storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord, defaultDomeinFileRecord)
import Perspectives.IndentParser (runIndentParser')
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Instances.ObjectGetters (binding, context, getEnumeratedRoleInstances_)
import Perspectives.ModelDependencies (indexedContext, indexedRole, modelExternal)
import Perspectives.Parsing.Arc (domain)
import Perspectives.Parsing.Arc.AST (ContextE)
import Perspectives.Parsing.Arc.IndentParser (position2ArcPosition, runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwoState, runPhaseTwo_')
import Perspectives.Parsing.Messages (PerspectivesError(..), MultiplePerspectivesErrors)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..), EnumeratedRoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (loadModelIfMissing)
import Perspectives.Types.ObjectGetters (aspectsOfRole)
import Prelude (bind, discard, pure, show, void, ($), (<$>), (<>), (==), (>=>))
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

type Source = String

-- | Parses and compiles the ARC file to a DomeinFile. Does neither cache nor store the DomeinFile.
-- | However, will load, cache and store dependencies of the model.
loadAndCompileArcFile_ :: Source -> MonadPerspectivesTransaction (Either (Array PerspectivesError) DomeinFile)
loadAndCompileArcFile_ text = catchError
  do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} lift $ lift $ runIndentParser text domain
    case r of
      (Left e) -> pure $ Left [parseError2PerspectivesError e]
      (Right ctxt) -> do
        (Tuple result state :: Tuple (Either MultiplePerspectivesErrors DomeinFile) PhaseTwoState) <- lift $ lift $ runPhaseTwo_' (traverseDomain ctxt) defaultDomeinFileRecord empty empty Nil
        case result of
          (Left e) -> pure $ Left e
          (Right (DomeinFile dr'@{_id})) -> do
            dr''@{referredModels} <- pure dr' {referredModels = state.referredModels}
            -- We should load referred models if they are missing (but not the model we're compiling!).
            for_ (delete (DomeinFileId _id) state.referredModels) loadModelIfMissing
            (x' :: (Either MultiplePerspectivesErrors DomeinFileRecord)) <- lift $ phaseThree dr'' state.postponedStateQualifiedParts state.screens
            case x' of
              (Left e) -> pure $ Left e
              (Right correctedDFR@{referredModels:refModels}) -> do
                -- Run the type checker
                typeCheckErrors <- lift $ checkDomeinFile (DomeinFile correctedDFR)
                if null typeCheckErrors
                  then do
                    -- Remove the self-referral and add the source.
                    df <- pure $ DomeinFile correctedDFR
                      { referredModels = delete (DomeinFileId _id) refModels
                      , arc = text
                      }
                    -- void $ lift $ storeDomeinFileInCache _id df
                    pure $ Right df
                  else pure $ Left typeCheckErrors
  \e -> pure $ Left [Custom (show e)]

type Persister = String -> DomeinFile -> MonadPerspectives (Array PerspectivesError)

type ArcSource = String
type CrlSource = String

loadArcAndCrl' :: ArcSource -> CrlSource -> MonadPerspectivesTransaction (Either (Array PerspectivesError) DomeinFile)
loadArcAndCrl' arcSource crlSource = do
  r <- loadAndCompileArcFile_ arcSource
  case r of
    Left m -> pure $ Left m
    Right df@(DomeinFile drf@{_id}) -> do
      void $ lift $ storeDomeinFileInCache _id df
      x <- lift $ addModelDescriptionAndCrl drf
      lift $ removeDomeinFileFromCache _id
      case x of
        (Left e) -> pure $ Left e
        (Right withInstances) -> do
          pure $ Right (DomeinFile withInstances)
  where
    -- Adds the modelDescription, crl, indexedRoles and indexedContexts.
    addModelDescriptionAndCrl :: DomeinFileRecord -> MonadPerspectives (Either (Array PerspectivesError) DomeinFileRecord)
    addModelDescriptionAndCrl df = do
      (Tuple parseResult {roleInstances, prefixes}) <- runIndentParser' crlSource userData
      case parseResult of
        Left e -> pure $ Left $ [Custom (show e)]
        Right _ -> do
          (modelDescription :: Maybe PerspectRol) <- head <$> filterA
            (\(PerspectRol{pspType}) -> if pspType == (EnumeratedRoleType modelExternal)
                then pure true
                else do
                  aspects <- pspType ###= aspectsOfRole
                  pure $ isJust $ findIndex ((==) (EnumeratedRoleType modelExternal)) aspects)
            (values roleInstances)
          -- modelDescription <- pure $ find (\(PerspectRol{pspType}) -> pspType == EnumeratedRoleType modelExternal) roleInstances
          (Tuple indexedRoles indexedContexts) <- case modelDescription of
            Nothing -> pure $ Tuple [] []
            Just m -> do
              collectIndexedNames (identifier m)
          pure $ Right (df
            { modelDescription = modelDescription
            , crl = foldl (replacePrefix prefixes) crlSource (keys prefixes)
            , indexedRoles = indexedRoles
            , indexedContexts = indexedContexts})

    -- Prefixes are stored in ParserState with a colon appended.
    replacePrefix :: Object String -> String -> String -> String
    replacePrefix prefixes crl prefix = case lookup prefix prefixes of
      Nothing -> crl
      Just r -> replaceAll (Pattern prefix) (Replacement (r <> "$")) crl

    collectIndexedNames :: RoleInstance -> MonadPerspectives (Tuple (Array RoleInstance) (Array ContextInstance))
    collectIndexedNames modelDescription = do
      -- Notice that we MUST use the function that does no model reflection to get role instances from a context!
      -- This is because on compiling model:System, these roles are not yet defined.
      iroles <- modelDescription ##= context >=> getEnumeratedRoleInstances_ (EnumeratedRoleType indexedRole) >=> binding
      icontexts <- modelDescription ##= context >=> getEnumeratedRoleInstances_ (EnumeratedRoleType indexedContext) >=> binding >=> context
      pure $ Tuple iroles icontexts

parseError2PerspectivesError :: ParseError -> PerspectivesError
parseError2PerspectivesError (ParseError message pos) = ParserError message (position2ArcPosition pos)
