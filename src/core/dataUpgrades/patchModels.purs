module Perspectives.DataUpgrade.PatchModels where

import Prelude

import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (for, for_)
import Foreign.Object (Object, lookup)
import Main.RecompileBasicModels (UninterpretedDomeinFile(..))
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (addDocument_, documentsInDatabase, includeDocs)
import Perspectives.PerspectivesState (modelsDatabaseName)
import Perspectives.ResourceIdentifiers (resourceIdentifier2WriteDocLocator)
import Simple.JSON (read) as JSON

-- | For each of the named files, retrieve the text source and replace the `arc` property of the 
-- | local domain files.
-- | Typically apply this function before recompiling a local model.
patchModels :: Object String -> MonadPerspectives Unit
patchModels replacements = do 
    modelsDb <- modelsDatabaseName
    {rows:allModels} <- documentsInDatabase modelsDb includeDocs
    -- As doc is still uninterpreted, we can only rely on the rows.id member of the PouchdbAllDocs record. These, however, are DomeinFileIdentifiers.
    -- We do not have a useful test on the form of such identifiers.
    uninterpretedDomeinFiles <- for allModels \({id, doc}) -> case JSON.read <$> doc of
      Just (Left errs) -> (logPerspectivesError (Custom ("Cannot interpret model document as UninterpretedDomeinFile: '" <> id <> "' " <> show errs))) *> pure Nothing
      Nothing -> logPerspectivesError (Custom ("No document retrieved for model '" <> id <> "'.")) *> pure Nothing
      Just (Right (df :: UninterpretedDomeinFile)) -> pure $ Just df
    for_ (catMaybes uninterpretedDomeinFiles) \(UninterpretedDomeinFile dfr@{namespace}) -> case lookup namespace replacements of 
      Nothing -> pure unit
      -- Store the model in Couchdb, that is: in the local store of models.
      Just replacement -> do
        {database, documentName} <- resourceIdentifier2WriteDocLocator namespace
        void $ addDocument_ database (dfr {arc = replacement}) documentName

