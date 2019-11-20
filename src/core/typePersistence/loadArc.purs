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
import Perspectives.DomeinCache (storeDomeinFileInCouchdb)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.Parsing.Arc (domain)
import Perspectives.Parsing.Arc.AST (ContextE)
import Perspectives.Parsing.Arc.IndentParser (position2ArcPosition, runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (evalPhaseTwo', traverseDomain)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Prelude (bind, pure, show, ($), discard)
import Text.Parsing.Parser (ParseError(..))

-- | Load an Arc file from a directory. Parse the file completely. Store and cache it.
loadArcFile :: String -> String -> MonadPerspectives (Array PerspectivesError)
loadArcFile fileName directoryName = do
  procesDir <- liftEffect cwd
  catchError do
      text <- lift $ readTextFile UTF8 (Path.concat [procesDir, directoryName, fileName])
      (r :: Either ParseError ContextE) <- pure $ unwrap $ runIndentParser text domain
      case r of
        (Left e) -> pure [parseError2PerspectivesError e]
        (Right ctxt) -> do
          case unwrap $ evalPhaseTwo' (traverseDomain ctxt "model:") of
            (Left e) -> pure [e]
            (Right (DomeinFile dr')) -> do
              (x' :: (Either PerspectivesError DomeinFileRecord)) <- phaseThree dr'
              case x' of
                (Left e) -> pure [e]
                (Right correctedDFR) -> do
                  storeDomeinFileInCouchdb (DomeinFile correctedDFR)
                  pure []
    \e -> pure [Custom (show e)]

parseError2PerspectivesError :: ParseError -> PerspectivesError
parseError2PerspectivesError (ParseError message pos) = ParserError message (position2ArcPosition pos)
