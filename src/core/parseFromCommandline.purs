-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Perspectives.ParseFromCommandLine where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (runAff)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Foreign.Generic (encodeJSON)
import Node.Yargs.Applicative (runY, yarg)
import Node.Yargs.Setup (example, usage)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.RunPerspectives (runPerspectives)
import Perspectives.TypePersistence.LoadArc.FS (loadAndCompileArcFile)

parseFromCommandLine :: Effect Unit
parseFromCommandLine = do
  let setup = usage "$0 -parse path/to/file"
              <> example "$0 -parse myModel.arc" "Parses the file myModel.arc to a model. Serialises the model as a JSON value and sends it to standard output."

  runY setup $ app <$> yarg "parse" [] (Just "Path to an .arc file") (Right "An arc file name is required") true

app :: String -> Effect Unit
app relativePath = void $ runAff handleError ((runPerspectives "cor" "geheim" "cor" "http://127.0.0.1" 5984 "http://www.joopringelberg.nl/cdb/repository/" (parse relativePath)) >>= log)

parse :: String -> MonadPerspectives String
parse relativePath = do
  result <- loadAndCompileArcFile relativePath ""
  case result of
    Left errors -> pure $ show errors
    Right model -> pure $ encodeJSON model

handleError :: forall a. (Either Error a -> Effect Unit)
handleError (Left e) = log $ "An error condition: " <> (show e)
handleError (Right a) = log $ "Parsed a file."
