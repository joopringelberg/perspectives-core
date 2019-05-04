module Test.Perspectives.Utils where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (findIndex, singleton)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, error, throwError)
import Effect.Class.Console (log)
import Foreign (MultipleErrors)
import Foreign.Generic (decodeJSON)
import Perspectives.ApiTypes (ContextSerialization)
import Perspectives.BasicConstructors (constructContext)
import Perspectives.CoreTypes (MonadPerspectives, UserMessage(..))
import Perspectives.Identifiers (buitenRol, expandDefaultNamespaces)
import Perspectives.LoadCRL (loadCRLFile, unLoadCRLFile, withSemanticChecks, withoutSemanticChecks)
import Perspectives.PerspectivesTypes (BuitenRol(..), AnyContext)
import Perspectives.RunPerspectives (runPerspectives, runPerspectivesWithPropagation)
import Perspectives.SaveUserData (removeUserData, saveUserData)
import Test.Unit.Assert as Assert


runP :: forall a.
  MonadPerspectives a ->
  Aff a
runP t = runPerspectives "cor" "geheim" t

runPWithPropagation :: forall a.
  MonadPerspectives a ->
  Number ->
  Aff a
runPWithPropagation t n = do
  (Tuple s a) <- runPerspectivesWithPropagation "cor" "geheim" t n
  log $ show $ _.recomputed s
  pure a

p :: String -> String
p s = "model:Perspectives$" <> s

q :: String -> String
q s = "model:QueryAst$" <> s

u :: String -> String
u s = "model:User$" <> s

shouldEqual :: forall a. Eq a => a -> a -> Aff Boolean
shouldEqual a = \b -> pure (a == b)

type Message = String

assertEqual :: forall a. Eq a => Show a =>
  Message ->
  MonadPerspectives a ->
  a ->
  Aff Unit
assertEqual message test result = do
  r <- runP test
  case result == r of
    true -> Assert.assert message true
    false -> Assert.assert (message <> "\nExpected: " <>
      show result <> "\nReceived: " <>
      show r)
      false

assertEqualWithPropagation :: forall a. Eq a => Show a =>
  Message ->
  MonadPerspectives a ->
  a ->
  Number ->
  Aff Unit
assertEqualWithPropagation message test result duration = do
  r <- runPWithPropagation test duration
  case result == r of
    true -> Assert.assert message true
    false -> Assert.assert (message <> "\nExpected: " <>
      show result <> "\nReceived: " <>
      show r)
      false

addTestContext :: String -> Aff Unit
addTestContext = void <<< runP <<< addTestContext'
  where
    addTestContext' :: String -> MonadPerspectives Unit
    addTestContext' s = do
      (r :: Either MultipleErrors ContextSerialization) <- pure $ runExcept $ decodeJSON s
      case r of
        (Left m) -> throwError $ error $ show [NotWellFormedContextSerialization $ show m]
        (Right cs) -> do
          r' <- constructContext cs
          case r' of
            (Left messages) -> throwError (error (show messages))
            (Right id) -> do
              _ <- saveUserData [BuitenRol $ buitenRol id]
              pure unit

removeTestContext :: AnyContext -> Aff Unit
removeTestContext cid = void $ runP $ removeTestContext' (buitenRol (expandDefaultNamespaces cid))
  where

  removeTestContext' :: AnyContext -> MonadPerspectives Unit
  removeTestContext' = void <<< removeUserData <<< singleton <<< BuitenRol

loadTestModel :: String -> Aff Unit
loadTestModel ns = void $ runP $ loadCRLFile withoutSemanticChecks ns

unLoadTestModel :: String -> Aff Unit
unLoadTestModel ns = void $ runP $ unLoadCRLFile ns

-- | Check the definitions given in the modelfile. Provide an Array
-- | of error types (DataConstructors of UserMessage).
-- | An assertion false follows for any error type in the list that
-- | is not found by the TypeDefChecker.
typeDefCheckerNotifies :: String -> Array String -> Aff Unit
typeDefCheckerNotifies fileName messages = do
  notifications <- runP $ loadCRLFile withSemanticChecks fileName
  notificationStrings <- pure $ (map show) notifications
  for_ messages
    \message -> case findIndex (test (unsafeRegex message noFlags)) notificationStrings of
      Nothing -> Assert.assert ("TypeDefChecker failed to find " <> message) false
      (Just i) -> pure unit
