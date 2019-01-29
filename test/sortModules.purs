module SortModules where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExceptT)
import Data.Array (elemIndex, foldl, sortBy)
import Data.Either (Either(..))
import Data.Foreign (MultipleErrors, unsafeFromForeign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.StrMap (StrMap, fromFoldable, keys, lookup)
import Data.Tuple (Tuple(..))
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path as Path
import Node.Process (PROCESS, cwd)

newtype ModuleDependencies = ModuleDependencies (StrMap (Array String))

derive instance genericModuleDepencies :: Generic ModuleDependencies _

instance encodeModuleDependencies :: Encode ModuleDependencies where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

instance decodeModuleDependencies :: Decode ModuleDependencies where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

type FileName = String

type LoadModuleDependenciesEffects e = (console :: CONSOLE, fs :: FS, exception :: EXCEPTION, process :: PROCESS | e)

-- | Loads a file from the directory "src/model" relative to the directory of the
-- | active process.
loadModuleDependencies :: forall e. FileName -> Aff (LoadModuleDependenciesEffects e)  Unit
loadModuleDependencies file = do
  log ("=========================Parse the file " <> file <> "===================")
  procesDir <- liftEff cwd
  text <- liftEff $ readTextFile UTF8 (Path.concat [procesDir, "src", file])
  (mjson :: Either MultipleErrors ModuleDependencies) <- pure $ unwrap $ runExceptT (decodeJSON text)
  case mjson of
    (Left messages) -> log $ show messages
    (Right (ModuleDependencies m)) -> do
      (l :: Array String) <- pure $ sortBy (order m) (keys m)
      log $ show l
  where
    -- m1 dependsOn m2 iff m2 is a (recursive) dependency of m1. We then consider m1 to be larger (will be shifted to the right of m2).
    order :: StrMap (Array String) -> String -> String -> Ordering
    order deps m1 m2 = if m1 == m2 then EQ
      else if m1 `dependsOn` m2 then GT
        else if m2 `dependsOn` m1 then LT
          else EQ
      where
        dependsOn = dependsOn' deps

    -- m1 `dependsOn` m2 if
    --  * m2 is one of the dependencies of m1
    --  * one of the dependencies of m1 depends on m2
    dependsOn' :: StrMap (Array String) -> String -> String -> Boolean
    dependsOn' deps m1 m2 = case lookup m1 deps of
        Nothing -> false -- m1 has no dependencies, hence cannot depend on m2.
        (Just m1deps) -> case elemIndex m2 m1deps of
          -- if m1 dependsOn a dependency of m2, it is larger than m2.
          Nothing -> oneOfDependsOn m1deps m2
          otherwise -> true -- m2 is a dependency of m1, hence m1 dependsOn m2.
      where
        oneOfDependsOn :: Array String -> String -> Boolean
        oneOfDependsOn m1deps m2 = foldl (\cum m1dep -> if cum then cum else dependsOn' deps m1dep m2) false m1deps

testGenerate :: forall e. Aff (LoadModuleDependenciesEffects e) Unit
testGenerate = do
  deps <- pure $ ModuleDependencies $ fromFoldable [Tuple "aap" ["noot", "mies"]]
  json <- pure $ encode deps
  log $ unsafeFromForeign json
  deps' <- pure $ unwrap $ runExceptT (decode json)
  case deps' of
    (Left messages) -> log $ show messages
    (Right (json :: ModuleDependencies)) -> pure unit
