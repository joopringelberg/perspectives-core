module Test.SortModules where

import Prelude

import Control.Monad.Aff (Fiber, runAff)
import Control.Monad.Eff (Eff)
import SortModules (LoadModuleDependenciesEffects, loadModuleDependencies)
import Test.TestEffects as TE

test :: forall e. Eff (LoadModuleDependenciesEffects e) (Fiber (LoadModuleDependenciesEffects e) Unit)
test = runAff TE.handleError (loadModuleDependencies "ModuleDependencies.json")
