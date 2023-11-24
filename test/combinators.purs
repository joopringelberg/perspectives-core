module Test.Combinators where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (null)
import Data.Foldable (for_)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Perspectives.CoreTypes ((##=), removeInternally)
import Perspectives.Instances.ObjectGetters (binding, getEnumeratedRoleInstances)
import Perspectives.Query.UnsafeCompiler (getPropertyFunction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance, Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..))
import Perspectives.TypePersistence.LoadArc.FS (loadCompileAndCacheArcFile)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

theSuite :: Free TestF Unit
theSuite = suite "Combinators" do

  test "available" (runP do
      modelErrors <- loadCompileAndCacheArcFile "combinators" testDirectory
      if null modelErrors
        then do
          bindingAvailable <- getPropertyFunction "model:Combinators$TestCase$SomeRole$BindingAvailable"
          b <- (ContextInstance "model:User$MyTestCase") ##= getEnumeratedRoleInstances (EnumeratedRoleType "model:Combinators$TestCase$SomeRole") >=> bindingAvailable
          liftAff $ assert "The property value SomeRole$BindingAvailable should be false because there is no binding at all." (b == [Value "false"])
          -- log "Just after looking for the binding of SomeRole."
          bindingAvailable' <- getPropertyFunction "model:Combinators$TestCase$AnotherRole$BindingAvailable"
          b' <- (ContextInstance "model:User$MyTestCase") ##= getEnumeratedRoleInstances (EnumeratedRoleType "model:Combinators$TestCase$AnotherRole") >=> bindingAvailable'
          liftAff $ assert "The property value AnotherRole$BindingExists should be true, because there is a binding available and it is represented" (b' == [Value "true"])
          -- log "Just after looking for the binding of AnotherRole."
          -- remove the binding from cache without modifying the instance.
          (bnd :: Array RoleInstance) <- (ContextInstance "model:User$MyTestCase") ##= getEnumeratedRoleInstances (EnumeratedRoleType "model:Combinators$TestCase$AnotherRole") >=> binding
          for_ bnd removeInternally
          -- log "Removed the binding of AnotherRole."
          b'' <- (ContextInstance "model:User$MyTestCase") ##= getEnumeratedRoleInstances (EnumeratedRoleType "model:Combinators$TestCase$AnotherRole") >=> bindingAvailable'
          liftAff $ assert "The property value AnotherRole$BindingExists should be false after we remove the binding from cache" (b'' == [Value "false"])
          -- log "Just after looking for the binding of AnotherRole after removing it."
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )
