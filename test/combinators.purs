module Test.Combinators where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (null)
import Data.Foldable (for_)
import Effect.Aff.Class (liftAff)
import Perspectives.CoreTypes ((##=))
import Perspectives.Instances.ObjectGetters (binding, getRole)
import Perspectives.Query.Compiler (getPropertyFunction)
import Perspectives.Representation.Class.Cacheable (removeInternally)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance, Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..))
import Perspectives.TypePersistence.LoadArc (loadCompileAndCacheArcFile)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

theSuite :: Free TestF Unit
theSuite = suiteSkip "Combinators" do

  test "available" (runP do
      modelErrors <- loadCompileAndCacheArcFile "combinators" testDirectory
      if null modelErrors
        then do
          bindingAvailable <- getPropertyFunction "model:Combinators$TestCase$SomeRole$BindingAvailable"
          b <- (ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Combinators$TestCase$SomeRole") >=> bindingAvailable
          liftAff $ assert "The property value SomeRole$BindingExistes should be false because there is no binding at all." (b == [Value "false"])
          bindingAvailable' <- getPropertyFunction "model:Combinators$TestCase$AnotherRole$BindingAvailable"
          b' <- (ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Combinators$TestCase$AnotherRole") >=> bindingAvailable'
          liftAff $ assert "The property value AnotherRole$BindingExists should be true, because there is a binding available and it is represented" (b' == [Value "true"])
          -- remove the binding from cache without modifying the instance.
          (bnd :: Array RoleInstance) <- (ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Combinators$TestCase$AnotherRole") >=> binding
          for_ bnd removeInternally
          b'' <- (ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Combinators$TestCase$AnotherRole") >=> bindingAvailable'
          liftAff $ assert "The property value AnotherRole$BindingExists should be false after we remove the binding from cache" (b'' == [Value "false"])

        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )
