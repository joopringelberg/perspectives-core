module Test.Class.Role where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (concat, null)
import Data.Traversable (for)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.ModelDependencies (socialEnvironment, socialEnvironmentMe, sysUser, theSystem)
import Perspectives.Query.QueryTypes (RoleInContext(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Role (bindingOfADT, completeType, transitiveBindingOfADT)
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..))
import Perspectives.TypePersistence.LoadArc.FS (loadCompileAndCacheArcFile')
import Perspectives.Utilities (prettyPrint)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

-- Edit persistenceAPI.js first by declaring Pouchdb like this:
-- var PouchDB = require('pouchdb');

loadModels :: MonadPerspectives Unit
loadModels = do
  addAllExternalFunctions
  errs <- concat <$> for
    [ "couchdb"
    , "serialise"
    , "sensor"
    , "utilities"
    , "perspectivesSysteem"
    -- , "BodiesWithAccounts"
    -- , "parsing"
    -- , "files"
    -- , "rabbitMQ"
    -- , "BrokerServices"
    ] 
    (flip loadCompileAndCacheArcFile' modelDirectory)
  if null errs
    then pure unit
    else liftAff $ assert 
      ("There are instance- or model errors: " <> show errs) false

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Representation.Class.Role" do

  test "bindingOfADT" $ runP do
    loadModels

    bindingOfADT (SUM 
      [ (ST $ RoleInContext{role: EnumeratedRoleType sysUser, context: ContextType theSystem})
      , (ST $ RoleInContext{role: EnumeratedRoleType socialEnvironmentMe, context: ContextType socialEnvironment})])
        >>= showADT

    transitiveBindingOfADT (ST $ RoleInContext{role: EnumeratedRoleType sysUser, context: ContextType theSystem})
        >>= showADT

    completeType (ST $ RoleInContext{role: EnumeratedRoleType sysUser, context: ContextType theSystem})
        >>= showADT
    
    liftAff $ assert "" true
  
showADT :: forall a. Show a => ADT a -> MonadPerspectives Unit
showADT adt = do 
  log $ prettyPrint adt
  log "\n"