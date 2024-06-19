module Test.Perspectives.Representation.ADT.DisjunctiveNormalForm where

import Prelude

import Control.Monad.Free (Free)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Representation.ADT (DNF, ExpandedADT(..), toDisjunctiveNormalForm)
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

theSuite :: Free TestF Unit
theSuite = suite "Test.Perspectives.Representation.ADT.DisjunctiveNormalForm" do

  test "toDisjunctiveNormalForm" $ runP do
    showDNF  "Simple type:" $ toDisjunctiveNormalForm 
      (EST 1)
    showDNF "Product:" $ toDisjunctiveNormalForm 
      (EPROD [EST 1, EST 2])
    showDNF "Nested Product 1:" $ toDisjunctiveNormalForm 
      (EPROD [EST 1, EPROD [EST 2]])
    showDNF "Nested Product 2" $ toDisjunctiveNormalForm 
      (EPROD [EST 1, EPROD [EST 2, EST 3]])
    showDNF "Nested Product 3" $ toDisjunctiveNormalForm 
      (EPROD [EST 1, EPROD [EST 2, EPROD [EST 3]]])
    showDNF "Sum in Product:" $ toDisjunctiveNormalForm 
      (EPROD [EST 1, ESUM [EST 2, EST 3]])
    showDNF "Sum in Sum:" $ toDisjunctiveNormalForm 
      (ESUM [EST 1, ESUM [EST 2, EST 3]])
    showDNF "Product in Sum:" $ toDisjunctiveNormalForm 
      (ESUM [EST 1, EPROD [EST 2, EST 3]])
    showDNF "Product of Sum of Product" $ toDisjunctiveNormalForm
      (EPROD [(ESUM [EPROD [EST 1, EST 2], EPROD [EST 11, EST 12]]), ESUM [EPROD [EST 3, EST 4], EPROD [EST 13, EST 14]]])
    -- showDNF "EMPTY:" $ toDisjunctiveNormalForm 
    --   (EMPTY :: ADT Int)
    -- showDNF "UNIVERSAL:" $ toDisjunctiveNormalForm 
    --   (UNIVERSAL :: ADT Int)
    -- showDNF "EMPTY in Product ([] x y = []):" $ toDisjunctiveNormalForm 
    --   (EPROD [EST 1, EMPTY])
    -- showDNF "EMPTY in Sum ([] + y = y):" $ toDisjunctiveNormalForm 
    --   (ESUM [EST 1, EMPTY])
    -- showDNF "UNIVERSAL in Product (UNIVERSAL x y = UNIVERSAL):" $ toDisjunctiveNormalForm 
    --   (EPROD [EST 1, UNIVERSAL])
    -- showDNF "UNIVERSAL in Sum (1 + UNIVERSAL = y):" $ toDisjunctiveNormalForm 
    --   (ESUM [EST 1, UNIVERSAL])


    liftAff $ assert "" true
  
showDNF :: forall a. Show a => String -> DNF a -> MonadPerspectives Unit
showDNF comment adt = do 
  log comment
  log $ prettyPrint adt
  log "\n" 