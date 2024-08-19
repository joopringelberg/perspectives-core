module Test.Perspectives.Representation.ADT.DisjunctiveNormalForm where

import Prelude

import Control.Monad.Free (Free)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Representation.CNF (CNF, toConjunctiveNormalForm)
import Perspectives.Representation.ExpandedADT (ExpandedADT(..))
import Perspectives.Utilities (class PrettyPrint, prettyPrint)
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

  test "toConjunctiveNormalForm" $ runP do
    showDNF  "Simple type:" $ toConjunctiveNormalForm 
      (EST 1)
    showDNF "Sum:" $ toConjunctiveNormalForm 
      (ESUM [EST 1, EST 2])
    showDNF "Nested Sum 1:" $ toConjunctiveNormalForm 
      (ESUM [EST 1, ESUM [EST 2]])
    showDNF "Nested Sum 2" $ toConjunctiveNormalForm 
      (ESUM [EST 1, ESUM [EST 2, EST 3]])
    showDNF "Nested Sum 3" $ toConjunctiveNormalForm 
      (ESUM [EST 1, ESUM [EST 2, ESUM [EST 3]]])
    showDNF "Product in Sum:" $ toConjunctiveNormalForm 
      (ESUM [EST 1, EPROD [EST 2, EST 3]])
    showDNF "Product in Product:" $ toConjunctiveNormalForm 
      (EPROD [EST 1, EPROD [EST 2, EST 3]])
    showDNF "Sum in product:" $ toConjunctiveNormalForm 
      (EPROD [EST 1, ESUM [EST 2, EST 3]])
    showDNF "Sum of Product of Sum" $ toConjunctiveNormalForm
      (ESUM [(EPROD [ESUM [EST 1, EST 2], ESUM [EST 11, EST 12]]), EPROD [ESUM [EST 3, EST 4], ESUM [EST 13, EST 14]]])
    -- showDNF "EMPTY:" $ toConjunctiveNormalForm 
    --   (EMPTY :: ADT Int)
    -- showDNF "UNIVERSAL:" $ toConjunctiveNormalForm 
    --   (UNIVERSAL :: ADT Int)
    -- showDNF "EMPTY in Product ([] x y = []):" $ toConjunctiveNormalForm 
    --   (ESUM [EST 1, EMPTY])
    -- showDNF "EMPTY in Sum ([] + y = y):" $ toConjunctiveNormalForm 
    --   (EPROD [EST 1, EMPTY])
    -- showDNF "UNIVERSAL in Product (UNIVERSAL x y = UNIVERSAL):" $ toConjunctiveNormalForm 
    --   (ESUM [EST 1, UNIVERSAL])
    -- showDNF "UNIVERSAL in Sum (1 + UNIVERSAL = y):" $ toConjunctiveNormalForm 
    --   (EPROD [EST 1, UNIVERSAL])


    liftAff $ assert "" true
  
showDNF :: forall a. Show a => PrettyPrint a => String -> CNF a -> MonadPerspectives Unit
showDNF comment adt = do 
  log comment
  log $ prettyPrint adt
  log "\n" 