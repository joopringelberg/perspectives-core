module Test.Perspectives.Representation.ADT.SpecialisesADT where

import Prelude

import Control.Monad.Free (Free)
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Representation.ADT2 (ADT(..), DNF, ExpandedADT(..), equalsOrSpecialisesADT, expand)
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
theSuite = suite "Test.Perspectives.Representation.ADT.SpecialisesADT" do

  test "equalsOrSpecialisesADT" $ runP do
    -- LEFT = ST
    comparisonTest (ST 1) (ST 1)
    comparisonTest (ST 1) (PROD [ST 1])
    comparisonTest (ST 1) (SUM [ST 1, ST 2])
    -- LEFT = PROD
    comparisonTest (PROD [ST 1, ST 2]) (ST 1)
    comparisonTest (PROD [ST 1, ST 2, ST 3]) (PROD [ST 1, ST 2])
    comparisonTest (PROD [ST 1, ST 2, ST 3]) (SUM [ST 4, PROD [ST 1, ST 2]])
    -- LEFT = SUM
    comparisonTest (SUM [ST 1, ST 2, ST 3]) (SUM [ST 1, SUM[ ST 1, ST 2, ST 4], SUM [ST 1, ST 3]])

    -- reversal of arguments
    -- RIGHT = ST
    flip negatedComparisonTest (PROD [ST 1, ST 2]) (ST 1)
    flip comparisonTest (ST 1) (PROD [ST 1])
    flip negatedComparisonTest (ST 1) (SUM [ST 1, ST 2])
    -- RIGHT = PROD
    flip negatedComparisonTest (PROD [ST 1, ST 2]) (ST 1)
    flip negatedComparisonTest (PROD [ST 1, ST 2, ST 3]) (PROD [ST 1, ST 2])
    flip negatedComparisonTest (PROD [ST 1, ST 2, ST 3]) (SUM [ST 4, PROD [ST 1, ST 2]])
    -- RIGHT = SUM
    flip negatedComparisonTest (SUM [ST 1, ST 2, ST 3]) (SUM [ST 1, SUM[ ST 1, ST 2, ST 4], SUM [ST 1, ST 3]])
    
    -- Other
    liftAff $ assert "not (ST 1 <= ST 2)" $ not (unwrap (equalsOrSpecialisesADT <$> expandInIdentity (ST 1) <*> expandInIdentity (ST 2)))


showDNF :: forall a. Show a => String -> DNF a -> MonadPerspectives Unit
showDNF comment adt = do 
  log comment
  log $ prettyPrint adt
  log "\n" 

expandInIdentity :: ADT String Int -> Identity (ExpandedADT String Int)
expandInIdentity = expand (unsafePartial expandInIdentity')
  where 
  expandInIdentity' :: Partial => ADT String Int -> Identity (ExpandedADT String Int)
  expandInIdentity' adt = case adt of
    ST x -> Identity $ EST x
    UET x -> Identity $ EST x
    CT label x -> ECT label <$> (expandInIdentity x)

comparisonTest_ :: (Boolean -> Boolean) -> ADT String Int -> ADT String Int -> MonadPerspectives Unit
comparisonTest_ op left right = liftAff $ assert showComparison (op $ unwrap (equalsOrSpecialisesADT <$> (expandInIdentity left) <*> (expandInIdentity right)))
  where
    showComparison :: String
    showComparison = "\n" <> prettyPrint left <> "\n <= \n" <> prettyPrint right

comparisonTest :: ADT String Int -> ADT String Int -> MonadPerspectives Unit
comparisonTest = comparisonTest_ identity

negatedComparisonTest :: ADT String Int -> ADT String Int -> MonadPerspectives Unit
negatedComparisonTest = comparisonTest_ not