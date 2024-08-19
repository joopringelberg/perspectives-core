module Test.Perspectives.Representation.ADT2 where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (singleton)
import Data.Foldable (foldMap)
import Data.Identity (Identity(..))
import Data.Int (even)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect.Class.Console (log)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Representation.ADT (ADT(..), HArray(..), collect, computeBoolean, computeCollection, expand, foldMapADT)
import Perspectives.Representation.CNF (CNF, toConjunctiveNormalForm)
import Perspectives.Representation.ExpandedADT (ExpandedADT(..))
import Perspectives.Utilities (class PrettyPrint, prettyPrint)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, test)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

-- Edit persistenceAPI.js first by declaring Pouchdb like this:
-- var PouchDB = require('pouchdb');

t1 :: ADT Int
t1 = (PROD
        [ ST 1
        , UET 2
        , SUM [ST 1, ST 2]
        , PROD [ST 3]
        ])

theSuite :: Free TestF Unit
theSuite = suite "Test.Perspectives.Representation.ADT" do

  test "Functor" $ runP do
    result <- pure $ unwrap $ (toConjunctiveNormalForm <$> expand (unsafePartial expandInIdentity)
      (((*) 10) <$> (PROD [(SUM [PROD [ST 1, ST 2], PROD [ST 11, ST 12]]), SUM [PROD [ST 3, ST 4], PROD [ST 13, ST 14]]])))
    showDNF "map times 10:" $ result
    
    showADT "map times 20, all data constructors:" $ ((*) 20) <$> 
      (PROD
        [ ST 1
        , UET 2
        , SUM [ST 1, ST 2]
        , PROD [ST 3]
        ])

  test "Foldable" $ runP do
    showFold "foldMap even" $ foldMap (Conj <<< even )
      (PROD
        [ ST 1
        , UET 2
        , SUM [ST 1, ST 2]
        , PROD [ST 3]
        ])

    showFold "foldMap Conj even: true if all even numbers" $ foldMap (Conj <<< even) t1

    showFold "foldMap Disj even: true if at least one even number" $ foldMap (Disj <<< even ) t1

    showFold "foldMap Array: collect all terminals in Array" $ foldMap singleton t1

    showFold "foldMap Additive: sum all numbers" $ foldMap Additive t1

    showFold "foldMap Multiplicative: multiply all numbers" $ foldMap Multiplicative t1
      
  test "Traversable" $ runP do
    showFold "traverse Nothing" $ traverse (\a -> if even a then Just a else Nothing) t1

    showFold "traverse Just" $ traverse Just t1

    showFold "traverse Array" $ traverse singleton t1

    -- showFold "traverse to expand" $ traverse 

  test "foldMapADT" $ runP do
    showFold "foldMapADT Conj even: true if all even numbers" $ foldMapADT even (PROD [PROD [ST 2, ST 4], SUM [ST 2, ST 1]])

    showFold "foldMapADT SUM HArray <<< singleton:" $ foldMapADT (HArray <<< singleton) (SUM [ST 6, ST 7])

    showFold "foldMapADT PROD HArray <<< singleton:" $ foldMapADT (HArray <<< singleton) (PROD [ST 6, ST 7])

    showFold "foldMapADT HArray <<< singleton:" $ foldMapADT (HArray <<< singleton) (PROD [SUM [ST 5, ST 6], SUM [ST 6, ST 7]])

    showFold "foldMapADT HArray <<< singleton:" $ foldMapADT (HArray <<< singleton) (SUM [PROD [SUM [ST 10, ST 5], ST 5], SUM [ST 2, ST 1]])
  
  test "computeBoolean" $ runP do
    showFold "computeBoolean even" $ computeBoolean even t1

    showFold "computeBoolean even (2)" $ computeBoolean even (PROD [PROD [ST 2, ST 4], SUM [ST 2, ST 1]])

  test "computeCollection" $ runP do
    showFold "computeCollection SUM singleton:" $ computeCollection singleton (SUM [ST 6, ST 7])

    showFold "computeCollection SUM singleton:" $ computeCollection singleton (SUM [PROD [SUM [ST 10, ST 5], ST 5], SUM [ST 2, ST 1]])
  
  test "expand" $ runP do
    showFold "expand UET to ST with Array" $ expand (unsafePartial expandInArray) (PROD [UET 1, ST 2])
      
    showFold "expand UET to ST with Identity" $ expand (unsafePartial expandInIdentity) (PROD [UET 1, ST 2])

  test "expand" $ runP do
    showFold "foldMap Array: collect all terminals in Array" $ flip collect t1 (unsafePartial 
      \adt -> case adt of 
        ST a -> [show a]
        UET a -> [show a])

expandInArray :: Partial => ADT Int -> Array (ExpandedADT Int)
expandInArray adt = case adt of
  ST x -> [EST x]
  UET x -> [EST x]

expandInIdentity :: Partial => ADT Int -> Identity (ExpandedADT Int)
expandInIdentity adt = case adt of
  ST x -> Identity $ EST x
  UET x -> Identity $ EST x

showADT :: forall a. Show a => String -> ADT a -> MonadPerspectives Unit
showADT comment adt = do 
  log comment 
  log $ prettyPrint adt
  log "\n"

showFold :: forall a. Show a => String -> a -> MonadPerspectives Unit
showFold comment result = do
  log comment
  log (" " <> show result <> "\n")

showExpandedADT :: forall a. Show a => String -> ExpandedADT a -> MonadPerspectives Unit
showExpandedADT comment adt = do 
  log comment 
  log $ prettyPrint adt
  log "\n"

showDNF :: forall a. Show a => PrettyPrint a => String -> CNF a -> MonadPerspectives Unit
showDNF comment adt = do 
  log comment
  log $ prettyPrint adt
  log "\n" 