module Test.LocalAuthentication where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe)
import Effect.Class.Console (logShow)
import Perspectives.Couchdb.Databases (getDocument)
import Perspectives.CouchdbState (CouchdbUser(..), runMonadCouchdb)
import Perspectives.LocalAuthentication (AuthenticationResult(..), authenticate)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "/Users/joopringelberg/Code/perspectives-core/test"

couchdbHost :: String
couchdbHost = "http://127.0.0.1"

couchdbPort :: Int
couchdbPort = 5984

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.LocalAuthentication" do

  -- Note that this test depends on there being a System Admin "joop" with password "geheim" and no others.
  test "authenticate: OK" do
    r <- authenticate "joop" "geheim" couchdbHost couchdbPort
    -- logShow r
    assert "'authenticate joop geheim' should return OK" case r of
      OK _ -> true
      otherwise -> false

  -- These tests cannot be run immediately as the session will still be authorised!!
  -- r1 <- authenticate "onbekend" "geheim" couchdbHost couchdbPort
  -- logShow r1
  -- assert "'authenticate onbekend geheim' should return WrongCredentials" (r1 == WrongCredentials)
  -- r2 <- authenticate "cor" "fout" couchdbHost couchdbPort
  -- assert "'authenticate cor fout' should return WrongCredentials" (r2 == WrongCredentials)
  -- r3 <- authenticate "cor" "geheim" couchdbHost couchdbPort
  -- logShow r3
  -- assert "'authenticate cor geheim' should return UnknownUser" (r3 == UnknownUser)
