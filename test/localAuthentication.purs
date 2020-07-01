module Test.LocalAuthentication where

import Prelude

import Control.Monad.Free (Free)
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
  test "authenticate" do
    r <- authenticate "cor" "geheim" couchdbHost couchdbPort
    assert "'authenticate cor geheim' should return OK" case r of
      OK _ -> true
      otherwise -> false
    r1 <- authenticate "onbekend" "geheim" couchdbHost couchdbPort
    assert "'authenticate onbekend geheim' should return UnknownUser" (r1 == UnknownUser)
    r2 <- authenticate "cor" "fout" couchdbHost couchdbPort
    assert "'authenticate cor fout' should return WrongPassword" (r2 == WrongPassword)
