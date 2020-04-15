module Test.Utilities
where

import Prelude

import Control.Monad.Free (Free)
import Effect.Class.Console (log)
import Perspectives.Utilities (prettyPrint)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)

type MyRecordType = {a :: Int, b :: Boolean}

myRecord :: MyRecordType
myRecord = {a: 1, b: true}

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Utilities" do

  test "prettyPrint record" (do
    log $ "\n" <> (prettyPrint myRecord)
    log $ "\n" <> (prettyPrint {a: 1, b: {c: 2, d: 3}})
    log (prettyPrint [1, 2, 3])
    log $ "\n" <> (prettyPrint [{a: 1, b: 10}, {a: 2, b: 3}, {a: 10, b: 20}])
    )
