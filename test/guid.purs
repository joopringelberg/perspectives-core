module Test.Guid where

import Prelude

import Control.Monad.Free (Free)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Perspectives.Guid (guid)
import Test.Unit (TestF, suite, suiteOnly, test)
import Test.Unit.Assert (assert)

theSuite :: Free TestF Unit
theSuite = suite "Guid" do

  test "guid" do
    g <- liftEffect $ guid
    h <- liftEffect $ guid
    logShow g
    logShow h
    assert "Two guids should not be equal" (not (g == h))
