module Test.ArrayT where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Trans.Class (lift)
import Data.Int (odd)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.Instances.Combinators (filter)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (assert)

theSuite :: Free TestF Unit
theSuite = suite "ArrayT" do

  test "filter" (do
    r <- runArrayT ((filter numbers odd_) 0)
    -- logShow r
    liftAff $ assert "We should have [1, 3]" (r == [1, 3])
    s <- runArrayT ((filter numbers (odd_ >=> not_)) 0)
    liftAff $ assert "We should have [2, 4]" (s == [2, 4])
    t <- runArrayT ((filter numbers (lift <<< odd') 0))
    liftAff $ assert "We should have [1, 3]" (t == [1, 3])
    u <- runArrayT ((filter numbers (lift <<< odd' >=> not_)) 0)
    liftAff $ assert "We should have [2, 4]" (u == [2, 4])
    v <- runArrayT ((filter numbers (lift <<< odd' >=> pure <<< not)) 0)
    liftAff $ assert "We should have [2, 4]" (v == [2, 4])
    )


numbers :: Int -> ArrayT Aff Int
numbers _ = ArrayT $ pure [1, 2, 3, 4]

odd_ :: Int -> ArrayT Aff Boolean
odd_ n = ArrayT $ pure [odd n]

not_ :: Boolean -> ArrayT Aff Boolean
not_ b = ArrayT $ pure [not b]

odd' :: Int -> Aff Boolean
odd' n = pure $ odd n

-- x :: Int ~~> Boolean

x :: Int -> ArrayT Aff Boolean
-- x n = lift (odd' n)
x = lift <<< odd'

y :: Boolean -> ArrayT Aff Boolean
y = pure <<< not
