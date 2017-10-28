module Perspectives.SignalTest where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Prelude (pure)
import Signal (Signal, constant, map2, unwrap)
import Signal.Channel (CHANNEL, Channel, channel, send, subscribe)

type Subject = String
type Predicate = String
type Object = String

type Getter e = Subject -> Eff e Object

type Triple = Channel Object
type QueryResult = Signal Object

getGetter :: forall e. Predicate -> Getter e
getGetter p s = case s of
  "a" -> case p of
    "p" -> pure "b"
    otherwise -> pure "bottom"
  "b" -> case p of
    "b" -> pure "c"
    otherwise -> pure "bottom"
  otherwise -> pure "bottom"

makeTriple :: forall e.
  Subject
  -> (Subject
      -> Eff
           ( channel :: CHANNEL
           | e
           )
           Triple
     )
     -> Eff
          ( channel :: CHANNEL
          | e
          )
          (Channel Triple)
makeTriple s p = do
  o <- p s
  channel o

updateTriple :: forall e. Triple -> Object -> Eff (channel :: CHANNEL | e) Unit
updateTriple = send

lookupInTripleIndex :: forall e. Subject -> Predicate -> Eff (channel :: CHANNEL | e) Triple
lookupInTripleIndex s p = do
  o <- getGetter p s
  channel o

type TripleGetter e = Subject -> Eff (channel :: CHANNEL | e) Triple

getTripleGetter :: forall e. Predicate -> TripleGetter e
getTripleGetter p s = lookupInTripleIndex s p

type QueryGetter e = Subject -> Eff (channel :: CHANNEL | e) QueryResult

getQueryGetter :: forall e. Predicate -> QueryGetter e
getQueryGetter p s = do
  triple <- (getTripleGetter p s)
  pure (query triple)

query :: Triple -> QueryResult
query = subscribe

combine :: forall e.
  QueryGetter e
  -> QueryGetter e
     -> QueryGetter e
combine p q s = do
  (so :: Signal Object) <- p s
  (x :: Signal (Eff (channel :: CHANNEL | e) (Signal String))) <- pure (map q so)
  (y :: Signal (Signal String)) <- unwrap x
  pure (magic y)

foreign import magic :: forall a. Signal (Signal a) -> Signal a
------------------------------------------------------------
-- A definition for bind to make Signal an instance of Bind.
bind1 :: forall a b. Signal a -> (a -> Signal b) -> Signal b
bind1 (sa :: Signal a) f =
  let (ssb :: Signal (Signal b)) = map f sa
  in magic ssb

pure1 :: forall a. a -> Signal a
pure1 = constant

-- Check magic for monad laws.
-- Right identity:
-- do
--   x <- expr
--   pure x
--
-- pure expr >>= (\x -> x) === expr
-- bind (pure expr) (\x -> x) === expr

firstLaw :: forall a. Signal a -> Signal a
firstLaw sig = bind1 sig (\x -> pure x)

-- Postulating equality for Signals in terms of equality of their contents - now!
firstLawValid :: forall a. Eq a => a -> Signal Boolean
firstLawValid n = let sig = constant n
  in map2 (\l r -> l == r) sig (firstLaw sig)

-- Left identity law:
-- pure y >>= pure == pure y

secondLaw :: forall a. Signal a -> Signal a
secondLaw sig = bind1 sig pure1

secondLawValid :: forall a. Eq a => a -> Signal Boolean
secondLawValid n = let sig = constant n
  in map2 (\l r -> l == r) sig (secondLaw sig)

-- Associativity law:
-- Doing (m >>= f) >>= g is just like doing m >>= (\x -> f x >>= g)


------------------------------------------------------------
test :: forall e.
  Eff
    ( channel :: CHANNEL
    , console :: CONSOLE
    | e
    )
    (Signal Unit)
test = do
  (t :: Triple) <- (lookupInTripleIndex "a" "p")
  (q :: QueryResult) <- pure (query t)
  unwrap (map log q)

run :: forall t10 t5. Eff t10 t5 -> Unit
run = (const unit) <<< unsafePerformEff
-- run test

testTriple et = do
  t <- et
  (q :: QueryResult) <- pure (query t)
  unwrap (map log q)

t1 = lookupInTripleIndex "a" "p"
