module TestApply where

import Prelude
import Data.Either

nonEmpty :: String -> Either String Unit
nonEmpty "" = Left "Field cannot be empty"
nonEmpty _  = Right unit

newtype Person = Person
    { firstName :: String
    , lastName :: String
    }

joop :: Person
joop = Person{ firstName: "Joop", lastName: "Ringelberg"}

unsub :: Person
unsub = Person{ firstName: "", lastName: "Ringelberg"}

presult =
  let (Person o) = joop in
    (nonEmpty o.firstName) *> pure o.firstName
-- De applicative hier is Either String. Het kind daarvan is immers
-- Type -> Type, oftewel een type constructor zoals Array dat óók is.
-- Dat betekent dat *> hier werkt met de applicative Either String.
-- Daarom wordt pure hier óók geduid als van Either String.
-- | pure o.firstName is natuurlijk Right o.firstName (applyEither stelt: pure = Right).
-- |
-- | Hoe werkt map van Either e?
-- | f <$> Right x == Right (f x)
-- | f <$> Left y == Left y
-- |
-- Hoe werkt apply van Either e?
-- | Right f <*> Right x == Right (f x)
-- | Left f <*> Right x == Left x
-- | Right f <*> Left y == Left y
-- |
-- | En de definitie van *> luidt als volgt:
-- | Combine two effectful actions, keeping only the result of the second.
-- | applySecond :: forall a b f. Apply f => f a -> f b -> f b
-- | applySecond a b = const id <$> a <*> b

nresult =
  let (Person o) = unsub in
    (nonEmpty o.firstName) *> pure o.firstName

t0 = (Left "Field cannot be empty") *> (Right "Joop")
t1 = const id <$> (Left "Field cannot be empty") <*> (Right "Joop")
t2 = (Left "Field cannot be empty") <*> (Right "Joop")
t3 = (Left "Field cannot be empty")

r0 = (Right unit) *> (Right "Joop")
r1 = const id <$> (Right unit) <*> (Right "Joop")
r2 = (Right (const id unit)) <*> (Right "Joop")
r3 = Right id <*> (Right "Joop")
r4 = Right( id "Joop")
r5 = Right "Joop"
