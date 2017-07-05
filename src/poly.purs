-- | A test with a polymorphism

module Test.Poly where

import Prelude
import Data.Maybe
import Perspectives.Updatable



poly :: (forall a. a -> a) -> Boolean
poly f = (f 0 < 1) == f true

--test = poly testf

testf n = { foo: n }

identity x = x

type Edge r = { target :: Int | r }

getTarget :: forall r. Edge r -> Int
getTarget e = e.target

r = {target: 1}

test = getTarget {target: 1}

type PersonRecord r = { firstName :: String, lastName :: String | r }

fullName :: forall r. PersonRecord r -> String
fullName person = person.firstName <> " " <> person.lastName

test2 = fullName { firstName: "Joop", lastName: "Ringelberg"}
