module Try.NullOrUndefined where

import Prelude
import Data.Foreign.NullOrUndefined
import Data.Maybe
import Control.Monad.Except (Except, runExcept)
import Data.Either (Either, fromRight)
import Data.Foreign (F, MultipleErrors)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, unwrap, wrap)
import Partial.Unsafe (unsafePartial)

newtype Binding = Binding (Maybe String)

derive instance newtypeBinding :: Newtype Binding _

b :: Binding
b = Binding (Just "id")

c :: Maybe String
c = unwrap b

d :: NullOrUndefined String
d = NullOrUndefined (Just "aap")

e :: Maybe String
e = unwrap d

f :: NullOrUndefined String
f = wrap Nothing

g :: Maybe String
g = unwrap f

newtype T0 = T0 { intArray :: Array Int, optionalInt :: Maybe Int}

derive instance repGenericT0 :: Generic T0 _

instance showT0 :: Show T0 where
  show x = genericShow x

newtype T1 = T1 { intArray :: Array Int, optionalInt :: NullOrUndefined Int}

derive instance newtypeT1 :: Newtype T1 _

derive instance repGenericT1 :: Generic T1 _

instance decodeT1 :: Decode T1 where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance encodeT1 :: Encode T1 where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

test1 :: T1
test1 = T1 { intArray: [1,2,3], optionalInt: wrap Nothing}

instance showT1 :: Show T1 where
  show x = genericShow x

roundtrip1 :: T1
roundtrip1 = unsafePartial $ fromRight $ runExcept $ decodeJSON $ encodeJSON test1

h :: { intArray :: Array Int, optionalInt :: NullOrUndefined Int}
h = unwrap test1

i :: Array Int
i = _.intArray h

-- j = wrap ((unwrap test1) {optionalInt = unwrap <<< _.optionalInt})

j :: {intArray :: Array Int, optionalInt :: Maybe Int}
-- j = (\r -> r {optionalInt = unwrap (_.optionalInt r)}) (unwrap test1)
j = (\r -> r {optionalInt = unwrap r.optionalInt}) (unwrap test1)
-- j = (_ {optionalInt = unwrap _.optionalInt}) (unwrap test1)

t0Naart1 :: T0 -> T1
t0Naart1 (T0 r@{optionalInt}) = T1 $ r {optionalInt = wrap optionalInt}

t1Naart0 :: T1 -> T0
t1Naart0 (T1 r@{optionalInt}) = T0 $ r {optionalInt = unwrap optionalInt}

test2 :: T0
test2 = T0 {intArray: [1,2,3], optionalInt : Nothing}

encoded_test2 :: String
encoded_test2 = encodeJSON $ t0Naart1 test2

decoded_test2 :: T0
decoded_test2 = t1Naart0 $ unsafePartial $ fromRight $ runExcept $ decodeJSON encoded_test2

{-
newtype T2 = T2 {a :: Int}

derive instance repGenericT2 :: Generic T2 _

derive instance newtypeT2 :: Newtype T2 _

newtype T3 = T3 {a :: Int, b :: String}

derive instance repGenericT3 :: Generic T3 _

derive instance newtypeT3 :: Newtype T3 _

t2Naart3 :: T3 -> T2
t2Naart3 x = wrap (unwrap x)
-}
