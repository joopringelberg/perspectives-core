module Test.TestTypes2 where

import Prelude
import Data.Maybe

data LiteralType = IntL | BoolL
newtype Literal = Literal { value :: String, type :: LiteralType}

sumLiterals :: Literal -> Literal -> Maybe Literal
sumLiterals (Literal {value: v1, type: IntL}) (Literal {value: v2, type: IntL}) = Just (Literal { value : sumIntStrings v1 v2, type: IntL })
sumLiterals _ _ = Nothing

andLiterals :: Literal -> Literal -> Maybe Literal
andLiterals (Literal {value: v1, type: BoolL}) (Literal {value: v2, type: BoolL}) = Just (Literal { value : andBoolStrings v1 v2, type: BoolL })
andLiterals _ _ = Nothing


foreign import parseInt :: String -> Int
foreign import stringifyInt :: Int -> String
foreign import sumIntStrings :: String -> String -> String
foreign import andBoolStrings :: String -> String -> String
foreign import literalMap :: forall a b. (a->b) -> String -> LiteralType -> String

l1 = Literal{ value: "1", type: IntL}
l2 = Literal{ value: "2", type: IntL}
l3 = sumLiterals l1 l2

b1 = Literal{ value: "true", type: BoolL}
b2 = Literal{ value: "true", type: BoolL}
b3 = andLiterals b1 b2

instance showLiteral :: Show Literal where
  show (Literal{ value, type: t }) = "{" <> value <> ", " <> show t <> "}"

instance showLiteralType :: Show LiteralType where
  show IntL = "IntL"
  show BoolL = "BoolL"

fcallOnLiteral :: forall a b. (a -> b) -> Literal -> Literal
fcallOnLiteral f l@(Literal{ value, type: t}) = Literal{ value: literalMap f value t, type: t}

literalAdd1 = fcallOnLiteral ((+)1)
l4 = literalAdd1 l1
