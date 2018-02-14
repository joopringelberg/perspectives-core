module Research.Lens where

import Data.Lens
import Data.Lens.At
import Data.Lens.Index
import Data.List
import Data.Traversable
import Data.Monoid.Additive
import Data.StrMap as StrMap
import Data.Lens.Record (prop)
import Data.Lens.Types (Lens)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (class Strong)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Prelude (class Show, mod, (<<<), (<>), (==))

newtype Recipe = Recipe { title :: String }

instance showRecipe :: Show Recipe where show (Recipe {title}) = "Recipe: " <> title

recipe :: Recipe
recipe = Recipe { title : "Brownies" }

_title :: forall p. Strong p => p String String -> p Recipe Recipe
_title = lens (\(Recipe {title}) -> title) (\(Recipe r) value -> Recipe (r {title = value}))

{-
view _title recipe

viewOn recipe _title
recipe ^. _title

over _title (const "New Recipe") recipe
(_title %~ (const "New Recipe")) recipe
_title %~ (const "New Recipe") $ recipe
recipe # (_title %~ (const "New Recipe"))

a :: Maybe String
a = Nothing

b :: Maybe String
b = Just "abc"

view _Just a
view _Just b

over _Just (flip append "!!") a
over _Just (flip append "!!") b

-}

strmap :: StrMap.StrMap String
strmap = StrMap.fromFoldable [Tuple "user" "john", Tuple "id" "1"]

-- strmap ^. (at "user")

-- [1,2,3] # ix 2 .~ 20
-- set (ix 2) 20 $ [1, 2, 3]


-- [1, 2, 3] ^.. traversed
-- toListOf traversed [1, 2, 3]

even :: Int -> Boolean
even a = mod a 2 == 0

-- allOf (traversed <<< _2) even [Tuple 2 1]
-- allOf (traversed <<< _2) even [Tuple 2 2, Tuple 3 4]


-- strmap ^. at "name" <<< non "Not Found"
-- strmap ^. at "user" <<< non "Not Found"


-- ["hello", " ", "world"] ^. traversed
-- ([] ^. traversed) :: String
-- ([] ^. traversed) :: List Int


-- fold $ map Additive [1,2,3]
-- [1,2,3,4] ^. (to (foldMap Additive))

foo :: forall a b r. Lens { foo :: a | r } { foo :: b | r } a b
foo = prop (SProxy :: SProxy "foo")

bar :: forall a b r. Lens { bar :: a | r } { bar :: b | r } a b
bar = prop (SProxy :: SProxy "bar")

barAndFoo :: forall a b r. Getter' { bar :: a, foo :: b | r } (Tuple a b)
barAndFoo = takeBoth bar foo

type Foo a = { foo :: Maybe { bar :: Array a } }

doc :: Foo String
doc = { foo: Just { bar: [ "Hello", " ", "World" ]} }

bars :: forall a b. Traversal (Foo a) (Foo b) a b
bars = foo <<< _Just <<< bar <<< traversed
