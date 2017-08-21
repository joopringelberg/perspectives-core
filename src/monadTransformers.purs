module Temp.MonadTransformers where

import Prelude
import Control.Bind ((<=<), (>>=))
import Control.Monad.Aff (Aff, launchAff, runAff)
import Control.Monad.Aff.Console (CONSOLE, error)
import Control.Monad.Eff (runPure)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, throwError)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

{-
I want to stack Either String and Maybe.
Maybe must be the 'inner' Monad, i.o.w. the underlying Monad.
That is, I want the type:
  Either String (Maybe a).
Hence we need to use the transformer MaybeT.
There is an inversion. Run a computation of type:
  MaybeT (Either Error) a
with runMaybeT and you get a value of type:
  Either Error (Maybe a)

The outer Monad is Maybe, the underlying Monad is Either Error.
-}

type Error = String

type EitherAndMaybe a = MaybeT (Either Error) a

errorWithMaybe :: EitherAndMaybe Int
errorWithMaybe = do
  _ <- throwError ("Er is iets fout!")
  pure 1

y :: Either String (Maybe Int)
y = runMaybeT errorWithMaybe

y' :: Either String (Maybe Int)
y' = runMaybeT
  do
    pure 1

f :: Int -> EitherAndMaybe Int
f n | n < 1 = throwError "n is te klein"
f n = pure n

z :: Either String (Maybe Int)
z = runMaybeT $ f 3

g :: Int -> EitherAndMaybe Int
g n = pure (n * n)

a = runMaybeT $ f 3 >>= g
a' = runMaybeT $ (f <=< g) 3

b = runMaybeT $ (f <=< g) 0

type AffEitherMaybe e a = MaybeT (ExceptT Error (Aff e)) a

f' :: forall e. Int -> AffEitherMaybe e Int
f' n | n < 1 = throwError "n is te klein"
f' n = pure n

g' :: forall e. Int -> AffEitherMaybe e Int
g' n = pure (n * n)

-- testp' = p 3

run = runExceptT <<< runMaybeT

testf = run $ f' 2

testfg = run $ (f' <=< g') 2

test = launchAff do
  r <- testfg
  case r of
    (Left err) -> print err
    (Right mi) -> print $ show mi
  s <- run $ (f' <=< g') 0
  case s of
    (Left err) -> print err
    (Right mi) -> print $ show mi

print :: forall e a. MonadEff (console :: CONSOLE | e) a => String -> a Unit
print = liftEff <<< log


{-
I have functions whose result type is
    forall e a. Aff e (Either Error (Maybe a))
for some extensible set of effects e and where Error is an alias for String.
I would like to Kleisli-compose these functions.
Now I've constructed a stack of Maybe and Either Error using MaybeT:
    type EitherAndMaybe a = MaybeT (Either Error) a
Given:
    f :: Int -> EitherAndMaybe Int
    f n | n < 1 = throwError "n is too small"
    f n = pure n
and
    g :: Int -> EitherAndMaybe Int
    g n = pure (n * n)
I can compose:
    a = runMaybeT $ (f <=< g) 3
    b = runMaybeT $ (f <=< g) 0
where a is Right (Just 9) and b is Left "n is too small", the type of a and b being
    Either Error (Maybe Int)
So the next step would be to add Aff to this stack - but I cannot figure out how. I've tried:
    type AffEitherMaybe e a = MaybeT (ExceptT Error (Aff e)) a
Change the type of f and g to this type and one can compose:
    run = runExceptT <<< runMaybeT
    c = run $ (f <=< g) 3
    d = run $ (f <=< g) 0
However, the type of c and d is then
    forall e. Aff e Either Error (Maybe Int)

-}
