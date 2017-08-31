module Test.PropertyComposition1 where

import Prelude
import Perspectives.PropertyComposition
import Control.Monad.Aff (Aff, attempt, launchAff)
import Control.Monad.Aff.Console (log, logShow)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (throwError)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.String (length)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax)
import Network.HTTP.StatusCode (StatusCode(..))

{-
Tests:
1. Een functie die een resultaat geeft en een functie die een error gooit.
2. Een slagende en een falende asynchrone functie.
3. Een functie die een Array levert en één die een Maybe levert, in zodanige compositie dat alle overgangen getest worden.
-}

f :: forall e. Int -> Aff e (Maybe Int)
f n | n < 1 = throwError $ error "n is too small"
f n = pure (Just n)

g :: forall e. String -> Aff (ajax :: AJAX | e) (Maybe String)
g id = do
  res <- affjax $ userResourceRequest {url = baseURL <> id}
  case res.status of
    StatusCode 200 -> pure (Just res.response)
    otherwise -> throwError $ error "Wel verbinding, geen resource:"

baseURL :: String
baseURL = "http://localhost:5984/user_cor_contexts2/"

userResourceRequest :: AffjaxRequest Unit
userResourceRequest =
  { method: Left GET
  , url: "http://localhost:5984/user_cor_contexts2/user:xGebruiker"
  , headers: []
  , content: Nothing
  , username: Just "cor"
  , password: Just "geheim"
  , withCredentials: true
  }

h :: forall e. String -> Aff e (Maybe Int)
h s = pure $ Just (length s)

i :: forall a e. a -> Aff e (Array a)
i a = pure [a]

j :: forall e. Int -> Aff e (Maybe Int)
j b = pure (Just b)

test = launchAff do
  r <- attempt $ f 0
  _ <- case r of
    (Left err) -> log ("Error, message = " <> show err)
    (Right x) -> logShow x
  s <- attempt $ f 1
  _ <- case s of
    (Left err) -> log ("Error, message = " <> show err)
    (Right x) -> logShow x
  t <- attempt $ g "user:xGebruiker"
  _ <- case t of
    (Left err) -> log ("Error, message = " <> show err)
    (Right x) -> logShow x
  u <- attempt $ g "user:bestaatNiet"
  _ <- case u of
    (Left err) -> log ("Error, message = " <> show err)
    (Right x) -> logShow x
  -- v <- attempt $ (g >-> h) "user:xGebruiker"
  -- _ <- case v of
  --   (Left err) -> log ("Error, message = " <> show err)
  --   (Right x) -> logShow x
  -- w <- attempt $ (g >-> h >->> i) "user:xGebruiker"
  -- _ <- case w of
  --   (Left err) -> log ("Error, message = " <> show err)
  --   (Right x) -> logShow x
  -- x <- attempt $ (g >-> h >->> i >>->> i) "user:xGebruiker"
  -- _ <- case x of
  --   (Left err) -> log ("Error, message = " <> show err)
  --   (Right x) -> logShow x
  -- y <- attempt $ (g >-> h >->> i >>->> i >>-> j) "user:xGebruiker"
  -- _ <- case y of
  --   (Left err) -> log ("Error, message = " <> show err)
  --   (Right x) -> logShow x
  log "Tests done."
