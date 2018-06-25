--
-- module Main where
--
-- import Test.TypeDefChecker (test)
-- import Test.TestEffects as TE
-- import Control.Monad.Aff (Aff, Fiber, runAff, runAff_)
-- import Control.Monad.Aff.Console (CONSOLE, log) as AC
-- import Control.Monad.Eff (Eff)
-- import Perspectives.PerspectivesState (runPerspectives)
-- import Prelude (class Show, Unit, pure, unit, (>>=), show)
--
-- -- import Test.BoundContexts
--
-- main :: forall e. Eff (TE.CancelerEffects e) (Fiber (TE.CancelerEffects e) Unit)
-- main = runAff TE.handleError (runPerspectives "cor" "geheim" test)






module Main where
import Control.Monad.Aff (Aff, Error, error, liftEff', runAff, throwError)
import Control.Monad.Aff.AVar (AVar, makeVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location (search)
import DOM.HTML.Window (location)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (fromFoldable, lookup)
import Data.Tuple (Tuple(..))
import Data.URI.Query (Query(..), parser) as URI
import Perspectives.Api (setupApi)
import Perspectives.Couchdb (User, Password)
import Perspectives.Effects (AjaxAvarCache, AvarCache, REACT)
import Perspectives.PerspectivesState (newPerspectivesState, runPerspectivesWithState)
import Prelude (Unit, bind, pure, ($), (<$>), (>>=), (<>), show, void)
import Text.Parsing.StringParser (ParseError, runParser)

main :: Eff (AjaxAvarCache (console :: CONSOLE, dom :: DOM, react :: REACT)) Unit
main = void $ runAff handleError do
  mt <- credentialsFromQueryString
  case mt of
    Nothing -> throwError $ error "Both the user and password key-value pairs have to be present in the query string!"
    (Just (Tuple usr pwd)) -> do
      -- TODO: retrieve the couchdb credentials from the trusted cluster or through the user interface.
      url <- pure "http://127.0.0.1:5984/"
      (av :: AVar String) <- makeVar "This value will be removed on first authentication!"
      state <- makeVar $ newPerspectivesState {userName: usr, couchdbPassword: pwd, couchdbBaseURL: url} av
      runPerspectivesWithState setupApi state

-- TODO. Als geen waarde in usr beschikbaar is, moet de gebruiker een naam (opnieuw) invoeren!
credentialsFromQueryString :: forall e. Aff (AvarCache (dom :: DOM | e)) (Maybe (Tuple User Password))
credentialsFromQueryString = do
  (parseResult :: Either ParseError URI.Query) <- liftEff' ((runParser URI.parser) <$> (window >>= location >>= search))
  case parseResult of
    (Left m) -> pure Nothing
    (Right (URI.Query kvp)) -> case lookup "user" $ fromFoldable kvp of
      (Just (Just usr)) ->
        case lookup "password" $ fromFoldable kvp of
          (Just (Just pwd)) -> pure $ Just (Tuple usr pwd)
          otherwise -> pure Nothing
      otherwise -> pure Nothing

handleError :: forall e a. (Either Error a -> Eff (console :: CONSOLE | e) Unit)
handleError (Left e) = log $ "An error condition: " <> (show e)
handleError (Right a) = log $ "Success!"
