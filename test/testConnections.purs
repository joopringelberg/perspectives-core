module Test.Connections where

import Prelude
import Perspectives.PropertyComposition
import Perspectives.SystemQueries
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (log) as Aff
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Perspectives.Location (Location, saveInLocation, locationValue)
import Perspectives.LocationT (runLocationT)
import Perspectives.Property (MemorizingSingleGetter, SingleGetter)
import Perspectives.QueryCombinators (identity)
import Perspectives.QueryCombinators (filter) as QC
import Perspectives.Resource (locationFromMaybeResource, representResource)
import Perspectives.ResourceTypes (Resource)

test = launchAff $ runLocationT do
  -- (gb :: Maybe Resource) <- liftEff $ representResource "user:xGebruiker"
  -- (gebruikerType :: Maybe Resource) <- rdfType gb
  -- (labelGebruikerType :: Maybe String) <- label gebruikerType
  -- (identifierGebruikerType :: Maybe String) <- identifier gebruikerType
  -- log $ show gb <> ", " <> show gebruikerType <> ", " <> show labelGebruikerType <> ", " <> show identifierGebruikerType
  -- gtype <- liftEff $ representResource "model:SysteemDomein#Gebruiker"
  -- _ <- pure $ catchit gtype
  -- pure gb

  -- (entiteit :: Maybe Resource) <- liftEff $ representResource "model:SysteemDomein#Entiteit"
  -- entiteitSuperClasses <- (superClasses >>-> label) entiteit
  -- log (show entiteitSuperClasses)
  -- _ <- pure $ catchit entiteit
  -- pure entiteit

  -- (entiteit :: Maybe Resource) <- liftEff $ representResource "model:SysteemDomein#Entiteit"
  -- entiteitTypes <- (types >>-> label) entiteit
  -- _ <- pure $ catchit entiteit
  -- entiteitTypes2 <- (types >>-> label) entiteit
  -- log (show entiteitTypes)
  -- _ <- pure $ catchit entiteit
  -- pure entiteit

  -- (gebruiker :: Maybe Resource) <- liftEff $ representResource "model:SysteemDomein#Gebruiker"
  -- x <- types >>-> label $ gebruiker
  -- x' <- subClassOf >>->> types >>-> label $ gebruiker
  -- _ <- pure $ catchit gebruiker
  -- y <- types >>-> identifier $ gebruiker
  -- y' <- subClassOf >>->> types >>-> identifier $ gebruiker
  -- _ <- pure $ catchit gebruiker
  -- log $ show x
  -- log $ show y

  -- (gebruiker :: Maybe Resource) <- liftEff $ representResource "model:SysteemDomein#Gebruiker"
  -- x <- identity >->> types >>-> label $ gebruiker
  -- _ <- pure $ catchit gebruiker
  -- log $ show x

  -- (gebruiker :: Maybe Resource) <- liftEff $ representResource "model:SysteemDomein#Gebruiker"
  -- x' <- identity >->> (QC.filter hasLabel "onlyWithLabel" types) >>-> identifier $ gebruiker
  -- log $ show x'
  -- _ <- pure $ catchit gebruiker
  -- x <- identity >->> types >>-> identifier $ gebruiker
  -- log $ show x
  -- _ <- pure $ catchit gebruiker
  -- z <- identity >->> (QC.filter hasLabel "onlyWithLabel" types) >>-> identifier $ gebruiker
  -- log $ show z
  -- log "Klaar"

  (gebruiker :: Maybe Resource) <- liftEff $ representResource "owl:Thing"
  x <- identity >-> rdfType >-> identifier $ gebruiker
  log $ show x
  _ <- pure $ catchit gebruiker
  log "Klaar"

log = lift <<< Aff.log

catchit :: forall a. a -> a
catchit mr = mr
