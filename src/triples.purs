module Perspectives.Triples where

import Perspectives.ObjectCollection
import Control.Monad.Eff.Class (liftEff)
import Data.Argonaut (Json, toBoolean, toNumber, toString)
import Data.Array (cons, foldr)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Perspectives.Property (AsyncPropDefsM, PropertyName, getGetter)
import Perspectives.Resource (representResource')
import Perspectives.ResourceTypes (Resource(..))
import Perspectives.TripleAdministration (NamedFunction(..), ResourceIndex, Triple(..), TripleGetter, addTriple, booleanIndex, constructTripleGetter, constructTripleGetter1, lookup, lookup1, numberIndex, numbersIndex, resourceIndex, resourcesIndex, stringIndex, stringsIndex)
import Prelude (bind, id, pure, ($))

type NamedSingleTripleGetter a = forall e. NamedFunction (TripleGetter e (Maybe a))

type NamedPluralTripleGetter a = forall e. NamedFunction (TripleGetter e (Array a))

getString :: PropertyName -> NamedSingleTripleGetter String
getString name = constructTripleGetter1 toString name

-- | in AsyncDomeinFile, retrieve either an Array of Strings or an error message.
getStrings :: PropertyName -> NamedPluralTripleGetter String
getStrings name = constructTripleGetter1 toString name

-- | in AsyncDomeinFile, retrieve either a Number or an error message.
getNumber :: PropertyName -> NamedSingleTripleGetter Number
getNumber name = constructTripleGetter1 toNumber name

-- | in AsyncDomeinFile, retrieve either an Array of Numbers or an error message.
getNumbers :: PropertyName -> NamedPluralTripleGetter Number
getNumbers name = constructTripleGetter1 toNumber name

-- | in AsyncDomeinFile, retrieve either a Boolean value or an error message.
getBoolean :: PropertyName -> NamedSingleTripleGetter Boolean
getBoolean name = constructTripleGetter1 toBoolean name

-- | in AsyncDomeinFile, retrieve either a Resource or an error message. This sets up the property definitions.
getResource :: PropertyName -> NamedSingleTripleGetter Resource
getResource pn = NamedFunction pn tripleGetter where
  tripleGetter :: forall e. TripleGetter e (Maybe Resource)
  tripleGetter res@(Just (Resource{id})) = do
    t@(Triple{object}) <- liftEff (lookup resourceIndex id pn)
    case isEmpty object of
      true -> do
        (maybeId :: Maybe String) <- getGetter toString pn res
        case maybeId of
          Nothing -> liftEff $ addTriple resourceIndex id pn Nothing [] []
          (Just id') -> do
            res' <- liftEff $ representResource' id'
            liftEff $ addTriple resourceIndex id' pn res' [] []
      false -> pure t
  tripleGetter Nothing = pure (Triple{ subject: ""
          , predicate: pn
          , object: empty
          , supports: []
          , dependencies: []
          })

-- | in AsyncDomeinFile, retrieve either a Resource or an error message. This sets up the property definitions.
getResources :: PropertyName -> NamedPluralTripleGetter Resource
getResources pn = NamedFunction pn tripleGetter where
  tripleGetter :: forall e. TripleGetter e (Array Resource)
  tripleGetter res@(Just (Resource{id : rid})) = do
    t@(Triple{object}) <- liftEff (lookup resourcesIndex rid pn)
    case isEmpty object of
      true -> do
        (resIdArray :: Array String) <- getGetter toString pn res
        (x :: Array (Maybe Resource)) <- liftEff $ (traverse representResource' resIdArray)
        (resources :: Array Resource) <- pure $ foldr (maybe id cons) [] x
        liftEff $ addTriple resourcesIndex rid pn resources [] []
      false -> pure t
  tripleGetter Nothing = pure (Triple{ subject: ""
          , predicate: pn
          , object: empty
          , supports: []
          , dependencies: []
          })
