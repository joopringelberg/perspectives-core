module Perspectives.Triples where

import Control.Monad.Eff.Class (liftEff)
import Data.Argonaut (Json, toBoolean, toNumber, toString)
import Data.Array (cons, foldr)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Perspectives.Property (AsyncPropDefsM, PropertyName, getGetter)
import Perspectives.Resource (representResource')
import Perspectives.ResourceTypes (Resource(..))
import Perspectives.TripleAdministration (class PossiblyEmptyFunctor, ResourceIndex, Triple(..), addTriple, booleanIndex, empty, isEmpty, lookup, numberIndex, numbersIndex, resourceIndex, resourcesIndex, stringIndex, stringsIndex)
import Prelude (bind, id, pure, ($))

data NamedFunction f = NamedFunction String f

applyNamedFunction :: forall a b. NamedFunction (a -> b) -> a -> b
applyNamedFunction (NamedFunction _ f) a = f a

runTripleGetter :: forall a e. NamedFunction (TripleGetter e a) -> Maybe Resource -> AsyncPropDefsM e a
runTripleGetter (NamedFunction _ tg) mr = bind (tg mr) (\(Triple{object}) -> pure object)

type TripleGetter e a = Maybe Resource -> AsyncPropDefsM e (Triple a)

type NamedSingleTripleGetter a = forall e. NamedFunction (TripleGetter e (Maybe a))

type NamedPluralTripleGetter a = forall e. NamedFunction (TripleGetter e (Array a))

-- | Use this function to construct property getters that memorize in the triple administration.
constructTripleGetter :: forall a e ef. PossiblyEmptyFunctor ef => (Json -> Maybe a) -> ResourceIndex (ef a) -> PropertyName -> NamedFunction (TripleGetter e (ef a))
constructTripleGetter tofn tripleStore pn = NamedFunction pn tripleGetter where
  -- Here we interpret the empty string as the identification of Nothing??
  tripleGetter ::  TripleGetter e (ef a)
  tripleGetter res@(Just (Resource{id})) = do
    t@(Triple{object}) <- liftEff (lookup tripleStore id pn)
    case isEmpty object of
      true -> do
        (object' :: ef a) <- getGetter tofn pn res
        liftEff (addTriple tripleStore id pn object' [] [])
      false -> pure t
  tripleGetter Nothing = pure (Triple{ subject: ""
          , predicate: pn
          , object: empty
          , supports: []
          , dependencies: []
          })

getString :: PropertyName -> NamedSingleTripleGetter String
getString name = constructTripleGetter toString stringIndex name

-- | in AsyncDomeinFile, retrieve either an Array of Strings or an error message.
getStrings :: PropertyName -> NamedPluralTripleGetter String
getStrings name = constructTripleGetter toString stringsIndex name

-- | in AsyncDomeinFile, retrieve either a Number or an error message.
getNumber :: PropertyName -> NamedSingleTripleGetter Number
getNumber name = constructTripleGetter toNumber numberIndex name

-- | in AsyncDomeinFile, retrieve either an Array of Numbers or an error message.
getNumbers :: PropertyName -> NamedPluralTripleGetter Number
getNumbers name = constructTripleGetter toNumber numbersIndex name

-- | in AsyncDomeinFile, retrieve either a Boolean value or an error message.
getBoolean :: PropertyName -> NamedSingleTripleGetter Boolean
getBoolean name = constructTripleGetter toBoolean booleanIndex name

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
