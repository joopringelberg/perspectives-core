module Perspectives.PerspectEntiteit where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVar, isEmptyVar, makeEmptyVar, putVar, readVar)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (throwError, runExcept)
import Data.Either (Either)
import Data.Foreign (MultipleErrors)
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Generic (decodeJSON, encodeJSON)
import Data.Maybe (Maybe(..))
import Perspectives.ContextAndRole (changeContext_type, changeRol_type, context_id, context_pspType, context_rev', rol_id, rol_pspType, rol_rev')
import Perspectives.DomeinCache (retrieveContextFromDomein, retrieveRolFromDomein)
import Perspectives.Effects (AvarCache, AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.EntiteitCache (contextDefinitions, rolDefinitions)
import Perspectives.GlobalUnsafeStrMap (poke, peek)
import Perspectives.Identifiers (Namespace)
import Perspectives.Syntax (PerspectContext(..), PerspectRol(..), Revision, revision)
import Prelude (Unit, bind, discard, pure, unit, ($), (*>), (<<<), (<>))

class (Encode a, Decode a) <=  PerspectEntiteit a where
  getRevision :: a -> Revision
  setRevision :: String -> a -> a
  getType :: a -> ID
  setType :: ID -> a -> a
  getId :: a -> ID
  -- | Create an empty AVar that will be filled by the PerspectEntiteit.
  representInternally :: forall e. ID -> Aff (AvarCache e) (AVar a)
  retrieveInternally :: forall e. ID -> Aff (AvarCache e) (Maybe (AVar a))
  -- | A default implementation for encode is encodeJSON.
  encode :: a -> String
  -- | A default implementation for decode is decodeJSON.
  decode :: String -> Either MultipleErrors a
  retrieveFromDomein :: forall e. ID -> Namespace -> Aff (AjaxAvarCache e) a

instance perspectEntiteitContext :: PerspectEntiteit PerspectContext where
  getRevision = context_rev'
  setRevision r (PerspectContext c) = PerspectContext c {_rev = revision r}
  getType = context_pspType
  setType = changeContext_type
  getId = context_id
  representInternally c = do
    av <- makeEmptyVar
    _ <- liftEff $ poke contextDefinitions c av
    pure av
  retrieveInternally id = liftEff $ peek contextDefinitions id
  encode = encodeJSON
  decode = runExcept <<< decodeJSON
  retrieveFromDomein = retrieveContextFromDomein

instance perspectEntiteitRol :: PerspectEntiteit PerspectRol where
  getRevision = rol_rev'
  setRevision r (PerspectRol rp) = PerspectRol rp {_rev = revision r}
  getType = rol_pspType
  setType = changeRol_type
  getId = rol_id
  representInternally c = do
    av <- makeEmptyVar
    _ <- liftEff $ poke rolDefinitions c av
    pure av
  retrieveInternally id = liftEff $ peek rolDefinitions id
  encode = encodeJSON
  decode = runExcept <<< decodeJSON
  retrieveFromDomein = retrieveRolFromDomein

-- | Store an internally created PerspectEntiteit for the first time in the local store.
cacheEntiteit :: forall e a. PerspectEntiteit a => ID -> a -> Aff (AvarCache e) Unit
cacheEntiteit id e = do
  (av :: AVar a) <- representInternally id
  putVar e av
  pure unit

-- | Modify a PerspectEntiteit in the cache.
cacheEntiteitAgain :: forall e a. PerspectEntiteit a => ID -> a -> Aff (AvarCache e) a
cacheEntiteitAgain id e = do
  mAvar <- retrieveInternally id
  case mAvar of
    Nothing -> throwError $ error $ "cacheEntiteitAgain: cannot change an entiteit that is not cached: " <> id
    (Just avar) -> putVar e avar *> pure e

-- | Returns an entity. Throws an error if the resource is not represented in cache or not
-- | immediately available in cache.
readEntiteitFromCache :: forall e a. PerspectEntiteit a => ID -> Aff (AvarCache e) a
readEntiteitFromCache id = do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> throwError $ error ("getEntiteitFromCache needs a locally stored resource for " <> id)
    (Just avar) -> do
      empty <- isEmptyVar avar
      if empty
        then throwError $ error ("getEntiteitFromCache found an empty AVar for " <> id)
        else readVar avar
