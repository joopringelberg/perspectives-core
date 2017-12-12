module Test.PrettyPrinter where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (makeVar)
import Control.Monad.Eff.Class (liftEff)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (lookup)
import Perspectives.ContextRoleParser (context) as CRP
import Perspectives.GlobalUnsafeStrMap (poke)
import Perspectives.IndentParser (runIndentParser)
import Perspectives.PrettyPrinter (context, prettyPrint, strMapTraverse_)
import Perspectives.Resource (PROPDEFS, resourceDefinitions, unwrapPerspectContext, unwrapPerspectRol)
import Perspectives.ResourceTypes (DomeinFileEffects)
import Perspectives.Syntax (EntityCollection(..), NamedEntityCollection(..), PerspectEntity(..))
import Prelude (Unit, bind, pure, unit, ($))

type TestEffects e = (DomeinFileEffects (prd :: PROPDEFS | e))

test :: forall e. Aff (DomeinFileEffects (prd :: PROPDEFS | e)) String
test = case runIndentParser "--Commentaar voor :A1\n:Aangifte :A1\n\tpublic :urgentie = 1" CRP.context of
  (Right (NamedEntityCollection ident (EntityCollection j))) -> do
    _ <- strMapTraverse_ storePerspectEntityInResourceDefinitions j
    case lookup ident j of
      Nothing -> pure "onbekende fout"
      (Just (Context c)) -> prettyPrint c (context [])
      (Just (Rol _)) -> pure "er is geen rol"
  otherwise -> pure "fout in expressie"

storePerspectEntityInResourceDefinitions :: forall e. String -> PerspectEntity -> Aff (DomeinFileEffects e) Unit
storePerspectEntityInResourceDefinitions key (Context c) = do
  av <- makeVar (unwrapPerspectContext c)
  _ <- liftEff $ poke resourceDefinitions key av
  pure unit
storePerspectEntityInResourceDefinitions key (Rol r) = do
  av <- makeVar (unwrapPerspectRol r)
  _ <- liftEff $ poke resourceDefinitions key av
  pure unit

-- traverse_
--   :: forall a b f m
--    . Applicative m
--   => Foldable f
--   => (a -> m b)
--   -> f a
--   -> m Unit
-- traverse_ f = foldr ((*>) <<< f) (pure unit)
--
-- instance foldableStrMap :: Foldable StrMap where
--   foldl f = fold (\z _ -> f z)
--   foldr f z m = foldr f z (values m)  -- values m :: Array a
--   foldMap f = foldMap (const f)
--
-- -- | Get a list of the values in a map
-- values :: forall a. StrMap a -> Array a
-- values = toArrayWithKey (\_ v -> v)
--
-- -- | Fold the keys and values of a map
-- fold :: forall a z. (z -> String -> a -> z) -> z -> StrMap a -> z
-- fold = _foldM ((#))
