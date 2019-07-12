module Perspectives.Representation.Class.EnumReadForeign where

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), to)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign (Foreign, F, ForeignError(..))
import Prelude ((<<<), (==), (<$>), bind, ($), pure, (<>))
import Simple.JSON (readImpl)

enumReadForeign :: forall a rep
   . Generic a rep
  => EnumReadForeign rep
  => Foreign
  -> F a
enumReadForeign f =
  to <$> enumReadForeignImpl f

-- type class for "enums", or nullary sum types
class EnumReadForeign rep where
  enumReadForeignImpl :: Foreign -> F rep

instance sumEnumReadForeign ::
  ( EnumReadForeign a
  , EnumReadForeign b
  ) => EnumReadForeign (Sum a b) where
  enumReadForeignImpl f
      = Inl <$> enumReadForeignImpl f
    <|> Inr <$> enumReadForeignImpl f

instance constructorEnumReadForeign ::
  ( IsSymbol name
  ) => EnumReadForeign (Constructor name NoArguments) where
  enumReadForeignImpl f = do
    s <- readImpl f
    if s == name
       then pure $ Constructor NoArguments
       else throwError <<< pure <<< ForeignError $
            "Enum string " <> s <> " did not match expected string " <> name
    where
      name = reflectSymbol (SProxy :: SProxy name)
