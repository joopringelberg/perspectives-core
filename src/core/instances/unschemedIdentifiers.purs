module Perspectives.UnschemedIdentifiers
  ( UnschemedResourceIdentifier(..)
  , unschemeContextInstance
  , unschemePerspectivesUser
  , unschemeRoleInstance
  )
  where

import Prelude

import Data.Array.NonEmpty (index)
import Data.Maybe (Maybe(..)) 
import Data.Newtype (class Newtype, over)
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), PerspectivesUser(..), RoleInstance(..))
import Simple.JSON (class ReadForeign, class WriteForeign)

-----------------------------------------------------------
-- UNSCHEMED RESOURCE IDENTIFIER
-----------------------------------------------------------
-- | A type that represents resource identifiers without a storage scheme.
newtype UnschemedResourceIdentifier = UnschemedResourceIdentifier String
instance Show UnschemedResourceIdentifier where show (UnschemedResourceIdentifier i) = i
derive instance Newtype UnschemedResourceIdentifier _
instance Eq UnschemedResourceIdentifier where eq (UnschemedResourceIdentifier s1) (UnschemedResourceIdentifier s2) = eq s1 s2
instance Ord UnschemedResourceIdentifier where compare (UnschemedResourceIdentifier s1) (UnschemedResourceIdentifier s2) = compare s1 s2
derive newtype instance WriteForeign UnschemedResourceIdentifier
derive newtype instance ReadForeign UnschemedResourceIdentifier

unschemeRoleInstance :: RoleInstance -> UnschemedResourceIdentifier
unschemeRoleInstance r = over RoleInstance takeGuid r

unschemeContextInstance :: ContextInstance -> UnschemedResourceIdentifier
unschemeContextInstance r = over ContextInstance takeGuid r

unschemePerspectivesUser :: PerspectivesUser -> UnschemedResourceIdentifier
unschemePerspectivesUser r = over PerspectivesUser takeGuid r

-- | Captures everything following the "#" as its first and only capturing group.
discardStorageRegex :: Regex
discardStorageRegex = unsafeRegex "^[^#$]+#(.+)" noFlags

-- | Just retains the (unique) identifier of the ResourceIdentifier; 
-- | discards all storage information such as scheme, database name or url.
takeGuid :: String -> String
takeGuid s = case match discardStorageRegex s of
  Nothing -> s
  Just matches -> case index matches 1 of
    Just (Just g) -> g
    _ -> s
