module Perspectives.Types.ObjectGetters where

import Control.Plus ((<|>))
import Data.Array (singleton)
import Data.Newtype (unwrap)
import Perspectives.CoreTypes (MonadPerspectives, type (~~~>), MP)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.Identifiers (isContainingNamespace)
import Perspectives.Instances.Combinators (closure_, filter')
import Perspectives.Representation.ADT (ADT(..), reduce)
import Perspectives.Representation.Class.PersistentType (getPerspectType, getContext)
import Perspectives.Representation.Class.Role (class RoleClass, getRole', properties, propertiesOfADT, views, viewsOfADT)
import Perspectives.Representation.Context (Context, aspects, roleInContext, externalRole) as Context
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType, EnumeratedRoleType(..), PropertyType, RoleType, ViewType(..), propertytype2string, roletype2string)
import Prelude (pure, (==), (>>>), (<<<), (>=>))

externalRoleOfADT_ :: ADT ContextType ~~~> EnumeratedRoleType
externalRoleOfADT_ = ArrayT <<< reduce eRole where
  eRole :: ContextType -> MP (Array EnumeratedRoleType)
  eRole = getContext >=> pure <<< singleton <<< Context.externalRole

externalRoleOfADT :: ADT ContextType -> MP (ADT EnumeratedRoleType)
externalRoleOfADT = reduce eRole where
  eRole :: ContextType -> MP (ADT EnumeratedRoleType)
  eRole = getContext >=> pure <<< ST <<< Context.externalRole

-- | If a role with the given qualified name is available, return it as a RoleType. From the type we can find out its RoleKind, too.
lookForRoleType :: String -> (ContextType ~~~> RoleType)
lookForRoleType s = lookForRole (roletype2string >>> ((==) s))

-- | We simply require the Pattern to match the end of the string.
lookForUnqualifiedRoleType :: String -> ContextType ~~~> RoleType
lookForUnqualifiedRoleType s = lookForRole (roletype2string >>> isContainingNamespace s)

lookForRole :: (RoleType -> Boolean) -> ContextType ~~~> RoleType
lookForRole criterium = filter' (contextAspectsClosure >=> roleInContext) criterium

lookForRoleTypeOfADT :: String -> (ADT ContextType ~~~> RoleType)
lookForRoleTypeOfADT s = lookForRoleOfADT (roletype2string >>> ((==) s))

-- | We simply require the Pattern to match the end of the string.
lookForUnqualifiedRoleTypeOfADT :: String -> ADT ContextType ~~~> RoleType
lookForUnqualifiedRoleTypeOfADT s = lookForRoleOfADT (roletype2string >>> isContainingNamespace s)

lookForRoleOfADT :: (RoleType -> Boolean) -> ADT ContextType ~~~> RoleType
lookForRoleOfADT criterium = ArrayT <<< reduce f
  where
    f :: ContextType -> MonadPerspectives (Array RoleType)
    f = unwrap <<< lookForRole criterium

roleInContext :: ContextType ~~~> RoleType
roleInContext = ArrayT <<< ((getPerspectType :: ContextType -> MonadPerspectives Context.Context) >=> pure <<< Context.roleInContext)

contextAspectsClosure :: ContextType ~~~> ContextType
contextAspectsClosure = closure_ f where
  f :: ContextType -> ArrayT MonadPerspectives ContextType
  f = ArrayT <<< ((getPerspectType :: ContextType -> MonadPerspectives Context.Context) >=> pure <<< Context.aspects)

lookForPropertyType_ :: String -> (EnumeratedRoleType ~~~> PropertyType)
lookForPropertyType_ s i = lookForProperty (propertytype2string >>> ((==) s)) (ST i)

-- | We simply require the Pattern to match the end of the string.
lookForUnqualifiedPropertyType_ :: String -> (EnumeratedRoleType ~~~> PropertyType)
lookForUnqualifiedPropertyType_ s i = lookForProperty (propertytype2string >>> isContainingNamespace s) (ST i)

lookForPropertyType :: String -> (ADT EnumeratedRoleType ~~~> PropertyType)
lookForPropertyType s = lookForProperty (propertytype2string >>> ((==) s))

-- | We simply require the Pattern to match the end of the string.
lookForUnqualifiedPropertyType :: String -> (ADT EnumeratedRoleType ~~~> PropertyType)
lookForUnqualifiedPropertyType s = lookForProperty (propertytype2string >>> isContainingNamespace s)

lookForProperty :: (PropertyType -> Boolean) -> ADT EnumeratedRoleType ~~~> PropertyType
lookForProperty criterium = filter' (ArrayT <<< propertiesOfADT) criterium

propertiesOfRole :: String ~~~> PropertyType
propertiesOfRole s = propertiesOfRole_ (EnumeratedRoleType s) <|> propertiesOfRole_ (CalculatedRoleType s)
  where
    propertiesOfRole_ :: forall r i. RoleClass r i => i ~~~> PropertyType
    propertiesOfRole_ = ArrayT <<< ((getPerspectType :: i -> MonadPerspectives r) >=> properties)

viewsOfRole :: String ~~~> ViewType
viewsOfRole s = f (EnumeratedRoleType s) <|> f (CalculatedRoleType s) where
  f :: forall i r. RoleClass r i => i ~~~> ViewType
  f = ArrayT <<< (getRole' >=> views)

lookForUnqualifiedViewType :: String -> (ADT EnumeratedRoleType ~~~> ViewType)
lookForUnqualifiedViewType s = lookForView (unwrap >>> isContainingNamespace s)

lookForView :: (ViewType -> Boolean) -> ADT EnumeratedRoleType ~~~> ViewType
lookForView criterium = filter' (ArrayT <<< viewsOfADT) criterium
