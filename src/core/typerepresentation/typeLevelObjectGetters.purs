module Perspectives.Types.ObjectGetters where

-- | If a role with the given qualified name is available, return it as a RoleType. From the type we can find out its RoleKind, too.
import Data.Array (singleton)
import Data.Maybe (Maybe)
import Perspectives.CoreTypes (MonadPerspectives, MP, type (~~~>))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.Identifiers (isContainingNamespace)
import Perspectives.Instances.Combinators (closure_, filter')
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.PersistentType (getPerspectType, getPerspectTypes')
import Perspectives.Representation.Class.Role (class RoleClass, binding, properties, roleAspects)
import Perspectives.Representation.Context (Context, aspects, roleInContext) as Context
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType, PropertyType, RoleType, propertytype2string, roletype2string)
import Prelude (pure, (==), (>>>), (<<<), (>=>), map)

-- | If a role with the given qualified name is available, return it as a RoleType. From the type we can find out its RoleKind, too.
lookForRoleType :: String -> (ContextType ~~~> RoleType)
lookForRoleType s = lookForRole (roletype2string >>> ((==) s))

-- | We simply require the Pattern to match the end of the string.
lookForUnqualifiedRoleType :: String -> ContextType ~~~> RoleType
lookForUnqualifiedRoleType s = lookForRole (roletype2string >>> isContainingNamespace s)

lookForRole :: (RoleType -> Boolean) -> ContextType ~~~> RoleType
lookForRole criterium = filter' (contextAspectsClosure >=> roleInContext) criterium

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
lookForProperty criterium = filter' (roleAspectsClosure >=> propertyOfRole) criterium

propertyOfRole :: forall r i. RoleClass r i => i ~~~> PropertyType
propertyOfRole = ArrayT <<< ((getPerspectType :: i -> MonadPerspectives r) >=> properties)

roleAspectsClosure :: ADT EnumeratedRoleType ~~~> EnumeratedRoleType
roleAspectsClosure = g >=> closure_ f where
  f :: EnumeratedRoleType -> ArrayT MonadPerspectives EnumeratedRoleType
  f = ArrayT <<< ((getPerspectType :: EnumeratedRoleType -> MonadPerspectives EnumeratedRole) >=> roleAspects)

  g :: ADT EnumeratedRoleType ~~~> EnumeratedRoleType
  g = ((getPerspectTypes' :: ADT EnumeratedRoleType ~~~> EnumeratedRole) >=> ArrayT <<< roleAspects)

-- alleen gebruikt in DescriptionCompiler. Probeer te vervangen door fullType.
typeOfBinding :: ADT EnumeratedRoleType ~~~> Maybe (ADT EnumeratedRoleType)
typeOfBinding = (getPerspectTypes' :: ADT EnumeratedRoleType ~~~> EnumeratedRole) >=> ArrayT <<< map singleton <<< binding

-- wordt niet gebruikt.
-- typeOfBinding_ :: EnumeratedRoleType ~~~> (ADT EnumeratedRoleType)
-- typeOfBinding_ = ArrayT <<< ((getPerspectType :: EnumeratedRoleType -> MP EnumeratedRole) >=> binding >=> pure <<< singleton)
