module Perspectives.Representation.Resource where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe, fromJust, isJust)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, case_, inj, on)
import Partial.Unsafe (unsafePartial)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)

_context = SProxy :: SProxy "context"
_role = SProxy :: SProxy "role"
_value = SProxy :: SProxy "value"
_contexts = SProxy :: SProxy "contexts"
_roles = SProxy :: SProxy "roles"
_values = SProxy :: SProxy "values"

type Resource = Variant
  ( context :: ContextInstance
  , role :: RoleInstance
  , contexts :: (Array ContextInstance)
  , roles :: (Array RoleInstance)
  , values :: (Array Value))

type Subject = Variant
  ( context :: ContextInstance
  , role :: RoleInstance
  )

type Objects = Variant
  ( contexts :: (Array ContextInstance)
  , roles :: (Array RoleInstance)
  , values :: (Array Value)
  )

type Object = Variant
  ( context :: ContextInstance
  , role :: RoleInstance
  , value :: Value
  )

type MaybeObject = Variant
  ( context :: Maybe ContextInstance
  , role :: Maybe RoleInstance
  , value :: Maybe Value
  )

firstObject :: Objects -> MaybeObject
firstObject = case_
    # on _contexts (\cs -> inj _context (head cs))
    # on _roles (\rs -> inj _role (head rs))
    # on _values (\vs -> inj _value (head vs))

noObject :: MaybeObject -> Boolean
noObject = case_
  # on _context isJust
  # on _role isJust
  # on _value isJust

justObject :: MaybeObject -> Object
justObject = case_
  # on _context ((inj _context) <<< unsafePartial fromJust)
  # on _role ((inj _role) <<< unsafePartial fromJust)
  # on _value ((inj _value <<< unsafePartial fromJust))

type CSubject s = Variant (context :: ContextInstance | s)
type RSubject s = Variant (context :: RoleInstance | s)
type CObjects o = Variant (contexts :: Array ContextInstance | o)
type RObjects o = Variant (contexts :: Array RoleInstance | o)
type VObjects o = Variant (contexts :: Array Value | o)

subjectString :: Subject -> String
subjectString = case_
  # on _context unwrap
  # on _role unwrap
