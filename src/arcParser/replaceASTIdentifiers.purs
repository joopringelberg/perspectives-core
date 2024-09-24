module Perspectives.Parsing.Arc.AST.ReplaceIdentifiers  where

import Prelude

import Data.Maybe (Maybe(..))
import Perspectives.Identifiers (endsWithSegments)
import Perspectives.Parsing.Arc.AST (ActionE(..), AutomaticEffectE(..), ContextActionE(..), NotificationE(..), AuthorOnly(..), PropertyVerbE(..), RoleE(..), RoleIdentification(..), RolePart(..), RoleVerbE(..), SelfOnly(..), StateE(..), StateQualifiedPart(..), StateSpecification(..), StateTransitionE(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), EnumeratedRoleType(..), RoleType(..))

type OriginalId = String
type Addendum = String

class ReplaceIdentifiers a where
  replaceIdentifier :: OriginalId -> Addendum -> a -> a

instance ReplaceIdentifiers RoleE where 
  replaceIdentifier original addendum (RoleE r@({id,roleParts})) = RoleE (r 
    { id = if endsWithSegments id original then id <> addendum else original
    , roleParts = replaceIdentifier original addendum <$> roleParts
    })

instance ReplaceIdentifiers RolePart where
  replaceIdentifier original addendum (SQP parts) = SQP $ replaceIdentifier original addendum <$> parts
  replaceIdentifier original addendum (ROLESTATE s) = ROLESTATE $ replaceIdentifier original addendum s
  replaceIdentifier original addendum rp = rp

instance ReplaceIdentifiers StateQualifiedPart where
  replaceIdentifier original addendum (R r) = R $ replaceIdentifier original addendum r
  replaceIdentifier original addendum (P r) = P $ replaceIdentifier original addendum r
  replaceIdentifier original addendum (AC r) = AC $ replaceIdentifier original addendum r
  replaceIdentifier original addendum (CA r) = CA $ replaceIdentifier original addendum r
  replaceIdentifier original addendum (SO r) = SO $ replaceIdentifier original addendum r
  replaceIdentifier original addendum (PO r) = PO $ replaceIdentifier original addendum r
  replaceIdentifier original addendum (N r) = N $ replaceIdentifier original addendum r
  replaceIdentifier original addendum (AE r) = AE $ replaceIdentifier original addendum r
  replaceIdentifier original addendum (SUBSTATE r) = SUBSTATE $ replaceIdentifier original addendum r

instance ReplaceIdentifiers StateE where
  replaceIdentifier original addendum (StateE r@{id, stateParts, subStates}) = StateE $ r 
    { id = replaceIdentifier original addendum id
    , stateParts = replaceIdentifier original addendum <$> stateParts
    , subStates = replaceIdentifier original addendum <$> subStates
    }

instance ReplaceIdentifiers StateSpecification where
  replaceIdentifier original addendum (SubjectState rid mp) = SubjectState 
    (replaceIdentifier original addendum rid) 
    -- if the last segment equals the original, add the addendum
    (case mp of
      Nothing -> Nothing
      Just segments -> if endsWithSegments segments original
        then Just $ segments <> addendum
        else Just segments)
  replaceIdentifier original addendum ss = ss

instance ReplaceIdentifiers RoleIdentification where
  replaceIdentifier original addendum (ExplicitRole ct rt pos) = ExplicitRole ct (replaceIdentifier original addendum rt) pos
  replaceIdentifier original addendum rid = rid

instance ReplaceIdentifiers RoleType where
  replaceIdentifier original addendum r@(ENR (EnumeratedRoleType id)) = if endsWithSegments id original
    then ENR $ EnumeratedRoleType (id <> addendum)
    else r
  replaceIdentifier original addendum r@(CR (CalculatedRoleType id)) = if endsWithSegments id original
    then ENR $ EnumeratedRoleType (id <> addendum)
    else r

instance ReplaceIdentifiers RoleVerbE where
  replaceIdentifier original addendum (RoleVerbE r@{subject, state}) = RoleVerbE $ r
    { subject = replaceIdentifier original addendum subject
    , state = replaceIdentifier original addendum state}

instance ReplaceIdentifiers PropertyVerbE where
  replaceIdentifier original addendum (PropertyVerbE r@{subject, state}) = PropertyVerbE $ r
    { subject = replaceIdentifier original addendum subject
    , state = replaceIdentifier original addendum state}

instance ReplaceIdentifiers ContextActionE where
  replaceIdentifier original addendum (ContextActionE r@{subject, state}) = ContextActionE $ r
    { subject = replaceIdentifier original addendum subject
    , state = replaceIdentifier original addendum state}

instance ReplaceIdentifiers SelfOnly where
  replaceIdentifier original addendum (SelfOnly r@{subject, state}) = SelfOnly $ r
    { subject = replaceIdentifier original addendum subject
    , state = replaceIdentifier original addendum state}

instance ReplaceIdentifiers AuthorOnly where
  replaceIdentifier original addendum (AuthorOnly r@{subject, state}) = AuthorOnly $ r
    { subject = replaceIdentifier original addendum subject
    , state = replaceIdentifier original addendum state}

instance ReplaceIdentifiers ActionE where
  replaceIdentifier original addendum (ActionE r@{subject, state}) = ActionE $ r
    { subject = replaceIdentifier original addendum subject
    , state = replaceIdentifier original addendum state}

instance ReplaceIdentifiers NotificationE where
  replaceIdentifier original addendum (NotificationE r@{user, transition}) = NotificationE $ r
    { user = replaceIdentifier original addendum user
    , transition = replaceIdentifier original addendum transition}

instance ReplaceIdentifiers StateTransitionE where
  replaceIdentifier original addendum (Entry spec) = Entry $ replaceIdentifier original addendum spec
  replaceIdentifier original addendum (Exit spec) = Exit $ replaceIdentifier original addendum spec

instance ReplaceIdentifiers AutomaticEffectE where
  replaceIdentifier original addendum (AutomaticEffectE r@{subject, transition}) = AutomaticEffectE $ r
    { subject = replaceIdentifier original addendum subject
    , transition = replaceIdentifier original addendum transition}
