module Perspectives.Parsing.Arc.AST.ReplaceIdentifiers  where

import Prelude

import Perspectives.Parsing.Arc.AST (ActionE(..), AutomaticEffectE(..), ContextActionE(..), NotificationE(..), PropertyVerbE(..), RoleE(..), RoleIdentification(..), RolePart(..), RoleVerbE(..), SelfOnly(..), StateE(..), StateQualifiedPart(..), StateSpecification(..), StateTransitionE(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), EnumeratedRoleType(..), RoleType(..))

class ReplaceIdentifiers a where
  replaceIdentifier :: String -> a -> a

instance ReplaceIdentifiers RoleE where 
  replaceIdentifier newId (RoleE r@({id,roleParts})) = RoleE (r 
    { id = newId
    , roleParts = replaceIdentifier newId <$> roleParts
    })

instance ReplaceIdentifiers RolePart where
  replaceIdentifier newId (SQP parts) = SQP $ replaceIdentifier newId <$> parts
  replaceIdentifier newId (ROLESTATE s) = ROLESTATE $ replaceIdentifier newId s
  replaceIdentifier newId rp = rp

instance ReplaceIdentifiers StateQualifiedPart where
  replaceIdentifier newId (R r) = R $ replaceIdentifier newId r
  replaceIdentifier newId (P r) = P $ replaceIdentifier newId r
  replaceIdentifier newId (AC r) = AC $ replaceIdentifier newId r
  replaceIdentifier newId (CA r) = CA $ replaceIdentifier newId r
  replaceIdentifier newId (SO r) = SO $ replaceIdentifier newId r
  replaceIdentifier newId (N r) = N $ replaceIdentifier newId r
  replaceIdentifier newId (AE r) = AE $ replaceIdentifier newId r
  replaceIdentifier newId (SUBSTATE r) = SUBSTATE $ replaceIdentifier newId r

instance ReplaceIdentifiers StateE where
  replaceIdentifier newId (StateE r@{id, stateParts, subStates}) = StateE $ r 
    { id = replaceIdentifier newId id
    , stateParts = replaceIdentifier newId <$> stateParts
    , subStates = replaceIdentifier newId <$> subStates
    }

instance ReplaceIdentifiers StateSpecification where
  replaceIdentifier newId (SubjectState rid mp) = SubjectState (replaceIdentifier newId rid) mp
  replaceIdentifier newId ss = ss

instance ReplaceIdentifiers RoleIdentification where
  replaceIdentifier newId (ExplicitRole ct rt pos) = ExplicitRole ct (replaceIdentifier newId rt) pos
  replaceIdentifier newId rid = rid

instance ReplaceIdentifiers RoleType where
  replaceIdentifier newId r@(ENR _) = r
  replaceIdentifier newId (CR (CalculatedRoleType id)) = ENR $ EnumeratedRoleType newId

instance ReplaceIdentifiers RoleVerbE where
  replaceIdentifier newId (RoleVerbE r@{subject, state}) = RoleVerbE $ r
    { subject = replaceIdentifier newId subject
    , state = replaceIdentifier newId state}

instance ReplaceIdentifiers PropertyVerbE where
  replaceIdentifier newId (PropertyVerbE r@{subject, state}) = PropertyVerbE $ r
    { subject = replaceIdentifier newId subject
    , state = replaceIdentifier newId state}

instance ReplaceIdentifiers ContextActionE where
  replaceIdentifier newId (ContextActionE r@{subject, state}) = ContextActionE $ r
    { subject = replaceIdentifier newId subject
    , state = replaceIdentifier newId state}

instance ReplaceIdentifiers SelfOnly where
  replaceIdentifier newId (SelfOnly r@{subject, state}) = SelfOnly $ r
    { subject = replaceIdentifier newId subject
    , state = replaceIdentifier newId state}

instance ReplaceIdentifiers ActionE where
  replaceIdentifier newId (ActionE r@{subject, state}) = ActionE $ r
    { subject = replaceIdentifier newId subject
    , state = replaceIdentifier newId state}

instance ReplaceIdentifiers NotificationE where
  replaceIdentifier newId (NotificationE r@{user, transition}) = NotificationE $ r
    { user = replaceIdentifier newId user
    , transition = replaceIdentifier newId transition}

instance ReplaceIdentifiers StateTransitionE where
  replaceIdentifier newId (Entry spec) = Entry $ replaceIdentifier newId spec
  replaceIdentifier newId (Exit spec) = Exit $ replaceIdentifier newId spec

instance ReplaceIdentifiers AutomaticEffectE where
  replaceIdentifier newId (AutomaticEffectE r@{subject, transition}) = AutomaticEffectE $ r
    { subject = replaceIdentifier newId subject
    , transition = replaceIdentifier newId transition}
