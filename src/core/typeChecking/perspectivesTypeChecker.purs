module Perspectives.Checking.PerspectivesTypeChecker where

import Control.Monad.Except (catchError, lift, throwError)
import Data.Array (cons, elemIndex)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives, MP)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.InstanceRepresentation (PerspectContext, pspType)
import Perspectives.Instances (getPerspectEntiteit)
import Perspectives.Instances.ObjectGetters (roleType_)
import Perspectives.Parsing.Messages (PerspectivesError(..), PF, fail)
import Perspectives.Representation.ADT (lessThenOrEqualTo)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.PersistentType (ContextType, EnumeratedRoleType, getPerspectType)
import Perspectives.Representation.Class.Role (rangeOfRoleCalculation, expansionOfADT, kindOfRole)
import Perspectives.Representation.Context (Context, contextAspects, contextRole, defaultPrototype, externalRole, roleInContext, userRole, position)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.Representation.TypeIdentifiers (RoleKind(..), RoleType(..))
import Prelude (Unit, bind, discard, pure, unit, ($), (<<<), (==), (>=>), (>>=))

checkDomeinFile :: DomeinFile -> MonadPerspectives (Array PerspectivesError)
checkDomeinFile df = pure []

checkDomeinFile_ :: DomeinFile -> PF Unit
checkDomeinFile_ df = pure unit

checkContext :: Context -> PF Unit
checkContext c = do

  -- 1. The default prototype should have the type that we are checking.
  case defaultPrototype c of
    Nothing -> pure unit
    (Just pt) -> do
      (p :: PerspectContext) <- lift $ getPerspectEntiteit pt
      if (pspType p == identifier c)
        then pure unit
        else fail $ DefaultPrototype (identifier c :: ContextType) (pspType p)

  -- 2. The graph formed by the contextAspects may not be cyclic.
  -- I.e. when traversing the graph, the next node to be visited may not be in the path
  -- starting with c.
  catchError (lift $ throwOnCycle [] c)
    \e -> fail $ CyclicAspects (position c) (identifier c)

  -- 3. The RoleKind of each RoleType must equal the position of the RoleType in the context.
  -- E.g.: all EnumeratedRoles and CalculatedRoles in rolInContext must have RoleKind RolInContext.
  for_ (roleInContext c) (checkRoleKind RoleInContext)
  for_ (contextRole c) (checkRoleKind ContextRole)
  for_ (userRole c) (checkEnumeratedRole UserRole)
  checkEnumeratedRole ExternalRole (externalRole c)

  where
    checkRoleKind :: RoleKind -> RoleType -> PF Unit
    checkRoleKind kind (r :: RoleType) = do
      k <- lift $ rolekind r
      if (k == kind)
        then pure unit
        else fail $ WrongRoleKind r kind k

    checkEnumeratedRole :: RoleKind -> EnumeratedRoleType -> PF Unit
    checkEnumeratedRole kind r = do
      (rr :: EnumeratedRole) <- lift $ getPerspectType r
      if (kindOfRole rr == kind)
        then pure unit
        else fail $ WrongRoleKind (ENR r) kind (kindOfRole rr)

    throwOnCycle :: Array ContextType -> Context -> MP Unit
    throwOnCycle path next = if (isJust $ elemIndex (identifier next) path)
      then (throwError (error "cyclic"))
      else for_
        (contextAspects next)
        (getPerspectType >=> throwOnCycle (cons (identifier c) path))

    rolekind :: RoleType -> MP RoleKind
    rolekind (ENR r) = (getPerspectType r :: MP EnumeratedRole) >>= pure <<< kindOfRole
    rolekind (CR r) = (getPerspectType r :: MP CalculatedRole) >>= pure <<< kindOfRole

-----------------------------------------------------------
-- CHECKBINDING
-----------------------------------------------------------
checkBinding :: RoleType -> RoleInstance -> MP Boolean
checkBinding roletype instanceToBind = do
  eit <- (roleType_ >=> rangeOfRoleCalculation <<< ENR >=> expansionOfADT) instanceToBind
  ert <- (rangeOfRoleCalculation >=> expansionOfADT) roletype
  pure $ lessThenOrEqualTo ert eit
