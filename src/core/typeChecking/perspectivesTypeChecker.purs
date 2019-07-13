module Perspectives.Checking.PerspectivesTypeChecker where

import Control.Monad.Except (ExceptT, catchError, lift, throwError)
import Data.Array (cons, elemIndex)
import Data.Foldable (for_)
import Data.List.NonEmpty (singleton)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Effect.Exception (error)
import Foreign.Object (values)
import Perspectives.CoreTypes (MonadPerspectives, MP)
import Perspectives.InstanceRepresentation (PerspectContext, pspType)
import Perspectives.Instances (getPerspectEntiteit)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.CalculatedRole (kindOfRole) as CR
import Perspectives.Representation.Class.Persistent (ContextType, getPerspectType, identifier)
import Perspectives.Representation.Context (Context, contextAspects, defaultPrototype, roleInContext)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.EnumeratedRole (kindOfRole) as ER
import Perspectives.Representation.TypeIdentifiers (RoleKind(..), RoleType(..))
import Prelude (class Eq, class Show, Unit, bind, discard, pure, show, unit, ($), (<<<), (<>), (==), (>=>), (>>=))

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
    \e -> fail $ CyclicAspects (identifier c)

  -- 3. The RoleKind of each RoleType must equal the position of the RoleType in the context.
  -- E.g.: all EnumeratedRoles and CalculatedRoles in rolInContext must have RoleKind RolInContext.
  for_
    (values $ roleInContext c)
    \(r :: RoleType) -> do
      k <- lift $ rolekind r
      if (k == RoleInContext)
        then pure unit
        else fail $ WrongRoleKind r RoleInContext k

  where
    throwOnCycle :: Array ContextType -> Context -> MP Unit
    throwOnCycle path next = if (isJust $ elemIndex (identifier next) path)
      then (throwError (error "cyclic"))
      else for_
        (contextAspects next)
        (getPerspectType >=> throwOnCycle (cons (identifier c) path))

    rolekind :: RoleType -> MP RoleKind
    rolekind (ENR r) = (getPerspectType r :: MP EnumeratedRole) >>= pure <<< ER.kindOfRole
    rolekind (CR r) = (getPerspectType r :: MP CalculatedRole) >>= pure <<< CR.kindOfRole

-----------------------------------------------------------
-- COLLECTING ERRORS DURING TYPE CHECKING
-----------------------------------------------------------
-- | A type for Perspectives errors
data PerspectivesError
  = DefaultPrototype ContextType ContextType
    | CyclicAspects ContextType
    | WrongRoleKind RoleType RoleKind RoleKind

derive instance eqPerspectivesError :: Eq PerspectivesError

instance showPerspectivesError :: Show PerspectivesError where
  show (DefaultPrototype expected given) = "Invalid type for DefaultPrototype. Expected: '" <> unwrap expected <> "' but found '" <> unwrap given <> "'."
  show (CyclicAspects c) = "Context '" <> unwrap c <> "' has cyclic aspects."
  show (WrongRoleKind roletype expected found) = "Role '" <> show roletype <> "' has kind '" <> show found <> "' but should have kind '" <> show expected<> "'."

-- | A type for accumulating multiple `PerspectivesErrors`s.
type MultipleErrors = NonEmptyList PerspectivesError

-- | An error monad, used in this library to encode possible failures when
-- | checking a Perspectives model data.
-- |
-- | The `Alt` instance for `Except` allows us to accumulate errors,
-- | unlike `Either`, which preserves only the last error.
type PF = ExceptT MultipleErrors MonadPerspectives

-- | Throws a failure error in `F`.
fail :: forall a. PerspectivesError -> PF a
fail = throwError <<< singleton
