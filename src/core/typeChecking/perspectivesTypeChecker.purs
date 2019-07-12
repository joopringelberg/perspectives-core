module Perspectives.Checking.PerspectivesTypeChecker where

import Control.Monad.Except (ExceptT, catchError, lift, throwError)
import Data.Array (cons, elemIndex)
import Data.Foldable (for_)
import Data.List.NonEmpty (singleton)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives, MP)
import Perspectives.InstanceRepresentation (PerspectContext, pspType)
import Perspectives.Instances (getPerspectEntiteit)
import Perspectives.Representation.Class.Persistent (ContextType, getPerspectType, identifier)
import Perspectives.Representation.Context (Context, contextAspects, defaultPrototype)
import Prelude (Unit, bind, pure, unit, (==), (<>), ($), class Eq, class Show, (<<<), (>=>), discard)

type Errors = Array String

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
  catchError (lift $ search [] c)
    \e -> fail $ CyclicAspects (identifier c)

  -- 3. The RoleKind of each RoleType must equal the position of the RoleType in the context.
  -- E.g.: all EnumeratedRoles and CalculatedRoles in rolInContext must have RoleKind RolInContext.

  where
    search :: Array ContextType -> Context -> MP Unit
    search path next = if (isJust $ elemIndex (identifier next) path)
      then (throwError (error "cyclic"))
      else for_
        (contextAspects next)
        (getPerspectType >=> search (cons (identifier c) path))

-----------------------------------------------------------
-- COLLECTING ERRORS DURING TYPE CHECKING
-----------------------------------------------------------
-- | A type for Perspectives errors
data PerspectivesError
  = DefaultPrototype ContextType ContextType
    | CyclicAspects ContextType

derive instance eqPerspectivesError :: Eq PerspectivesError

instance showPerspectivesError :: Show PerspectivesError where
  show (DefaultPrototype expected given) = "Invalid type for DefaultPrototype. Expected: '" <> unwrap expected <> "' but found '" <> unwrap given <> "'."
  show (CyclicAspects c) = "Context '" <> unwrap c <> "' has cyclic aspects."

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
