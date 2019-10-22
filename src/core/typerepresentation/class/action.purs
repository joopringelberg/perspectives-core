module Perspectives.Representation.Class.Action where

import Control.Monad.Error.Class (throwError)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)
import Perspectives.Representation.Action (Action, Verb)
import Perspectives.Representation.Assignment (AssignmentStatement)
import Perspectives.Representation.Calculation (Calculation(..))
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.TypeIdentifiers (ActionType, EnumeratedRoleType, RoleType, ViewType)
import Prelude ((<<<), pure, (<>), ($))

-----------------------------------------------------------
-- ACTION TYPE CLASS
-----------------------------------------------------------
class ActionClass c where
  subject :: c -> EnumeratedRoleType
  verb :: c -> Verb
  object :: c -> RoleType
  indirectObject :: c -> Maybe RoleType
  requiredObjectProperties :: c -> Maybe ViewType
  requiredSubjectProperties :: c -> Maybe ViewType
  requiredIndirectObjectProperties :: c -> Maybe ViewType
  condition :: c -> MonadPerspectives QueryFunctionDescription
  effect :: c -> Maybe AssignmentStatement

instance actionActionClass :: ActionClass Action where
  subject = _.subject <<< unwrap
  verb = _.verb <<< unwrap
  object = _.object <<< unwrap
  indirectObject = _.indirectObject <<< unwrap
  requiredObjectProperties = _.requiredObjectProperties <<< unwrap
  requiredSubjectProperties = _.requiredSubjectProperties <<< unwrap
  requiredIndirectObjectProperties = _.requiredIndirectObjectProperties <<< unwrap
  condition r = case (unwrap r).condition of
    Q qd -> pure qd
    otherwise -> throwError (error ("Attempt to acces Condition of an Action before the expression has been compiled. This counts as a system programming error." <> (unwrap $ (identifier r :: ActionType))))
  effect = _.effect <<< unwrap
