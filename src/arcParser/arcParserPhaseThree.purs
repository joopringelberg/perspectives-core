module Perspectives.Parsing.Arc.PhaseThree where

-- | Phase Three of the parser solves problems that arise due to forward reference.
-- | In a View, for example, the modeller can reference a property of a Role that
-- | has not yet been parsed in phase two (this may happen if that Role is filled with
-- | a role that is 'later' in the source text).
-- |

import Control.Monad.Except (throwError)
import Control.Monad.State (modify)
import Control.Monad.Trans.Class (lift)
import Data.Array (head, length)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Foreign.Object (insert, lookup, values)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, (###=), MP)
import Perspectives.DomeinCache (withDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.Identifiers (isQualifiedWithDomein)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition)
import Perspectives.Parsing.Arc.PhaseTwo (PhaseTwo', runPhaseTwo_')
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Representation.Action (Action(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.PersistentType (typeExists)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.TypeIdentifiers (ActionType(..), ContextType, EnumeratedRoleType(..), RoleType(..), roletype2string)
import Perspectives.Types.ObjectGetters (lookForUnqualifiedRoleType)
import Prelude (Unit, bind, map, pure, unit, void, ($), (<<<), (<>), (==))

-- | A Monad based on MonadPerspectives, with state that indicates whether the Subject of
-- | an Action is a Bot, and allows exceptions.
type PhaseThree a = PhaseTwo' a MonadPerspectives

lift2 :: forall a. MonadPerspectives a -> PhaseThree a
lift2 = lift <<< lift

phaseThree :: DomeinFileRecord -> MP (Either PerspectivesError DomeinFileRecord)
phaseThree df@{_id} = withDomeinFile _id (DomeinFile df)
  do
    (Tuple ei {dfr}) <- runPhaseTwo_' (qualifyActionRoles df) df
    case ei of
      (Left e) -> pure $ Left e
      otherwise -> pure $ Right dfr

-- | Qualifies the identifiers used in the object- and indirectObject field of an Action.
-- | All Objects are by default constructed as enumerated; this function corrects that if
-- | applicable.
-- | Note that this function requires the DomeinFile to be available in the cache!
qualifyActionRoles :: DomeinFileRecord -> PhaseThree Unit
qualifyActionRoles {contexts, enumeratedRoles, actions, calculatedRoles} = for_ contexts
  \(Context{_id:ctxtId, gebruikerRol, rolInContext, contextRol}) -> for_ gebruikerRol
    \(EnumeratedRoleType ur) -> case lookup ur enumeratedRoles of
      Nothing -> throwError (Custom $ "Impossible error: cannot find '" <> ur <> "' in model.")
      (Just (EnumeratedRole {perspectives})) -> for_ (values perspectives)
        \acts -> for_ acts
          \(ActionType a) -> case lookup a actions of
            Nothing -> throwError (Custom $ "Impossible error: cannot find '" <> a <> "' in model.")
            (Just (Action ar@{_id: actId, object, indirectObject: mindirectObject, pos})) -> do
              -- TODO. Refactor een functie String -> MP (Array RoleType)
              ar' <- do
                qname <- qualifiedRoleType ctxtId pos (roletype2string object)
                pure $ ar {object = qname}
              ar'' <- case mindirectObject of
                (Just indirectObject) -> do
                  qname <- qualifiedRoleType ctxtId pos (roletype2string indirectObject)
                  pure $ ar' {indirectObject = Just qname}
                otherwise -> pure ar'
              if ar'' == ar
                then pure unit
                -- A change, so modify the DomeinFileRecord
                else modifyDF (\df@{actions: actions'} -> df {actions = insert (unwrap actId) (Action ar'') actions'})
  where
    qualifiedRoleType :: ContextType -> ArcPosition -> String -> PhaseThree RoleType
    qualifiedRoleType ctxtId pos ident = if isQualifiedWithDomein ident
      then case lookup ident calculatedRoles of
        Nothing -> do
          -- Does the role exist at all (in some other model)?
          exists <- lift2 $ typeExists (EnumeratedRoleType ident)
          if exists
            then pure $ ENR $ EnumeratedRoleType ident
            else throwError $ UnknownRole pos ident
        (Just (CalculatedRole{_id:id'})) -> pure $ CR id'
      else do
        types <- lift2 $ ctxtId ###= lookForUnqualifiedRoleType ident
        case length types of
          0 -> throwError $ RoleMissingInContext pos ident (unwrap ctxtId)
          1 -> pure $ unsafePartial $ fromJust $ head types
          otherwise -> throwError $ NotUniquelyIdentifying pos ident (map roletype2string types)

    modifyDF :: (DomeinFileRecord -> DomeinFileRecord) -> PhaseThree Unit
    modifyDF f = void $ modify \s@{dfr} -> s {dfr = f dfr}
