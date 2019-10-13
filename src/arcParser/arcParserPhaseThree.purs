module Perspectives.Parsing.Arc.PhaseThree where

-- | Phase Three of the parser solves problems that arise due to forward reference.
-- | In a View, for example, the modeller can reference a property of a Role that
-- | has not yet been parsed in phase two (this may happen if that Role is filled with
-- | a role that is 'later' in the source text).
-- |

import Control.Monad.Except (throwError)
import Control.Monad.State (gets, modify)
import Control.Monad.Trans.Class (lift)
import Data.Array (filter, head, length)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, insert, keys, lookup, values)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, (###=), MP)
import Perspectives.DomeinCache (withDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.Identifiers (endsWithSegments, isQualifiedWithDomein)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition)
import Perspectives.Parsing.Arc.PhaseTwo (PhaseTwo', runPhaseTwo_')
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Representation.ADT (ADT(..), reduce)
import Perspectives.Representation.Action (Action(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.PersistentType (typeExists)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.TypeIdentifiers (ActionType(..), ContextType, EnumeratedRoleType(..), PropertyType, RoleType(..), propertytype2string, roletype2string)
import Perspectives.Representation.View (View(..))
import Perspectives.Types.ObjectGetters (lookForUnqualifiedPropertyType_, lookForUnqualifiedRoleType)
import Prelude (Unit, bind, map, pure, unit, void, ($), (<<<), (<>), (==), discard, (>>=))

-- | A Monad based on MonadPerspectives, with state that indicates whether the Subject of
-- | an Action is a Bot, and allows exceptions.
type PhaseThree a = PhaseTwo' a MonadPerspectives

lift2 :: forall a. MonadPerspectives a -> PhaseThree a
lift2 = lift <<< lift

phaseThree :: DomeinFileRecord -> MP (Either PerspectivesError DomeinFileRecord)
phaseThree df@{_id} = do
    -- TODO. Put the DomeinFileRecord that results from qualifyBindings in cache
    -- before calling qualifyPropertyReferences!
    (Tuple ei {dfr}) <- runPhaseTwo_'
      (do
        qualifyActionRoles
        qualifyBindings
        qualifyPropertyReferences)
      df
    case ei of
      (Left e) -> pure $ Left e
      otherwise -> pure $ Right dfr

getDF :: Unit -> PhaseThree DomeinFileRecord
getDF _ = lift $ gets _.dfr

-- | Qualifies the identifiers used in the object- and indirectObject field of an Action.
-- | All Objects are by default constructed as enumerated; this function corrects that if
-- | applicable.
-- | Note that this function requires the DomeinFile to be available in the cache!
qualifyActionRoles :: PhaseThree Unit
qualifyActionRoles = (lift $ gets _.dfr) >>= qualifyActionRoles'
  where
  qualifyActionRoles' :: DomeinFileRecord -> PhaseThree Unit
  qualifyActionRoles' {contexts, enumeratedRoles, actions, calculatedRoles} = for_ contexts
    \(Context{_id:ctxtId, gebruikerRol, rolInContext, contextRol}) -> for_ gebruikerRol
      \(EnumeratedRoleType ur) -> case lookup ur enumeratedRoles of
        Nothing -> throwError (Custom $ "Impossible error: cannot find '" <> ur <> "' in model.")
        (Just (EnumeratedRole {perspectives})) -> for_ (values perspectives)
          \acts -> for_ acts
            \(ActionType a) -> case lookup a actions of
              Nothing -> throwError (Custom $ "Impossible error: cannot find '" <> a <> "' in model.")
              (Just (Action ar@{_id: actId, object, indirectObject: mindirectObject, pos})) -> do
                ar' <- do
                  qname <- qualifiedRoleType calculatedRoles ctxtId pos (roletype2string object)
                  pure $ ar {object = qname}
                ar'' <- case mindirectObject of
                  (Just indirectObject) -> do
                    qname <- qualifiedRoleType calculatedRoles ctxtId pos (roletype2string indirectObject)
                    pure $ ar' {indirectObject = Just qname}
                  otherwise -> pure ar'
                if ar'' == ar
                  then pure unit
                  -- A change, so modify the DomeinFileRecord
                  else modifyDF (\df@{actions: actions'} -> df {actions = insert (unwrap actId) (Action ar'') actions'})
    where
      qualifiedRoleType :: Object CalculatedRole -> ContextType -> ArcPosition -> String -> PhaseThree RoleType
      qualifiedRoleType calculatedRoles ctxtId pos ident = if isQualifiedWithDomein ident
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
            -- This will not happen: a context does not allow two roles with the same local name.
            otherwise -> throwError $ NotUniquelyIdentifying pos ident (map roletype2string types)

modifyDF :: (DomeinFileRecord -> DomeinFileRecord) -> PhaseThree Unit
modifyDF f = void $ modify \s@{dfr} -> s {dfr = f dfr}

-- | Qualifies the identifiers used in the filledBy part of an EnumeratedRole declaration.
-- | A binding is represented as an ADT. We transform all elements of the form `ST segmentedName` in the tree
-- | to `ST qualifiedName`, using the `Reducible a (ADT b)` instance.
-- | We qualify a name only by searching the roles of the domain. Role names that have the segmentedName as a suffix
-- | are candidates to qualify it. Only one such Role may exist in the domain!
-- | Note that this function requires the DomeinFile to be available in the cache!
-- | This function just uses the DomeinFileRecord that is passed in as an argument.
qualifyBindings :: PhaseThree Unit
qualifyBindings = (lift $ gets _.dfr) >>= qualifyBindings'
  where
    qualifyBindings' :: DomeinFileRecord -> PhaseThree Unit
    qualifyBindings' {enumeratedRoles:roles} = for_ roles
      (\(EnumeratedRole rr@{_id, binding, pos}) -> do
        qbinding <- reduce (qualifyBinding pos) binding
        if binding == qbinding
          then pure unit
          else -- change the role in the domain
            modifyDF (\df@{enumeratedRoles} -> df {enumeratedRoles = insert (unwrap _id) (EnumeratedRole rr {binding = qbinding}) enumeratedRoles}))
      where
        qualifyBinding :: ArcPosition -> EnumeratedRoleType -> PhaseThree (ADT EnumeratedRoleType)
        qualifyBinding pos i@(EnumeratedRoleType ident) = if isQualifiedWithDomein ident
          then pure $ ST i
          else do
            (candidates :: Array String) <- pure $ filter (\_id -> _id `endsWithSegments` ident) (keys roles)
            case head candidates of
              Nothing -> throwError $ UnknownRole pos ident
              (Just qname) | length candidates == 1 -> pure $ ST $ EnumeratedRoleType $ qname
              otherwise -> throwError $ NotUniquelyIdentifying pos ident candidates


-- | Qualify the references to Properties in each View.
-- | Note that we need the DomeinFile with qualified bindings in the cache for this
-- | function to work correctly!
qualifyPropertyReferences :: PhaseThree Unit
qualifyPropertyReferences = (lift $ gets _.dfr) >>= qualifyPropertyReferences'
  where
    qualifyPropertyReferences' :: DomeinFileRecord -> PhaseThree Unit
    qualifyPropertyReferences' df@{_id, views} = do
      qviews <- traverseWithIndex qualifyView views
      modifyDF \dfr -> dfr {views = qviews}

      where
        qualifyView :: String -> View -> PhaseThree View
        qualifyView viewName (View vr@{propertyReferences, role, pos}) = do
          qprops <- traverse (qualifyProperty role pos) propertyReferences
          pure $ View $ vr {propertyReferences = qprops}

        qualifyProperty :: EnumeratedRoleType -> ArcPosition -> PropertyType -> PhaseThree PropertyType
        qualifyProperty erole pos propType = do
          (candidates :: Array PropertyType) <- lift2 $ withDomeinFile
            _id
            (DomeinFile df)
            (erole ###= lookForUnqualifiedPropertyType_ (propertytype2string propType))
          case head candidates of
            Nothing -> throwError $ UnknownProperty pos (propertytype2string propType)
            (Just t) | length candidates == 1 -> pure t
            otherwise -> throwError $ NotUniquelyIdentifying pos (propertytype2string propType) (map propertytype2string candidates)
