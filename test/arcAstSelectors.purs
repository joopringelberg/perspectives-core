module Test.Parsing.ArcAstSelectors where

import Prelude

import Data.Array (fromFoldable) as ARR
import Data.List (List(..), filter, find, findIndex, fromFoldable, null)
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff (Aff, throwError, error)
import Partial.Unsafe (unsafePartial)
import Perspectives.Parsing.Arc.AST (ActionE(..), AutomaticEffectE(..), ContextE(..), ContextPart(..), NotificationE(..), PropertyVerbE(..), PropsOrView, RoleE(..), RoleIdentification(..), RolePart(..), RoleVerbE(..), StateE(..), StateQualifiedPart(..), StateSpecification(..), StateTransitionE(..))
import Perspectives.Parsing.Arc.Expression.AST (SimpleStep(..), Step(..))
import Perspectives.Parsing.Arc.Position (ArcPosition(..))
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), RoleKind(..), roletype2string)
import Perspectives.Representation.Verbs (PropertyVerb(..), RoleVerbList) as Verbs

failure :: forall a. String -> Aff a
failure = throwError <<< error

blancoState :: StateSpecification
blancoState = ContextState (ContextType "model:") Nothing

allPropertyVerbs :: Array Verbs.PropertyVerb
allPropertyVerbs = [Verbs.RemovePropertyValue, Verbs.DeleteProperty, Verbs.AddPropertyValue, Verbs.SetPropertyValue]

--------------------------------------------------------------------------------
---- CONTEXT
--------------------------------------------------------------------------------
-- | Results in a RoleE or fails the test.
ensureUserRole :: String -> ContextE -> Aff RoleE
ensureUserRole roleId (ContextE{contextParts}) = case find
  (\cp -> case cp of
    RE (RoleE{id, kindOfRole}) -> id == roleId && kindOfRole == UserRole
    _ -> false)
  contextParts of
    Just (RE r) -> pure r
    _ -> failure ("No user role '" <> roleId <> "' found.")

isIndexed :: String -> ContextE -> Aff Unit
isIndexed indexedName (ContextE{contextParts}) = case find
  (case _ of
    IndexedContext name _ -> indexedName == name
    _ -> false)
  contextParts of
    Just _ -> pure unit
    Nothing -> failure ("No indexed name '" <> indexedName <> "' in context.")

ensureContext :: String -> ContextE -> Aff ContextE
ensureContext cid (ContextE{contextParts}) =  case find (case _ of
  CE (ContextE{id}) -> id == cid
  _ -> false)
  contextParts of
    Just (CE c) -> pure c
    _ -> failure ("No context '" <> show cid <> "'.")

ensureStateInContext :: StateSpecification -> ContextE -> Aff StateE
ensureStateInContext stateId (ContextE{contextParts}) = case find (case _ of
  STATE (StateE{id}) -> id == stateId
  _ -> false)
  contextParts of
    Just (STATE s) -> pure s
    _ -> failure ("No state '" <> show stateId <> "'.")

stateExists :: StateE -> Aff Unit
stateExists s = pure unit

stateParts :: StateE -> Aff (List StateQualifiedPart)
stateParts (StateE{stateParts:sp}) = pure sp
--------------------------------------------------------------------------------
---- STATE
--------------------------------------------------------------------------------
ensureSubState :: StateSpecification -> StateE -> Aff StateE
ensureSubState stateId (StateE{subStates}) = do
  -- logShow subStates
  case find (case _ of
    StateE{id} -> id == stateId)
    subStates of
      Just s -> pure s
      _ -> failure ("No state '" <> show stateId <> "'.")

ensureOnEntry :: (List StateQualifiedPart) -> Aff (List StateQualifiedPart)
ensureOnEntry sqps = case filter (case _ of
  N (NotificationE {transition}) -> case transition of
    Entry _ -> true
    _ -> false
  AE (AutomaticEffectE{transition}) -> case transition of
    Entry _ -> true
    _ -> false
  _ -> false) sqps of
    Nil -> failure "No on entry clause."
    parts -> pure parts

ensureOnExit :: (List StateQualifiedPart) -> Aff (List StateQualifiedPart)
ensureOnExit sqps = case filter (case _ of
  N (NotificationE {transition}) -> case transition of
    Exit _ -> true
    _ -> false
  AE (AutomaticEffectE{transition}) -> case transition of
    Exit _ -> true
    _ -> false
  _ -> false) sqps of
    Nil -> failure "No on exit clause."
    parts -> pure parts

isNotified :: String -> (List StateQualifiedPart) -> Aff Unit
isNotified usr sqps = case filter (case _ of
  N (NotificationE {user}) -> case user of
    ExplicitRole _ u _ -> usr == roletype2string u
    (ImplicitRole _ (Simple (ArcIdentifier _ rl))) -> rl == usr
    _ -> false
  _ -> false) sqps of
    Nil -> failure $ "User '" <> usr <> "' is not notified."
    _ -> pure unit

hasAutomaticAction :: (List StateQualifiedPart) -> Aff Unit
hasAutomaticAction sqps = if isJust $ findIndex (case _ of
  AE _ -> true
  _ -> false) sqps
  then pure unit
  else failure "No automatic action."

--------------------------------------------------------------------------------
---- ROLE
--------------------------------------------------------------------------------
-- hasProperty :: RoleE -> PropertyE

-- | Get all parts for a particular state
ensureStateInRole :: (StateSpecification -> Boolean) -> RoleE -> Aff (List StateQualifiedPart)
ensureStateInRole tester (RoleE{roleParts}) = do
  -- logShow roleParts
  pure $ join $ map (\(rp :: RolePart) ->
    case rp of
      SQP parts -> filter (unsafePartial \sp -> case sp of
          R (RoleVerbE{state}) -> tester state
          P (PropertyVerbE{state}) -> tester state
          AC (ActionE{state}) -> tester state
          N (NotificationE{transition}) -> transitionForState tester transition
          AE (AutomaticEffectE{transition}) -> transitionForState tester transition)
        parts
      _ -> Nil)
    roleParts

isStateWithContext :: String -> StateSpecification -> Boolean
isStateWithContext contextName (ContextState (ContextType ctxt) _) = contextName == ctxt
isStateWithContext _ _ = false

isStateWithContext_ :: String -> Maybe String -> StateSpecification -> Boolean
isStateWithContext_ contextName path (ContextState (ContextType ctxt) p) = contextName == ctxt && path == p
isStateWithContext_ _ _ _ = false

isStateWithExplicitRole :: String -> StateSpecification -> Boolean
isStateWithExplicitRole roleName (SubjectState (ExplicitRole _ r _) _) = roleName == roletype2string r
isStateWithExplicitRole _ _ = false

isStateWithExplicitRole_ :: String -> Maybe String -> StateSpecification -> Boolean
isStateWithExplicitRole_ roleName path (SubjectState (ExplicitRole _ r _) p) = roleName == roletype2string r && path == p
isStateWithExplicitRole_ _ _ _ = false

transitionForState :: (StateSpecification -> Boolean) -> StateTransitionE -> Boolean
transitionForState tester (Entry s) = tester s
transitionForState tester (Exit s) = tester s

ensurePerspectiveOn :: String -> (List StateQualifiedPart) -> Aff (List StateQualifiedPart)
ensurePerspectiveOn objectId = ensurePerspectiveOn_ (Simple (ArcIdentifier (ArcPosition{line: 0, column: 0}) objectId))

perspectiveExists :: List StateQualifiedPart -> Aff Unit
perspectiveExists l = if null l
  then failure "No perspective"
  else pure unit

ensurePerspectiveOn_ :: Step -> (List StateQualifiedPart) -> Aff (List StateQualifiedPart)
ensurePerspectiveOn_ objectId sp = do
  -- logShow sp
  case filter (case _ of
    R (RoleVerbE{object}) -> case object of
      ImplicitRole _ stp -> objectId == stp
      _ -> false
    P (PropertyVerbE{object}) -> case object of
      ImplicitRole _ stp -> objectId == stp
      _ -> false
    AC (ActionE{object}) -> case object of
      ImplicitRole _ stp -> objectId == stp
      _ -> false
    _ -> false) sp of
    notAny | null notAny -> failure ("No perspective on '" <> show objectId <> "'.")
    ps -> pure ps

ensurePerspectiveOf :: (RoleIdentification -> Boolean) -> (List StateQualifiedPart) -> Aff (List StateQualifiedPart)
ensurePerspectiveOf tester sp = case filter (case _ of
  R (RoleVerbE{subject}) -> tester subject
  P (PropertyVerbE{subject}) -> tester subject
  AC (ActionE{subject}) -> tester subject
  _ -> false) sp of
    notAny | null notAny -> failure "No perspective of X found"
    ps -> pure ps

-- (Simple (ArcIdentifier (ArcPosition { column: 22, line: 4 }) "SomeUser")
isImplicitRoleOnIdentifier :: String -> RoleIdentification -> Boolean
isImplicitRoleOnIdentifier localRoleName (ImplicitRole _ (Simple (ArcIdentifier _ rl))) = rl == localRoleName
isImplicitRoleOnIdentifier _ _ = false

-- | Ensure the RoleVerbList holds in the list of StateQualifiedParts
ensureRoleVerbs :: Verbs.RoleVerbList -> List StateQualifiedPart -> Aff Unit
ensureRoleVerbs rvl sp = case find (case _ of
  R (RoleVerbE {roleVerbs}) -> rvl == roleVerbs
  _ -> false) sp of
    Just _ -> pure unit
    Nothing -> failure ("No RoleVerbs '" <> show rvl <> "'.")

-- | Ensure the PropertyVerbs hold for the Properties (or View) in the state for the user role.
ensurePropertyVerbsForPropsOrView :: Array Verbs.PropertyVerb -> PropsOrView -> List StateQualifiedPart -> Aff Unit
ensurePropertyVerbsForPropsOrView propVerbs pOrv sp = ensurePropertyVerbsForPropsOrView' (fromFoldable propVerbs)
  where
    ensurePropertyVerbsForPropsOrView' :: List Verbs.PropertyVerb -> Aff Unit
    ensurePropertyVerbsForPropsOrView' propVerbs' = case find (case _ of
      (P (PropertyVerbE {propertyVerbs, propsOrView})) -> (PSet $ ARR.fromFoldable propVerbs') == propertyVerbs && pOrv == propsOrView
      _ -> false
      ) sp of
        Just _ -> pure unit
        Nothing -> failure ("No PropertyVerbs '" <> show propVerbs <> "' for properties '" <> show pOrv <> "'.")

--------------------------------------------------------------------------------
---- ACTION
--------------------------------------------------------------------------------
ensureAction :: String -> List StateQualifiedPart -> Aff (List StateQualifiedPart)
ensureAction actionName sqps = pure $ filter (case _ of
  AC (ActionE{id}) -> actionName == id
  _ -> false) sqps

actionExists :: List StateQualifiedPart -> Aff Unit
actionExists l = if null l
  then failure "No action"
  else pure unit
