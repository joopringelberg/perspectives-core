module Test.Parsing.ArcAstSelectors where

import Prelude

import Data.List (List(..), elemIndex, filter, find, findIndex, fromFoldable, null)
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (traverse)
import Effect.Aff (Aff, throwError, error)
import Effect.Class.Console (logShow)
import Partial.Unsafe (unsafePartial)
import Perspectives.Parsing.Arc.AST (ActionE(..), AutomaticEffectE(..), ContextE(..), ContextPart(..), NotificationE(..), PropertyVerbE(..), PropsOrView, RoleE(..), RolePart(..), RoleVerbE(..), StateE(..), StateQualifiedPart(..), StateTransitionE(..))
import Perspectives.Parsing.Arc.Expression.AST (SimpleStep(..), Step(..))
import Perspectives.Parsing.Arc.Position (ArcPosition(..))
import Perspectives.Representation.TypeIdentifiers (RoleKind(..), StateIdentifier(..))
import Perspectives.Representation.Verbs (PropertyVerb(..), RoleVerbList) as Verbs

failure :: forall a. String -> Aff a
failure = throwError <<< error

blancoState :: StateIdentifier
blancoState = StateIdentifier ""

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

ensureStateInContext :: StateIdentifier -> ContextE -> Aff StateE
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
ensureSubState :: StateIdentifier -> StateE -> Aff StateE
ensureSubState stateId (StateE{subStates}) = case find (case _ of
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
  N (NotificationE {user}) -> usr == user
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
ensureStateInRole :: StateIdentifier -> RoleE -> Aff (List StateQualifiedPart)
ensureStateInRole stateId (RoleE{roleParts}) =
  pure $ join $ map (\(rp :: RolePart) ->
    case rp of
      SQP parts -> filter (unsafePartial \sp -> case sp of
          R (RoleVerbE{state}) -> eq state stateId
          P (PropertyVerbE{state}) -> eq state stateId
          AC (ActionE{state}) -> eq state stateId
          N (NotificationE{transition}) -> transitionForState stateId transition
          AE (AutomaticEffectE{transition}) -> transitionForState stateId transition)
        parts
      _ -> Nil)
    roleParts

transitionForState :: StateIdentifier -> StateTransitionE -> Boolean
transitionForState stateId (Entry s) = eq stateId s
transitionForState stateId (Exit s) = eq stateId s

ensurePerspectiveOn :: String -> (List StateQualifiedPart) -> Aff (List StateQualifiedPart)
ensurePerspectiveOn objectId = ensurePerspectiveOn_ (Simple (ArcIdentifier (ArcPosition{line: 0, column: 0}) objectId))

perspectiveExists :: List StateQualifiedPart -> Aff Unit
perspectiveExists l = if null l
  then failure "No perspective"
  else pure unit

ensurePerspectiveOn_ :: Step -> (List StateQualifiedPart) -> Aff (List StateQualifiedPart)
ensurePerspectiveOn_ objectId sp = case filter (case _ of
  R (RoleVerbE{object}) -> object == objectId
  P (PropertyVerbE{object}) -> object == objectId
  AC (ActionE{object}) -> object == objectId
  _ -> false) sp of
    notAny | null notAny -> failure ("No perspective on '" <> show objectId <> "'.")
    ps -> pure ps

ensurePerspectiveOf :: String -> (List StateQualifiedPart) -> Aff (List StateQualifiedPart)
ensurePerspectiveOf userName sp = case filter (case _ of
  R (RoleVerbE{subject}) -> subject == userName
  P (PropertyVerbE{subject}) -> subject == userName
  AC (ActionE{subject}) -> subject == userName
  _ -> false) sp of
    notAny | null notAny -> failure ("No perspective of '" <> userName <> "'.")
    ps -> pure ps

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
      (P (PropertyVerbE {propertyVerbs, propsOrView})) -> propVerbs' == propertyVerbs && pOrv == propsOrView
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
