module Test.Parsing.DomeinFileSelectors where

--------------------------------------------------------------------------------
---- PROPERTY
--------------------------------------------------------------------------------
import Prelude

import Data.Array (filter, head)
import Data.Foldable (find, for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff (Aff, throwError, error)
import Foreign.Object (lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.DomeinFile (DomeinFileRecord)
import Perspectives.Identifiers (typeUri2typeNameSpace_)
import Perspectives.Query.QueryTypes (Calculation(..), QueryFunctionDescription, Range, RoleInContext(..), range)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Action (AutomaticAction)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.ExplicitSet (ExplicitSet(..), subsetPSet)
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..), objectOfPerspective) as Perspective
import Perspectives.Representation.Perspective (StateSpec)
import Perspectives.Representation.Range (Range) as Range
import Perspectives.Representation.State (State(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType)
import Perspectives.Representation.Verbs (PropertyVerb)
import Test.Unit (Test)

failure :: forall a. String -> Aff a
failure = throwError <<< error

exists :: forall a. a -> Aff Unit
exists _ = pure unit

both :: forall a. (a -> Aff Unit) -> (a -> Aff Unit) -> a -> Aff Unit
both f s  a = f a *> s a *> pure unit

all :: forall a. (Array (a -> Aff Unit)) -> a -> Aff Unit
all fs a = for_ fs (\f -> f a)
--------------------------------------------------------------------------------
---- ENUMERATEDPROPERTY
--------------------------------------------------------------------------------
ensureEnumeratedProperty :: String -> DomeinFileRecord -> Aff EnumeratedProperty
ensureEnumeratedProperty pname {enumeratedProperties} = case lookup pname enumeratedProperties of
  Nothing -> failure ("No property '" <> pname <> "'.")
  Just p -> pure p

--------------------------------------------------------------------------------
---- ENUMERATEDROLE
--------------------------------------------------------------------------------
ensureERole :: String -> DomeinFileRecord -> Aff EnumeratedRole
ensureERole roleName {enumeratedRoles} = case lookup roleName enumeratedRoles of
  Nothing -> failure  ("There should be a role '" <> roleName <> "'.")
  Just erole -> pure erole

-- | Ensure the object is compiled to a description before applying this function
-- | (in PhaseThree)
-- | NOTE: we assume that roleName is not instantiated as an Aspect role in another context
-- | (we derive the context type as the lexical context of the EnumeratedRoleType)
ensurePerspectiveOn :: String -> EnumeratedRole -> Aff Perspective.Perspective
ensurePerspectiveOn roleName (EnumeratedRole{perspectives}) = case head $ filter
  (unsafePartial Perspective.objectOfPerspective >>> eq (ST $ RoleInContext {context: ContextType $ typeUri2typeNameSpace_ roleName, role: EnumeratedRoleType roleName})) perspectives of
  Nothing -> failure  ("There should be a Perspective on '" <> roleName <> "'.")
  Just p -> pure p

-- | Use this function in phaseThree.
-- ensureObjectsAreCompiled :: EnumeratedRole -> Aff EnumeratedRole
-- ensureObjectsAreCompiled p@(EnumeratedRole{perspectives}) = do
--   for_ perspectives
--     (\(Perspective.Perspective{object}) -> case object of
--       Q _ -> pure p
--       _ -> failure "The object of the Perspective should be a description.")
--   pure p

--------------------------------------------------------------------------------
---- CALCULATEDROLE
--------------------------------------------------------------------------------
ensureCRole :: String -> DomeinFileRecord -> Aff CalculatedRole
ensureCRole roleName {calculatedRoles} = case lookup roleName calculatedRoles of
  Nothing -> failure  ("There should be a role '" <> roleName <> "'.")
  Just erole -> pure erole


--------------------------------------------------------------------------------
---- ROLEHASPROPERTY
--------------------------------------------------------------------------------
ensureEnumeratedRoleHasProperty :: String -> EnumeratedRole -> Aff PropertyType
ensureEnumeratedRoleHasProperty name (EnumeratedRole{id, properties}) = case find
  (case _ of
    ENP (EnumeratedPropertyType n) -> n == name
    CP (CalculatedPropertyType n) -> n == name) properties of
  Nothing -> failure ("Role '" <> show id <> "' has no property '" <> name <> "'.")
  Just r -> pure r

enumeratedPropertyIsFunctional :: Boolean -> EnumeratedProperty -> Aff Unit
enumeratedPropertyIsFunctional b (EnumeratedProperty{functional}) = if functional == b
  then pure unit
  else if b
    then failure "Property is not functional."
    else failure "Property is functional."

enumeratedPropertyIsMandatory :: Boolean -> EnumeratedProperty -> Aff Unit
enumeratedPropertyIsMandatory b (EnumeratedProperty{mandatory}) = if mandatory == b
  then pure unit
  else if b
    then failure "Property is not mandatory."
    else failure "Property is mandatory."

enumeratedPropertyHasRange :: Range.Range -> EnumeratedProperty -> Aff Unit
enumeratedPropertyHasRange r (EnumeratedProperty{range}) = if range == r
  then pure unit
  else failure ("Property does not have range '" <> show r <> "'.")

-- (ExplicitSet PropertyType) haveVerbs (Array PropertyVerb)
-- eg: Universal `haveVerbs` [Consult]
haveVerbs :: (ExplicitSet PropertyType) -> (Array PropertyVerb) -> (Array Perspective.PropertyVerbs) -> Test
haveVerbs props verbs pvArr = case find (\(Perspective.PropertyVerbs propset verbSet) -> propset `subsetPSet` props && verbSet `subsetPSet` (PSet verbs)) pvArr of
  Nothing -> failure ("Expected '" <> show pvArr <> "' for '" <> show props <> "'.")
  Just _ -> pure unit

--------------------------------------------------------------------------------
---- PERSPECTIVE
--------------------------------------------------------------------------------
ensurePropertyVerbsInState  :: StateSpec -> Perspective.Perspective -> Aff (Array Perspective.PropertyVerbs)
ensurePropertyVerbsInState stateSpec (Perspective.Perspective{propertyVerbs}) = do
  -- log $ showTree (unwrap propertyVerbs)
  case Map.lookup stateSpec (unwrap propertyVerbs) of
    Nothing -> failure ("There should be an entry in propertyVerbs for state '" <> show stateSpec <> "'.")
    Just pv -> pure pv

-- ensureRoleVerbsInState :: String -> Perspective.Perspective -> Aff RoleVerbList
-- ensureRoleVerbsInState stateName (Perspective.Perspective {roleVerbs}) = case Map.lookup (StateIdentifier stateName) (unwrap roleVerbs) of
--   Nothing -> failure ("There should be an entry in roleVerbs for state '" <> stateName <> "'.")
--   Just rv -> pure rv

objectOfPerspective :: Perspective.Perspective -> Aff QueryFunctionDescription
objectOfPerspective (Perspective.Perspective{object}) = pure object

isCalculationOf :: Range -> QueryFunctionDescription -> Aff Unit
isCalculationOf r qfd = if r == range qfd
  then pure unit
  else failure ("Calculation is not the right type")

--------------------------------------------------------------------------------
---- STATE
--------------------------------------------------------------------------------
ensureState :: String -> DomeinFileRecord -> Aff State
ensureState stateId {states} = do
  case lookup stateId states of
    Nothing -> failure ("There should be a state '" <> show stateId <> "'.")
    Just s -> pure s

stateQuery :: State -> Aff Calculation
stateQuery = pure <<< _.query <<< unwrap

ensureDescription :: Calculation -> Aff QueryFunctionDescription
ensureDescription (Q qfd) = pure qfd
ensureDescription _ = failure "The query of a state should have been compiled to a description."

ensureOnEntry :: RoleType -> State -> Aff AutomaticAction
ensureOnEntry rt (State{automaticOnEntry}) = case Map.lookup rt (unwrap automaticOnEntry) of
  Nothing -> failure ("No automatic on entry effect for " <> show rt)
  Just automaticAction -> pure automaticAction

{-
getERole :: DomeinFileRecord -> String -> Maybe EnumeratedRole
getERole {enumeratedRoles} roleName = lookup roleName enumeratedRoles

ensurePerspectiveInStateOn :: EnumeratedRole -> String -> Aff Perspective
ensurePerspectiveInStateOn roleName stateId (EnumeratedRole{perspectives}) = case head $ filter (objectOfPerspective >>> eq (ST $ EnumeratedRole roleName)) of
  Nothing -> failure ("There should be a Perspective on '" <> roleName <> "' in state '" <> show stateId <> "'.")
  Just p@(Perspective {roleVerbs}) -> if isJust $ Map.lookup stateId roleVerbs
    then pure p
    else failure ("Perspective is not valid for state '" <> show stateId <> "'.")

getPerspectiveOn :: EnumeratedRole -> String -> Maybe Perspective
getPerspectiveOn (EnumeratedRole{perspectives}) roleName = head $ filter (unsafePartial objectOfPerspective >>> eq (ST $ EnumeratedRoleType roleName)) perspectives

ensureRoleVerb  :: RoleVerb -> RoleVerbList -> Test
ensureRoleVerb roleVerb p = if perspectiveHasRoleVerb p roleVerb
  then assert "ok" true
  else assert ("The Perspective should have role verb '" <> show roleVerb <> "'") false

perspectiveHasRoleVerb :: Perspective -> RoleVerb -> Boolean
perspectiveHasRoleVerb (Perspective{roleVerbs}) roleVerb = isJust $ find (roleVerbsInclude roleVerb) $ Map.values (unwrap roleVerbs)

perspectiveHasRoleVerbInState :: Perspective -> RoleVerb -> StateIdentifier -> Aff Unit
perspectiveHasRoleVerbInState (Perspective{roleVerbs}) roleVerb stateId = case Map.lookup stateId (unwrap roleVerbs) of
  Nothing -> failure ("Perspective is not valid for '" <> show stateId <> "'.")
  Just roleVerbs -> if roleVerbsInclude roleVerb roleVerbs
    then pure unit
    else failure ("Perspective does not have '" <> show roleVerb <> "' in state '" <> show stateId <> "'.")

roleVerbsInclude :: RoleVerb -> RoleVerbList -> Boolean
roleVerbsInclude _ All = true
roleVerbsInclude v (Including verbs) = isJust $ elemIndex v verbs
roleVerbsInclude v (Excluding verbs) = isNothing $ elemIndex v verbs

ensurePropertyVerb  :: PropertyVerb -> Perspective -> Test
ensurePropertyVerb propertyVerb (Perspective{propertyVerbs}) = case lookup propertyVerb (unwrap propertyVerbs) of
  Nothing -> assert "The Perspective should have property verb '" <> show propertyVerb <> "'" false
  otherwise -> assert "ok" true
-}
