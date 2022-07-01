-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

-- TODO
  --  A case expression contains unreachable cases:
  --
  --   (MQD dom fun args ran _ _)                 a
  --   (BQD _ (BinaryCombinator g) f1 f2 ran _ _) a
  --   (BQD _ (BinaryCombinator g) f1 f2 _ _ _)   a
  --   (BQD _ (BinaryCombinator g) f1 f2 ran _ _) a
  --   (BQD _ (BinaryCombinator g) f1 f2 _ _ _)   a
  --   ...
  --
  -- in binding group interpret, getterFromPropertyType, getDynamicPropertyGetter


module Perspectives.Query.Interpreter where

import Control.Monad.AvarMonadAsk (modify)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Control.MonadZero (empty, join, void)
import Data.Array (elemIndex, head, length, null, union, unsafeIndex)
import Data.List (List(..))
import Data.List.NonEmpty (fromList, tail)
import Data.List.Types (List, NonEmptyList)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~>), MP, MPQ, liftToInstanceLevel, (##>>), (##=))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.External.HiddenFunctionCache (lookupHiddenFunction, lookupHiddenFunctionNArgs)
import Perspectives.HiddenFunction (HiddenFunction)
import Perspectives.Identifiers (isExternalRole)
import Perspectives.Instances.Combinators (available', not_)
import Perspectives.Instances.Environment (_pushFrame)
import Perspectives.Instances.ObjectGetters (binding, binding_, context, contextModelName, contextType, externalRole, fills, getEnumeratedRoleInstances, getFilledRoles, getProperty, getUnlinkedRoleInstances, roleModelName, roleType)
import Perspectives.Instances.Values (bool2Value, value2Date, value2Int)
import Perspectives.Names (lookupIndexedContext, lookupIndexedRole)
import Perspectives.PerspectivesState (addBinding, getVariableBindings, pushFrame, restoreFrame)
import Perspectives.Query.Interpreter.Dependencies (Dependency(..), DependencyPath, addAsSupportingPaths, allPaths, appendPaths, applyValueFunction, consOnMainPath, dependencyToValue, domain2Dependency, functionOnBooleans, functionOnStrings, singletonPath, snocOnMainPath, (#>>))
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..))
import Perspectives.Query.UnsafeCompiler (lookup, mapNumericOperator, orderFunction, performNumericOperation')
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.PersistentType (getPerspectType)
import Perspectives.Representation.Class.Property (calculation) as PC
import Perspectives.Representation.Class.Property (getProperType)
import Perspectives.Representation.Class.Role (allLocallyRepresentedProperties, calculation)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), propertytype2string)
import Perspectives.Types.ObjectGetters (allRoleTypesInContext, contextTypeModelName', roleTypeModelName', specialisesRoleType)
import Prelude (bind, discard, flip, pure, show, ($), (<$>), (<<<), (<>), (==), (>=>), (>>=), notEq, (&&), (||), (<#>), (<=))
import Unsafe.Coerce (unsafeCoerce)

lift2MPQ :: forall a. MP a -> MPQ a
lift2MPQ = lift <<< lift

interpret :: QueryFunctionDescription -> DependencyPath ~~> DependencyPath

-----------------------------------------------------------
-- UQD
-----------------------------------------------------------
interpret (UQD _ (BindVariable varName) f1 _ _ _) a = ArrayT do
  values <- runArrayT $ interpret f1 a
  lift $ addBinding varName (toString <$> values)
  pure values
interpret (UQD _ WithFrame f1 _ _ _) a = do
  old <- lift2MPQ getVariableBindings
  void $ lift $ lift $ modify \s@{variableBindings} -> s {variableBindings = (_pushFrame old)}
  x <- interpret f1 a
  void $ lift $ lift $ modify \s@{variableBindings} -> s {variableBindings = old}
  pure x
interpret (UQD _ (UnaryCombinator ExistsF) f1 _ _ _) a = ArrayT do
  (r :: Array DependencyPath) <- runArrayT $ interpret f1 a
  pure $ (consOnMainPath (V "ExistsF" $ Value (show $ null r))) <$> r
interpret (UQD _ (UnaryCombinator FilledByF) f1 _ _ _) a = do
  (boundRoleL :: DependencyPath) <- interpret f1 a
  -- If the head boundRole is a RoleInstance and (a `bindsRole` head boundRole) is true,
  -- add V (Value "true") to boundRole
  case a.head, boundRoleL.head of
    (R bindingRole), (R boundRole) -> do
      b <- lift $ lift (boundRole ##>> fills bindingRole)
      if b
        then pure (consOnMainPath (V "FilledByF" (Value "true")) boundRoleL)
        else pure (consOnMainPath (V "FilledByF" (Value "false")) boundRoleL)
    _, _ -> pure (consOnMainPath (V "FilledByF" (Value "false")) boundRoleL)

interpret (UQD _ (UnaryCombinator FillsF) f1 _ _ _) a =  do
  (boundRoleL :: DependencyPath) <- interpret f1 a
  -- If the head boundRole is a RoleInstance and a `bindsRole` head boundRole is true,
  -- cons V (Value "true") op boundRole
  case a.head, boundRoleL.head of
    (R bindingRole), (R boundRole) -> do
      b <- lift $ lift (boundRole ##>> fills bindingRole)
      if b
        then pure (consOnMainPath (V "FillsF" (Value "true")) boundRoleL)
        else pure (consOnMainPath (V "FillsF" (Value "false")) boundRoleL)
    _, _ -> pure (consOnMainPath (V "FillsF" (Value "false")) boundRoleL)

interpret (UQD _ (UnaryCombinator AvailableF) f1 _ _ _) a = ArrayT do
  (r :: Array DependencyPath) <- runArrayT $ interpret f1 a
  result <- lift $ available' (toString <$> r)
  pure $ [consOnMainPath (V "AvailableF" (Value $ show result)) a]

interpret (UQD _ (UnaryCombinator NotF) f1 _ _ _) a = ArrayT do
  (r :: Array DependencyPath) <- runArrayT $ interpret f1 a
  -- The DescriptionCompiler ensures that we have only Value type heads.
  result <- lift $ not_ (Value <<< toString <$> r)
  pure $ [consOnMainPath (V "NotF" (Value $ show result)) a]

-----------------------------------------------------------
-- BQD
-----------------------------------------------------------
interpret (BQD _ (BinaryCombinator ComposeF) f1 f2@(SQD _ (Constant _ _) _ _ _) _ _ _) a = interpret f2 a
interpret (BQD _ (BinaryCombinator ComposeF) f1 f2@(SQD _ (DataTypeGetter IdentityF) _ _ _) _ _ _) a = interpret f1 a
interpret (BQD _ (BinaryCombinator ComposeF) f1@(SQD _ (DataTypeGetter IdentityF) _ _ _) f2 _ _ _) a = interpret f2 a
interpret (BQD _ (BinaryCombinator ComposeF) f1 f2 _ _ _) a =
  (interpret f1 >=> interpret f2) a
interpret (BQD _ (BinaryCombinator FilterF) source criterium _ _ _) a = ArrayT do
  (values :: Array DependencyPath) <- runArrayT $ interpret source a
  -- Each result contains just a single DependencyPath, or is empty.
  -- The main path of that result is the main path of the value path (v).
  -- All paths resulting from the criterium are added as supporting paths to the result.
  -- NOTICE that every result has at least a single supporting path.
  (results :: Array (Array DependencyPath)) <- for values (\v -> do
    -- The DescriptionCompiler only allows functional criteria, so r is either empty or
    -- contains a single result.
    (r :: Array DependencyPath) <- runArrayT $ interpret criterium v
    case head r of
      Just l -> unsafePartial $ case l.head of
        (V "FilterF" (Value "true")) -> pure $ [appendPaths v l]
        (V "FilterF" (Value "false")) -> pure []
      -- interpret no criterium result as false.
      otherwise -> pure []
    )
  pure $ join results
interpret (BQD _ (BinaryCombinator SequenceF) f1 f2 _ _ _) a = ArrayT $ do
  (f1r :: Array DependencyPath) <- runArrayT $ interpret f1 a
  f2r <- runArrayT $ interpret f2 a
  -- Let's assume both result in a single DependencyPath.
  -- If the sequence represents a let statement, we should make the main path of the DependencyPath resulting from the
  -- second expression the main path of the end result, to which we add all paths in the first as supporting paths.
  -- This works out well for recursively nested sequences. In the end, the very last main path becomes
  -- the main path of the entire expression; all others are supporting paths.
  --
  -- However, in general, the second expression may result in multiple DependencyPaths (think of obtaining the
  -- instances of a role or of its binders).
  -- We should apply the above to ALL those paths.
  --
  -- Finally, the first expression may yield more than one DependencyPath, too. We simply obtain all paths from
  -- all of them and add them as secundary paths.
  (allSecundaries :: Array (NonEmptyList Dependency)) <- pure $ join (allPaths <$> f1r)
  pure (addAsSupportingPaths allSecundaries <$> f2r)

interpret (BQD _ (BinaryCombinator IntersectionF) f1 f2 _ _ _) a = ArrayT do
  (f1r :: Array DependencyPath) <- runArrayT $ interpret f1 a
  if null f1r
    then do
      f2r <- runArrayT $ interpret f2 a
      pure $ f1r `union` f2r
    else pure f1r

interpret (BQD _ (BinaryCombinator UnionF) f1 f2 _ _ _) a = ArrayT do
  (l :: Array DependencyPath) <- runArrayT $ interpret f1 a
  (r :: Array DependencyPath) <- runArrayT $ interpret f2 a
  pure (l `union` r)

interpret (BQD _ (BinaryCombinator FilledByF) sourceOfBindingRoles sourceOfBoundRoles _ _ _) a = ArrayT do
  (bindingRoles :: Array DependencyPath) <- runArrayT $ interpret sourceOfBindingRoles a
  (boundRoles :: Array DependencyPath) <- runArrayT $ interpret sourceOfBindingRoles a
  -- bindingRoles and boundRoles must be functional.
  case head bindingRoles, head boundRoles of
    Just bindingRolesh, Just boundRolesh | length bindingRoles <= 1 && length boundRoles <= 1 ->
      case bindingRolesh.head, boundRolesh.head of
        R filled, R filler -> do
          result <- lift (filler ##>> (fills filled))
          pure [{ head: (V "" (Value $ show result))
                , mainPath: Nothing
                , supportingPaths: allPaths bindingRolesh `union` allPaths boundRolesh
                }]
        _, _ -> throwError (error $ "'binds' expects two roles, but got: " <> show bindingRolesh.head <> " and " <> show boundRolesh.head)
    Just bindingRolesh, Just boundRolesh -> throwError (error $ "'binds' expects at most a single role instance on both the left and right side")
    _, _ -> pure [{ head: (V "" (Value "false"))
                , mainPath: Nothing
                , supportingPaths: []
                }]

-- The compiler only allows f1 and f2 if they're functional.
interpret (BQD _ (BinaryCombinator g) f1 f2 _ _ _) a | isJust $ elemIndex g [EqualsF, NotEqualsF] = ArrayT do
  -- Both are singleton arrays, or empty.
  (fr1 :: Array DependencyPath) <- runArrayT (interpret f1 a)
  fr2 <- runArrayT (interpret f1 a)
  unsafePartial $ case g of
    EqualsF -> case head fr1, head fr2 of
      Just fr1h, Just fr2h -> pure [unsafePartial applyValueFunction (functionOnStrings \x y -> show (x == y)) fr1h fr2h]
      _, _ -> pure []
    NotEqualsF -> case head fr1, head fr2 of
      Just fr1h, Just fr2h -> pure [unsafePartial applyValueFunction (functionOnStrings \x y -> show (x `notEq` y)) fr1h fr2h]
      _, _ -> pure []

-- The Description Compiler makes sure we only have Value types that can be ordered, here.
-- It also ensures the f1 and f2 are functional.
interpret (BQD _ (BinaryCombinator g) f1 f2 ran _ _) a | isJust $ elemIndex g [LessThanF, LessThanEqualF, GreaterThanF, GreaterThanEqualF] = ArrayT do
  -- Both are singleton arrays, or empty.
  (fr1 :: Array DependencyPath) <- runArrayT (interpret f1 a)
  fr2 <- runArrayT (interpret f1 a)
  case head fr1, head fr2 of
    Just fr1h, Just fr2h -> unsafePartial $ case ran of
      VDOM PString _ -> pure [unsafePartial applyValueFunction (functionOnStrings \x y -> show $ (orderFunction g) x y) fr1h fr2h]
      VDOM PBool _ -> pure [unsafePartial applyValueFunction (functionOnBooleans (orderFunction g)) fr1h fr2h]
      VDOM PNumber _ -> pure [unsafePartial $ applyValueFunction (\x y -> bool2Value ((orderFunction g) (value2Int x) (value2Int y))) fr1h fr2h]
      VDOM PDate _ -> pure [unsafePartial applyValueFunction (\x y -> bool2Value ((orderFunction g) (value2Date x) (value2Date y))) fr1h fr2h] 
      VDOM PEmail _ -> pure [unsafePartial applyValueFunction (functionOnStrings \x y -> show $ (orderFunction g) x y) fr1h fr2h]
    _, _ -> pure []

  -- throwError (error $ "Perspectives.Query.Interpreter: no implementation for LessThanF, LessThanEqualF, GreaterThanF, GreaterThanEqualF")
-- The Description Compiler ensuers we have only PBool Value types and that both f1 and f2 are functional.
interpret (BQD _ (BinaryCombinator g) f1 f2 _ _ _) a | isJust $ elemIndex g [AndF, OrF] = ArrayT do
  -- Both are singleton arrays, or empty.
  (fr1 :: Array DependencyPath) <- runArrayT (interpret f1 a)
  fr2 <- runArrayT (interpret f2 a)
  case head fr1, head fr2 of
    Just fr1h, Just fr2h -> do
      bfunction <- if g == AndF then pure (&&) else pure (||)
      pure [unsafePartial applyValueFunction (functionOnBooleans bfunction) fr1h fr2h]
    _, _ -> pure []

-- The Description Compiler ensures we have only Value types on which these operations are valid and that both f1 and f2 are functional.
interpret (BQD _ (BinaryCombinator g) f1 f2 ran _ _) a | isJust $ elemIndex g [AddF, SubtractF, DivideF, MultiplyF] = ArrayT do
  -- Both are singleton arrays, or empty.
  (fr1 :: Array DependencyPath) <- runArrayT (interpret f1 a)
  fr2 <- runArrayT (interpret f1 a)
  case head fr1, head fr2 of
    Just fr1h, Just fr2h -> do
      (result :: Array String) <- (performNumericOperation'
        g
        ran
        (unsafePartial mapNumericOperator g ran)
        [(unwrap $ unsafePartial dependencyToValue $ _.head fr1h)]
        [(unwrap $ unsafePartial dependencyToValue $ _.head fr2h)]
        )
      pure [{ head: (V (show g) (Value $ unsafePartial fromJust $ head result))
        , mainPath: Nothing
        , supportingPaths: (allPaths fr1h) `union` (allPaths fr2h)
        }]
    _, _ -> pure []

-----------------------------------------------------------
-- MQD
-----------------------------------------------------------
interpret (MQD dom fun args ran _ _) a = do
  functionName <- case fun of
    (ExternalCoreRoleGetter f) -> pure f
    (ExternalCorePropertyGetter f) -> pure f
    otherwise -> throwError (error $ "Unknown function construction: " <> show fun)
  (f :: HiddenFunction) <- pure $ unsafePartial $ fromJust $ lookupHiddenFunction functionName
  (argValues :: Array DependencyPath) <- traverse (flip interpret a) args
  case unsafePartial $ fromJust $ lookupHiddenFunctionNArgs functionName of
    0 -> do
      r <- (unsafe1argFunction f) (toString a)
      pure a {head = domain2Dependency ran r}
    1 -> do
      r <- (unsafe2argFunction f)
        [ ( toString (first argValues))]
        (toString a)
      pure a {head = domain2Dependency ran r}
    2 -> do
      r <- (unsafe3argFunction f)
        [ ( toString (first argValues))]
        [ ( toString (second argValues))]
        (toString a)
      pure a {head = domain2Dependency ran r}
    3 -> do
      r <- (unsafe4argFunction f)
        [ ( toString (first argValues))]
        [ ( toString (second argValues))]
        [ ( toString (third argValues))]
        (toString a)
      pure a {head = domain2Dependency ran r}
    4 -> do
      r <- (unsafe5argFunction f)
        [ ( toString (first argValues))]
        [ ( toString (second argValues))]
        [ ( toString (third argValues))]
        [ ( toString (fourth argValues))]
        (toString a)
      pure a {head = domain2Dependency ran r}
    otherwise -> throwError (error "Too many arguments for external core module: maximum is 4")

-----------------------------------------------------------
-- SQD
-----------------------------------------------------------
interpret (SQD _ (DataTypeGetter IdentityF) _ _ _) a = pure a

interpret (SQD dom (DataTypeGetter ModelNameF) _ _ _) a = do
  result <- unsafePartial case dom, a.head of
    RDOM _, R rid  -> roleModelName rid
    CDOM _, C cid -> contextModelName cid
    VDOM _ (Just pt), V _ _ -> pure $ Value $ propertytype2string pt
    ContextKind, CT ct -> unsafeCoerce contextTypeModelName' ct
    RoleKind, RT rt ->  unsafeCoerce roleTypeModelName' rt
  pure $ a {head = V "ModelNameF" result}

interpret (SQD dom (Constant range value) _ _ _) a = pure a {head = V "ConstantF" (Value value)}

interpret (SQD dom (RoleIndividual individual) _ _ _) a = ArrayT do
  mi <- lift $ lookupIndexedRole (unwrap individual)
  case mi of
    Nothing -> pure [a]
    Just i -> pure $ [consOnMainPath (R i) a]

interpret (SQD dom (ContextIndividual (ContextInstance individual)) _ _ _) a = ArrayT do
  mi <- lift $ lookupIndexedContext individual
  case mi of
    Nothing -> pure [a]
    Just i -> pure $ [consOnMainPath (C i) a]

interpret (SQD dom (VariableLookup varName) range _ _) a = do
  -- the lookup function ignores its second argument.
  r <- lookup varName "ignore"
  -- We do not need to record the dependencies of the computation behind the variable,
  -- as we recorded them before actually storing the result in the variable.
  pure a {head = domain2Dependency range r}

interpret qfd a = case a.head of
    -----------------------------------------------------------
    -- ContextInstance
    -----------------------------------------------------------
    (C cid) -> case qfd of
      (SQD _ (RolGetter (ENR (EnumeratedRoleType r))) _ _ _) -> if
        isExternalRole r
          then (flip consOnMainPath a) <<< R <$> externalRole cid
          else (flip consOnMainPath a) <<< R <$> getEnumeratedRoleInstances (EnumeratedRoleType r) cid
      (SQD _ (RolGetter (CR cr)) _ _ _) -> do
        (ct :: CalculatedRole) <- lift $ lift $ getPerspectType cr
        (lift $ lift $ calculation ct) >>= flip interpret a
      (SQD _ (DataTypeGetter ExternalRoleF) _ _ _) -> (flip consOnMainPath a) <<< R <$> externalRole cid
      (SQD _ (TypeGetter TypeOfContextF) _ _ _) -> (flip consOnMainPath a) <<< CT <$> contextType cid
      (SQD _ (DataTypeGetterWithParameter GetRoleInstancesForContextFromDatabaseF roleTypeName) _ _ _) -> (flip consOnMainPath a) <<< R <$> getUnlinkedRoleInstances (EnumeratedRoleType roleTypeName) cid

      otherwise -> throwError (error $ "(head=ContextInstance) No implementation in Perspectives.Query.Interpreter for " <> show qfd <> " and " <> show cid)

    -----------------------------------------------------------
    -- RoleInstance
    -----------------------------------------------------------
    dep@(R rid) -> case qfd of
      -- TODO. We moeten hier niet de ADT van het domein nemen. We passen immers toe op de dependency. We moeten dus een ADT opstellen voor de dependency!
      (SQD _ (PropertyGetter (ENP (EnumeratedPropertyType pt))) _ _ _) -> getDynamicPropertyGetter pt rid >>= \(leadingDependencies :: DependencyPath) -> do
        -- We apply the property getter to the head of a. This results in a DependencyPath with or without value.
        -- Lets call this result PropResult.
        --  * when with a value, that value is PropResult.head; PropResult.mainPath starts with that value and ends with a.head.
        --  * when without a value, the PropResult.head is the role bearing the property or the bottom of the chain; PropResult.mainPath starts with that role and ends with a.head.
        -- Invariant: PropResult.mainPath ends with a.head, which is the source of the 'propertyquery' we apply to it.
        -- We then would compose a.mainPath with PropResult.mainPath.
        -- The result is a mainPath whose first part is PropResult.mainPath, which is
        -- followed by the a.mainPath. The head is the head of PropResult.
        -- However, this causes a.head to appear twice in the mainPath:
        --  * once as the head of `a` (and thus the first dependency of a.mainPath)
        --  * once as the end of PropResult.mainPath.
        -- Hence, we remove the first dependency from a.mainPath before composing it with PropResult.mainPath.
        reducedMainPath <- case tail <$> a.mainPath of
          Just l@(Cons _ _) -> pure $ Just $ unsafePartial fromJust $ fromList l
          otherwise -> pure Nothing
        pure ({head: a.head, mainPath: reducedMainPath, supportingPaths: a.supportingPaths} #>> leadingDependencies)

      (SQD _ (PropertyGetter (CP pt)) _ _ _) -> do
        (cp :: CalculatedProperty) <- lift2MPQ $ getPerspectType pt
        (lift2MPQ $ PC.calculation cp) >>= flip interpret a
      (SQD _ (DataTypeGetter ContextF) _ _ _) -> (flip consOnMainPath a) <<< C <$> context rid
      (SQD _ (DataTypeGetter BindingF) _ _ _) -> (flip consOnMainPath a) <<< R <$> binding rid
      (SQD _ (GetRoleBindersF roleType contextType) _ _ _ ) -> (flip consOnMainPath a) <<< R <$> getFilledRoles contextType roleType rid

      otherwise -> throwError (error $ "(head=RoleInstance) No implementation in Perspectives.Query.Interpreter for " <> show qfd <> " and " <> show rid)

    -----------------------------------------------------------
    -- Value
    -----------------------------------------------------------
    (V _ val) -> case qfd of

      -- We merely change the type of the head from Value to a RoleInstance.
      (SQD dom (Value2Role _) _ _ _) -> pure a {head = R $ RoleInstance $ unwrap $ unsafePartial dependencyToValue a.head}


      otherwise -> throwError (error $ "(head is Value) No implementation in Perspectives.Query.Interpreter for " <> show qfd <> " and " <> show val)

    -----------------------------------------------------------
    -- ContextKind
    -----------------------------------------------------------
    (CT contextType) -> case qfd of
      (SQD _ (TypeGetter RoleTypesF) _ _ _) -> (flip consOnMainPath a) <<< RT <$> (liftToInstanceLevel allRoleTypesInContext) contextType

      otherwise -> throwError (error $ "(head is ContextKind) No implementation in Perspectives.Query.Interpreter for " <> show qfd <> " and " <> show contextType)
    -----------------------------------------------------------
    -- RoleKind
    -----------------------------------------------------------
    (RT roleType) -> case qfd of

      (SQD _ (DataTypeGetterWithParameter SpecialisesRoleTypeF parameter) _ _ _ ) -> (flip consOnMainPath a) <<< V "SpecialisesRoleType" <$> (liftToInstanceLevel ((flip specialisesRoleType) (ENR $ EnumeratedRoleType parameter)) roleType)

      otherwise -> throwError (error $ "(head is RoleKind) No implementation in Perspectives.Query.Interpreter for " <> show qfd <> " and " <> show roleType)

toBool :: List Dependency -> Boolean
toBool (Cons (V _ (Value s)) _) = s == "true"
toBool _ = false

toString :: DependencyPath -> String
toString {head} = case head of
  (V _ (Value s)) -> s
  (C (ContextInstance c)) -> c
  (R (RoleInstance r)) -> r
  (CT (ContextType r)) -> r
  (RT (ENR (EnumeratedRoleType r))) -> r
  (RT (CR (CalculatedRoleType r))) -> r

-- | From a string that represents either a Calculated or an Enumerated property,
-- | for a given abstract datatype of roles, retrieve the values from a role instance.
-- | Returns a DependencyPath with (R roleId) as the last dependency in the mainPath, (V "SomeProperty" "value") as its head.
getDynamicPropertyGetter :: String -> RoleInstance ~~> DependencyPath
getDynamicPropertyGetter p rid = do
  rt <- roleType rid
  (pt :: PropertyType) <- lift2MPQ $ getProperType p
  allProps <- lift2MPQ $ allLocallyRepresentedProperties (ST rt)
  if (isJust $ elemIndex pt allProps)
    then getterFromPropertyType pt rid
    else f rid

  where
    f :: RoleInstance ~~> DependencyPath
    f roleInstance = do
      (bnd :: Maybe RoleInstance) <- lift2MPQ $ binding_ roleInstance
      case bnd of
        Nothing -> empty
        Just bnd' -> (flip snocOnMainPath (R roleInstance)) <$> getDynamicPropertyGetter p bnd'

-- NOTE. HOW do we handle no results?
-- | From a PropertyType, retrieve or construct a function to get values for that Property from a Role instance.
-- | Returns a List with (R roleId) as its last dependency, (V "SomeProperty" "value") as its first dependency.
getterFromPropertyType :: PropertyType -> RoleInstance ~~> DependencyPath
-- getterFromPropertyType (ENP ep@(EnumeratedPropertyType id)) roleId = (flip consOnMainPath (singletonPath $ R roleId)) <<< V id <$> getProperty ep roleId
getterFromPropertyType (ENP ep@(EnumeratedPropertyType id)) roleId = ArrayT do
  (vals :: Array Value) <- lift (roleId ##= getProperty ep)
  case head vals of
    Nothing -> pure []
    otherwise -> pure ((V id <$> vals) <#> \dep -> consOnMainPath dep (singletonPath $ R roleId))
getterFromPropertyType (CP cp@(CalculatedPropertyType id)) roleId =
  (lift $ lift $ getPerspectType cp) >>=
    lift <<< lift <<< PC.calculation >>=
      \calc -> do
        old <- lift $ lift $ pushFrame
        lift $ lift (addBinding "currentobject" [unwrap roleId])
        r <- interpret calc (singletonPath (R roleId))
        lift $ lift $ restoreFrame old
        pure r


unsafe1argFunction :: HiddenFunction -> String ~~> String
unsafe1argFunction = unsafeCoerce

unsafe2argFunction :: HiddenFunction -> Array String -> String ~~> String
unsafe2argFunction = unsafeCoerce

unsafe3argFunction :: HiddenFunction -> Array String -> Array String -> String ~~> String
unsafe3argFunction = unsafeCoerce

unsafe4argFunction :: HiddenFunction -> Array String -> Array String -> Array String -> String ~~> String
unsafe4argFunction = unsafeCoerce

unsafe5argFunction :: HiddenFunction -> Array String -> Array String -> Array String -> Array String -> String ~~> String
unsafe5argFunction = unsafeCoerce

first :: forall a. Array a -> a
first = flip (unsafePartial unsafeIndex) 0

second :: forall a. Array a -> a
second = flip (unsafePartial unsafeIndex) 1

third :: forall a. Array a -> a
third = flip (unsafePartial unsafeIndex) 2

fourth :: forall a. Array a -> a
fourth = flip (unsafePartial unsafeIndex) 3
