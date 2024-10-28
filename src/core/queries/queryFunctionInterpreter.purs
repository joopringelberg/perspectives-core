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

import Control.Bind (join)
import Control.Monad.AvarMonadAsk (modify)
import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Control.Plus (empty)
import Data.Array (concat, elemIndex, foldl, head, index, length, null, union, unsafeIndex)
import Data.List (List(..))
import Data.List.NonEmpty (fromList, singleton, tail)
import Data.List.Types (List, NonEmptyList)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (unwrap)
import Data.Traversable (for_, maximum, minimum, traverse)
import Effect.Exception (error)
import Foreign.Object (empty, lookup) as OBJ
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (rol_binding, rol_id, rol_pspType)
import Perspectives.CoreTypes (type (~~>), ArrayWithoutDoubles(..), InformedAssumption(..), MP, MPQ, AssumptionTracking, liftToInstanceLevel, (##=), (##>>))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.Error.Boundaries (handlePerspectRolError')
import Perspectives.External.HiddenFunctionCache (lookupHiddenFunction, lookupHiddenFunctionNArgs)
import Perspectives.HiddenFunction (HiddenFunction)
import Perspectives.Identifiers (isExternalRole)
import Perspectives.InstanceRepresentation (PerspectRol)
import Perspectives.Instances.Combinators (available', not_)
import Perspectives.Instances.Environment (_pushFrame)
import Perspectives.Instances.ObjectGetters (Filled_(..), Filler_(..), binding_, context, contextModelName, contextType, externalRole, filledBy, fills, getAllFilledRoles, getEnumeratedRoleInstances, getFilledRoles, getProperty, getUnlinkedRoleInstances, roleModelName, roleType)
import Perspectives.Instances.Values (bool2Value, parseNumber, value2Date, value2Number)
import Perspectives.ModelDependencies (roleWithId)
import Perspectives.Names (lookupIndexedRole)
import Perspectives.Parsing.Arc.Position (arcParserStartPosition)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (getPerspectRol)
import Perspectives.PerspectivesState (addBinding, getVariableBindings, pushFrame, restoreFrame)
import Perspectives.Query.Interpreter.Dependencies (Dependency(..), DependencyPath, addAsSupportingPaths, allPaths, appendPaths, applyValueFunction, composePaths, consOnMainPath, dependencyToValue, domain2Dependency, functionOnBooleans, functionOnStrings, singletonPath, snocOnMainPath, (#>>))
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), RoleInContext, domain2PropertyRange, domain2roleType, range)
import Perspectives.Query.UnsafeCompiler (lookup, mapDurationOperator, mapNumericOperator, orderFunction, performNumericOperation')
import Perspectives.Representation.ADT (ADT(..), equalsOrSpecialises_)
import Perspectives.Representation.CNF (CNF)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole, getPerspectType)
import Perspectives.Representation.Class.Property (calculation) as PC
import Perspectives.Representation.Class.Property (getPropertyType)
import Perspectives.Representation.Class.Role (allLocallyRepresentedProperties, calculation, toConjunctiveNormalForm_)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.Range (Range(..), isDateOrTime, isPDuration)
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), propertytype2string)
import Perspectives.Types.ObjectGetters (allRoleTypesInContext, contextTypeModelName', propertyAliases, roleTypeModelName', generalisesRoleType)
import Prelude (Unit, bind, discard, eq, flip, map, notEq, pure, show, unit, ($), (&&), (+), (<#>), (<$>), (<<<), (<=), (<>), (==), (>=>), (>>=), (||), void)
import Unsafe.Coerce (unsafeCoerce)

lift2MPQ :: forall a. MP a -> MPQ a
lift2MPQ = lift <<< lift

interpret :: QueryFunctionDescription -> DependencyPath ~~> DependencyPath
interpret qfd = case qfd of
  (UQD _ _ _ _ _ _) -> unsafePartial $ interpretUQD qfd
  (BQD _ _ _ _ _ _ _) -> unsafePartial $ interpretBQD qfd
  (MQD _ _ _ _ _ _) -> unsafePartial $ interpretMQD qfd
  (SQD _ _ _ _ _) -> unsafePartial $ interpretSQD qfd

-----------------------------------------------------------
-- UQD
-----------------------------------------------------------
interpretUQD :: Partial => QueryFunctionDescription -> DependencyPath ~~> DependencyPath
interpretUQD (UQD _ (BindVariable varName) f1 _ _ _) a = ArrayT do
  values <- runArrayT $ interpret f1 a
  lift $ addBinding varName (toString <$> values)
  pure values
interpretUQD (UQD _ WithFrame f1 _ _ _) a = do
  old <- lift2MPQ getVariableBindings
  void $ lift $ lift $ modify \s@{variableBindings} -> s {variableBindings = (_pushFrame old)}
  x <- interpret f1 a
  void $ lift $ lift $ modify \s@{variableBindings} -> s {variableBindings = old}
  pure x
interpretUQD (UQD _ (UnaryCombinator ExistsF) f1 _ _ _) a = ArrayT do
  (r :: Array DependencyPath) <- runArrayT $ interpret f1 a
  pure $ (consOnMainPath (V "ExistsF" $ Value (show $ null r))) <$> r
interpretUQD (UQD _ (UnaryCombinator FilledByF) sourceOfFillerRoles _ _ _) a = do
  (fillerRoleL :: DependencyPath) <- interpret sourceOfFillerRoles a
  -- If the head boundRole is a RoleInstance and (a `bindsRole` head boundRole) is true,
  -- add V (Value "true") to boundRole
  case a.head, fillerRoleL.head of
    (R filledRole), (R fillerRole) -> do
      -- b <- lift $ lift (boundRole ##>> fills bindingRole)
      b <- lift $ lift ((Filled_ filledRole) ##>> filledBy (Filler_ fillerRole))
      if b
        then pure (consOnMainPath (V "FilledByF" (Value "true")) fillerRoleL)
        else pure (consOnMainPath (V "FilledByF" (Value "false")) fillerRoleL)
    _, _ -> pure (consOnMainPath (V "FilledByF" (Value "false")) fillerRoleL)

interpretUQD (UQD _ (UnaryCombinator FillsF) sourceOfFilledRoles _ _ _) a =  do
  (filledRoleL :: DependencyPath) <- interpret sourceOfFilledRoles a
  -- If the head boundRole is a RoleInstance and a `bindsRole` head boundRole is true,
  -- cons V (Value "true") op boundRole
  case a.head, filledRoleL.head of
    (R fillerRole), (R filledRole) -> do
      b <- lift $ lift ((Filled_ filledRole) ##>> fills (Filler_ fillerRole))
      if b
        then pure (consOnMainPath (V "FillsF" (Value "true")) filledRoleL)
        else pure (consOnMainPath (V "FillsF" (Value "false")) filledRoleL)
    _, _ -> pure (consOnMainPath (V "FillsF" (Value "false")) filledRoleL)

interpretUQD (UQD _ (UnaryCombinator AvailableF) f1 _ _ _) a = ArrayT do
  (r :: Array DependencyPath) <- runArrayT $ interpret f1 a
  result <- lift $ available' (toString <$> r)
  pure $ [consOnMainPath (V "AvailableF" (Value $ show result)) a]

interpretUQD (UQD _ (UnaryCombinator ContextIndividualF) contextExpr _ _ _) a = ArrayT do
  (paths :: Array DependencyPath) <- runArrayT $ interpret contextExpr a
  pure (paths <#> \r@{head} -> unsafePartial $ case head of 
    (V _ (Value cid)) -> consOnMainPath (C (ContextInstance cid)) r)

interpretUQD (UQD _ (UnaryCombinator RoleIndividualF) roleExpr _ _ _) a = ArrayT do
  (paths :: Array DependencyPath) <- runArrayT $ interpret roleExpr a
  pure (paths <#> \r@{head} -> unsafePartial $ case head of 
    (V _ (Value rid)) -> consOnMainPath (R (RoleInstance rid)) r)

interpretUQD (UQD _ (UnaryCombinator NotF) f1 _ _ _) a = ArrayT do
  (r :: Array DependencyPath) <- runArrayT $ interpret f1 a
  -- The DescriptionCompiler ensures that we have only Value type heads.
  result <- lift $ not_ (Value <<< toString <$> r)
  pure $ [consOnMainPath (V "NotF" (Value $ show result)) a]

interpretUQD (UQD _ FilterF criterium _ _ _) a = ArrayT do
  (r :: Array DependencyPath) <- runArrayT $ interpret criterium a
  case head r of 
    Just l -> unsafePartial $ case l.head of
      (V _ (Value "true")) -> pure $ [appendPaths a l]
      (V _ (Value "false")) -> pure []
    _ -> pure []

-----------------------------------------------------------
-- BQD
-----------------------------------------------------------
interpretBQD :: Partial => QueryFunctionDescription -> DependencyPath ~~> DependencyPath
interpretBQD (BQD _ (BinaryCombinator ComposeF) f1 f2@(SQD _ (Constant _ _) _ _ _) _ _ _) a = interpret f2 a
interpretBQD (BQD _ (BinaryCombinator ComposeF) f1 f2 _ _ _) a =
  case f1, f2 of
    (SQD _ (DataTypeGetter IdentityF) _ _ _), (SQD _ (DataTypeGetter IdentityF) _ _ _) -> pure a
    _, (SQD _ (DataTypeGetter IdentityF) _ _ _) -> interpret f1 a
    (SQD _ (DataTypeGetter IdentityF) _ _ _), _ -> interpret f2 a
    _, _ -> (interpret f1 >=> interpret f2) a
interpretBQD (BQD _ (BinaryCombinator SequenceF) f1 f2 _ _ _) a = ArrayT $ do
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

interpretBQD (BQD _ (BinaryCombinator IntersectionF) f1 f2 _ _ _) a = ArrayT do
  (f1r :: Array DependencyPath) <- runArrayT $ interpret f1 a
  if null f1r
    then do
      f2r <- runArrayT $ interpret f2 a
      pure $ f1r `union` f2r
    else pure f1r

interpretBQD (BQD _ (BinaryCombinator UnionF) f1 f2 _ _ _) a = ArrayT do
  (l :: Array DependencyPath) <- runArrayT $ interpret f1 a
  (r :: Array DependencyPath) <- runArrayT $ interpret f2 a
  pure (l `union` r)

interpretBQD (BQD _ (BinaryCombinator FilledByF) sourceOfFilledRoles sourceOfFillerRoles _ _ _) a = ArrayT do
  (filledRoles :: Array DependencyPath) <- runArrayT $ interpret sourceOfFilledRoles a
  (fillerRoles :: Array DependencyPath) <- runArrayT $ interpret sourceOfFillerRoles a
  -- filledRoles and fillerRoles must be functional.
  case head filledRoles, head fillerRoles of
    Just filledRolesh, Just fillerRolesh | length filledRoles <= 1 && length fillerRoles <= 1 ->
      case filledRolesh.head, fillerRolesh.head of
        R filled, R filler -> do
          result <- lift ((Filled_ filled) ##>> (filledBy (Filler_ filler)))
          pure [{ head: (V "" (Value $ show result))
                , mainPath: Nothing
                , supportingPaths: allPaths filledRolesh `union` allPaths fillerRolesh
                }]
        _, _ -> throwError (error $ "'binds' expects two roles, but got: " <> show filledRolesh.head <> " and " <> show fillerRolesh.head)
    Just filledRolesh, Just fillerRolesh -> throwError (error $ "'binds' expects at most a single role instance on both the left and right side")
    _, _ -> pure [{ head: (V "" (Value "false"))
                , mainPath: Nothing
                , supportingPaths: []
                }]

interpretBQD (BQD _(BinaryCombinator OrElseF) f1 f2 ran fun man) a = ArrayT do
  fr1 <- runArrayT (interpret f1 a)
  if null fr1
    then runArrayT (interpret f2 a)
    else pure fr1

interpretBQD (BQD _ (BinaryCombinator ComposeSequenceF) f1 f2 ran _ _) a = ArrayT
  case f2 of
    -- f2 results from the expression that follows `>>=` (must have been: "sum", "product", etc.).
    -- This was parsed as `SequenceFunction f` and is now compiled as `UnaryCombinator f` in an SQD.
    -- Notice by the domain and range that we assume functions that are Monoids.
    -- Notice the strangeness of compiling a binary expression into an SQD description.
    SQD dom (UnaryCombinator fname) _ _ _-> do 
      case fname of
        -- we can count anything and the result is a number.
        CountF -> do
          (result :: Array DependencyPath) <- runArrayT (interpret f1 a)
          -- Now count the results in a and return that. We add the Dependency that holds the count to the path and it becomes the new head.
          -- For a sequence function, there is no single path that leads to the result. We consider the result to be the main path.
          -- All other paths (both main and supporting) of the result paths are combined in the supporting paths.
          pure [{ head: (V (show fname) (Value $ show $ length result))
                , mainPath: Just $ singleton (V (show fname) (Value $ show $ length result))
                , supportingPaths: concat (allPaths <$> result)
                }]    
        -- We can also try to take the first element of a sequence of whatever type.
        FirstF -> do
          (result :: Array DependencyPath) <- runArrayT (interpret f1 a)
          case head result of
            Nothing -> pure []
            Just h -> pure [h]
        _ -> case ran of
          (VDOM rangeType _) -> 
            case fname of
              AddF -> do
                (result :: Array DependencyPath) <- runArrayT (interpret f1 a)
                -- We can safely assume the heads of the paths can be added up, but we do have to know whether they are Integers or Strings.
                unsafePartial $ case rangeType of
                  PNumber -> addNumbers fname result
                  PDuration _ -> addNumbers fname result
                  PString -> do
                    (strs :: Array String) <- pure $ (unwrap <<< dependencyToValue <<< _.head) <$> result
                    sum <- pure (foldl (\cumulator nr -> cumulator <> nr) "" strs)
                    pure [{ head: (V (show fname) (Value $ show sum))
                          , mainPath: Just $ singleton (V (show fname) (Value $ show $ length result))
                          , supportingPaths: concat (allPaths <$> result)
                          }]
              
              MaximumF -> do
                (result :: Array DependencyPath) <- runArrayT (interpret f1 a)
                unsafePartial case rangeType of 
                  PNumber -> largestNumber fname result
                  PDuration _ -> largestNumber fname result
                  PString -> do
                    (strs :: Array String) <- pure $ (unwrap <<< dependencyToValue <<< _.head) <$> result
                    pure [{ head: (V (show fname) (Value $ show (maximum strs)))
                          , mainPath: Just $ singleton (V (show fname) (Value $ show (maximum strs)))
                          , supportingPaths: concat (allPaths <$> result)
                          }]

              MinimumF -> do
                (result :: Array DependencyPath) <- runArrayT (interpret f1 a)
                unsafePartial case rangeType of 
                  PNumber -> smallestNumber fname result
                  PDuration _ -> smallestNumber fname result
                  PString -> do
                    (strs :: Array String) <- pure $ (unwrap <<< dependencyToValue <<< _.head) <$> result
                    pure [{ head: (V (show fname) (Value $ show (minimum strs)))
                          , mainPath: Just $ singleton (V (show fname) (Value $ show (minimum strs)))
                          , supportingPaths: concat (allPaths <$> result)
                          }]

              _ -> throwError (error $ show $ ArgumentMustBeSequenceFunction arcParserStartPosition)
          _ -> throwError $ (error $ show $ ArgumentMustBeSequenceFunction arcParserStartPosition)
    _ -> throwError $ (error $ show $ ArgumentMustBeSequenceFunction arcParserStartPosition)
  where
    addNumbers :: FunctionName -> Array DependencyPath -> AssumptionTracking (Array DependencyPath)
    addNumbers fname result = do
      (nrs :: Array Number) <- traverse (parseNumber <<< unwrap <<< dependencyToValue <<< _.head) result
      sum <- pure (foldl (\cumulator nr -> cumulator + nr) 0.0 nrs)
      pure [{ head: (V (show fname) (Value $ show sum))
            , mainPath: Just $ singleton (V (show fname) (Value $ show $ length result))
            , supportingPaths: concat (allPaths <$> result)
            }]
    smallestNumber :: FunctionName -> Array DependencyPath -> AssumptionTracking (Array DependencyPath)
    smallestNumber fname result = do
      (nrs :: Array Number) <- traverse (parseNumber <<< unwrap <<< dependencyToValue <<< _.head) result
      pure [{ head: (V (show fname) (Value $ show (maximum nrs)))
            , mainPath: Just $ singleton (V (show fname) (Value $ show (maximum nrs)))
            , supportingPaths: concat (allPaths <$> result)
            }]
    largestNumber :: FunctionName -> Array DependencyPath -> AssumptionTracking (Array DependencyPath)
    largestNumber fname result = do
      (nrs :: Array Number) <- traverse (parseNumber <<< unwrap <<< dependencyToValue <<< _.head) result
      pure [{ head: (V (show fname) (Value $ show (maximum nrs)))
            , mainPath: Just $ singleton (V (show fname) (Value $ show (minimum nrs)))
            , supportingPaths: concat (allPaths <$> result)
            }]

interpretBQD (BQD _ (BinaryCombinator fun) f1 f2 ran _ _) a = case fun of 

  -- The compiler only allows f1 and f2 if they're functional.
  g | isJust $ elemIndex g [EqualsF, NotEqualsF] -> ArrayT do
    -- Both are singleton arrays, or empty.
    (fr1 :: Array DependencyPath) <- runArrayT (interpret f1 a)
    fr2 <- runArrayT (interpret f2 a)
    unsafePartial $ case g of
      EqualsF -> case head fr1, head fr2 of
        Just fr1h, Just fr2h -> pure [unsafePartial applyValueFunction (functionOnStrings \x y -> show (x == y)) fr1h fr2h]
        _, _ -> pure []
      NotEqualsF -> case head fr1, head fr2 of
        Just fr1h, Just fr2h -> pure [unsafePartial applyValueFunction (functionOnStrings \x y -> show (x `notEq` y)) fr1h fr2h]
        _, _ -> pure []

  -- The Description Compiler makes sure we only have Value types that can be ordered, here.
  -- It also ensures the f1 and f2 are functional.
  g | isJust $ elemIndex g [LessThanF, LessThanEqualF, GreaterThanF, GreaterThanEqualF] -> ArrayT do
    -- Both are singleton arrays, or empty.
    (fr1 :: Array DependencyPath) <- runArrayT (interpret f1 a)
    fr2 <- runArrayT (interpret f2 a)
    case head fr1, head fr2 of
      Just fr1h, Just fr2h -> unsafePartial $ case ran of
        VDOM PString _ -> pure [unsafePartial applyValueFunction (functionOnStrings \x y -> show $ (orderFunction g) x y) fr1h fr2h]
        VDOM PBool _ -> pure [unsafePartial applyValueFunction (functionOnBooleans (orderFunction g)) fr1h fr2h]
        VDOM PNumber _ -> pure [unsafePartial $ applyValueFunction (\x y -> bool2Value ((orderFunction g) (value2Number x) (value2Number y))) fr1h fr2h]
        VDOM PDate _ -> pure [unsafePartial applyValueFunction (\x y -> bool2Value ((orderFunction g) (value2Date x) (value2Date y))) fr1h fr2h] 
        VDOM PEmail _ -> pure [unsafePartial applyValueFunction (functionOnStrings \x y -> show $ (orderFunction g) x y) fr1h fr2h]
      _, _ -> pure []

  -- The Description Compiler ensuers we have only PBool Value types and that both f1 and f2 are functional.
  g | isJust $ elemIndex g [AndF, OrF] -> ArrayT do
      -- Both are singleton arrays, or empty.
      (fr1 :: Array DependencyPath) <- runArrayT (interpret f1 a)
      fr2 <- runArrayT (interpret f2 a)
      case head fr1, head fr2 of
        Just fr1h, Just fr2h -> do
          bfunction <- if g == AndF then pure (&&) else pure (||)
          pure [unsafePartial applyValueFunction (functionOnBooleans bfunction) fr1h fr2h]
        _, _ -> pure []

  -- The Description Compiler ensures we have only Value types on which these operations are valid and that both f1 and f2 are functional.
  g | isJust $ elemIndex g [AddF, SubtractF, DivideF, MultiplyF] -> ArrayT do
    -- Both are singleton arrays, or empty.
    (fr1 :: Array DependencyPath) <- runArrayT (interpret f1 a)
    fr2 <- runArrayT (interpret f2 a)
    case head fr1, head fr2 of
      Just fr1h, Just fr2h -> if range f1 `eq` range f2
        then do
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
        else if isDateOrTime (unsafePartial domain2PropertyRange $ range f1) && isPDuration (unsafePartial domain2PropertyRange $ range f2)
          then do 
            (result :: Array String) <- (performNumericOperation'
              g
              ran
              (unsafePartial mapDurationOperator g (range f2))
              [(unwrap $ unsafePartial dependencyToValue $ _.head fr1h)]
              [(unwrap $ unsafePartial dependencyToValue $ _.head fr2h)]
              )
            pure [{ head: (V (show g) (Value $ unsafePartial fromJust $ head result))
              , mainPath: Nothing
              , supportingPaths: (allPaths fr1h) `union` (allPaths fr2h)
              }]
          else do 
            (result :: Array String) <- (performNumericOperation'
              g
              ran
              (unsafePartial mapDurationOperator g (range f1))
              [(unwrap $ unsafePartial dependencyToValue $ _.head fr2h)]
              [(unwrap $ unsafePartial dependencyToValue $ _.head fr1h)]
              )
            pure [{ head: (V (show g) (Value $ unsafePartial fromJust $ head result))
              , mainPath: Nothing
              , supportingPaths: (allPaths fr1h) `union` (allPaths fr2h)
              }]
      Just fr1h, Nothing -> pure fr1
      Nothing, Just fr2h -> pure fr2
      Nothing, Nothing -> pure [a]


  _ -> ArrayT $ pure []

-----------------------------------------------------------
-- MQD
-----------------------------------------------------------
interpretMQD :: Partial => QueryFunctionDescription -> DependencyPath ~~> DependencyPath
interpretMQD (MQD dom fun args ran _ _) a = do
  functionName <- case fun of
    (ExternalCoreRoleGetter f) -> pure f
    (ExternalCorePropertyGetter f) -> pure f
    otherwise -> throwError (error $ "Unknown function construction: " <> show fun)
  (f :: HiddenFunction) <- pure $ unsafePartial $ fromJust $ lookupHiddenFunction functionName
  (argValues :: Array DependencyPath) <- traverse (flip interpret a) args
  (nrOfParameters :: Int) <- pure $ unsafePartial (fromJust $ lookupHiddenFunctionNArgs functionName)
  -- Notice that the number of parameters given ignores the default argument (context or role) that the function is applied to anyway.
  -- If we do have an extra argument value, supply it as the last argument instead of r.
  (lastArgument :: String) <- case index argValues nrOfParameters of
    Nothing -> pure $ toString a
    Just v -> pure $ toString v
  case unsafePartial $ fromJust $ lookupHiddenFunctionNArgs functionName of
    0 -> do
      r <- (unsafe1argFunction f) lastArgument
      pure a {head = domain2Dependency ran r}
    1 -> do
      r <- (unsafe2argFunction f)
        [ ( toString (first argValues))]
        lastArgument
      pure a {head = domain2Dependency ran r}
    2 -> do
      r <- (unsafe3argFunction f)
        [ ( toString (first argValues))]
        [ ( toString (second argValues))]
        lastArgument
      pure a {head = domain2Dependency ran r}
    3 -> do
      r <- (unsafe4argFunction f)
        [ ( toString (first argValues))]
        [ ( toString (second argValues))]
        [ ( toString (third argValues))]
        lastArgument
      pure a {head = domain2Dependency ran r}
    4 -> do
      r <- (unsafe5argFunction f)
        [ ( toString (first argValues))]
        [ ( toString (second argValues))]
        [ ( toString (third argValues))]
        [ ( toString (fourth argValues))]
        lastArgument
      pure a {head = domain2Dependency ran r}
    5 -> do
      r <- (unsafe6argFunction f)
        [ ( toString (first argValues))]
        [ ( toString (second argValues))]
        [ ( toString (third argValues))]
        [ ( toString (fourth argValues))]
        [ ( toString (fifth argValues))]
        lastArgument
      pure a {head = domain2Dependency ran r}
    otherwise -> throwError (error "Too many arguments for external core module: maximum is 4")

-----------------------------------------------------------
-- SQD
-----------------------------------------------------------
interpretSQD :: Partial => QueryFunctionDescription -> DependencyPath ~~> DependencyPath
interpretSQD (SQD _ (DataTypeGetter IdentityF) _ _ _) a = pure a

interpretSQD (SQD dom (DataTypeGetter ModelNameF) _ _ _) a = do
  result <- unsafePartial case dom, a.head of
    RDOM _, R rid  -> roleModelName rid
    CDOM _, C cid -> contextModelName cid
    VDOM _ (Just pt), V _ _ -> pure $ Value $ propertytype2string pt
    ContextKind, CT ct -> unsafeCoerce contextTypeModelName' ct
    RoleKind, RT rt ->  unsafeCoerce roleTypeModelName' rt
  pure $ a {head = V "ModelNameF" result}

interpretSQD (SQD dom (Constant range value) _ _ _) a = pure a {head = V "ConstantF" (Value value)}

interpretSQD (SQD dom (RoleIndividual individual) _ _ _) a = ArrayT do
  mi <- lift $ lookupIndexedRole (unwrap individual)
  case mi of
    Nothing -> pure [a]
    Just i -> pure $ [consOnMainPath (R i) a]
-- An indexed individual is private to a PerspectivesUser. As we use this interpreter to calculate what to 
-- serialise, we should stop here. The receiving users will substitute whatever role instance that is this
-- indexed individual to them. We need not, indeed cannot, send them that.
-- interpretSQD (SQD dom (RoleIndividual individual) _ _ _) a = ArrayT $ pure [a]
interpretSQD (SQD dom (ContextIndividual (ContextInstance individual)) _ _ _) a = ArrayT $ pure []

interpretSQD (SQD dom (VariableLookup varName) range _ _) a = do
  -- the lookup function ignores its second argument.
  r <- lookup varName "ignore"
  -- We do not need to record the dependencies of the computation behind the variable,
  -- as we recorded them before actually storing the result in the variable.
  pure a {head = domain2Dependency range r}

interpretSQD qfd a = case a.head of
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
      (SQD _ (DataTypeGetter FillerF) ran _ _) -> composePaths a <$> getFillerTypeRecursively (unsafePartial domain2roleType ran) rid
      (SQD _ (DataTypeGetterWithParameter FillerF _) ran _ _) -> composePaths a <$> getFillerTypeRecursively (unsafePartial domain2roleType ran) rid
      (SQD _ (FilledF roleType contextType) _ _ _ ) -> composePaths a <$> getRecursivelyFilledRoles contextType roleType rid

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

      (SQD _ (DataTypeGetterWithParameter SpecialisesRoleTypeF parameter) _ _ _ ) -> (flip consOnMainPath a) <<< V "SpecialisesRoleType" <$> (liftToInstanceLevel ((flip generalisesRoleType) (ENR $ EnumeratedRoleType parameter)) roleType)

      otherwise -> throwError (error $ "(head is RoleKind) No implementation in Perspectives.Query.Interpreter for " <> show qfd <> " and " <> show roleType)

-- NOTE: in contrast with Perspectives.Instances.ObjectGetters.getRecursivelyFilledRoles', this function returns
-- no result if filled role is found.
getRecursivelyFilledRoles :: ContextType -> EnumeratedRoleType -> (RoleInstance ~~> DependencyPath)
getRecursivelyFilledRoles filledContextType filledType fillerId = ArrayT $ execWriterT $ depthFirst fillerId (singletonPath (R fillerId))
  where
    depthFirst :: RoleInstance -> DependencyPath ->  WriterT (Array DependencyPath) AssumptionTracking Unit
    depthFirst rid depPath = do
      r <- lift $ runArrayT $ getFilledRoles filledContextType filledType rid
      if null r
        then do 
          allFilleds <- lift $ lift $ getAllFilledRoles rid
          for_ allFilleds \filled -> depthFirst filled (consOnMainPath (R filled) depPath)
        else for_ r \filled -> tell [consOnMainPath (R filled) depPath]


-- | For each Role dependency, push a Role assumption.
pushAssumptionsForDependencyPath :: Partial => DependencyPath -> AssumptionTracking Unit
pushAssumptionsForDependencyPath dp = for_ (allPaths dp) 
  (\path -> for_ path 
    (\a -> case a of 
      R rid -> do
        cType <- lift (rid ##>> (context >=> contextType))
        rType <- lift (rid ##>> roleType)
        tell $ ArrayWithoutDoubles[FilledRolesAssumption rid cType rType]
      _ -> pure unit))

getFillerTypeRecursively :: ADT RoleInContext -> RoleInstance ~~> DependencyPath
getFillerTypeRecursively adt r = do 
  adtCnf <- lift $ lift $ toConjunctiveNormalForm_ adt
  ArrayT $ (lift $ try $ getPerspectRol r) >>=
    handlePerspectRolError' "getFillerTypeRecursively" [] (depthFirst adtCnf)
    where
    depthFirst :: CNF RoleInContext -> PerspectRol -> AssumptionTracking (Array DependencyPath)
    depthFirst adtCnf role =
        case rol_binding role of
          Nothing -> pure []
          Just b -> do
            bRole <- lift $ getPerspectRol b
            roleCnf <- lift (getEnumeratedRole (rol_pspType bRole) >>= pure <<< _.completeType <<< unwrap)
            if roleCnf `equalsOrSpecialises_` adtCnf
              then pure [snocOnMainPath (singletonPath (R b)) (R $ rol_id role)]
              else map (flip snocOnMainPath (R $ rol_id role)) <$> depthFirst adtCnf bRole

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
  (rt :: EnumeratedRoleType) <- roleType rid
  (pt :: PropertyType) <- lift2MPQ $ getPropertyType p
  case pt of 
    CP _ -> do 
      allProps <- lift2MPQ $ allLocallyRepresentedProperties (ST rt)
      -- Special case for the 'property of last resort' that is inserted in serialised perspectives for roles without properties.
      if (isJust $ elemIndex pt allProps) || pt == (CP $ CalculatedPropertyType roleWithId)
        then getterFromPropertyType pt rid
        else getItFromTheFiller rid
    ENP eprop -> getPropertyFromTelescope rt eprop

  where
    getPropertyFromTelescope :: EnumeratedRoleType -> EnumeratedPropertyType ~~> DependencyPath
    getPropertyFromTelescope roleType eprop = do 
      allProps <- lift2MPQ $ allLocallyRepresentedProperties (ST roleType)
      if isJust $ elemIndex (ENP eprop) allProps
        then ArrayT do 
          r <- runArrayT $ getterFromPropertyType (ENP eprop) rid
          if null r
            -- Even though the property is represented on this role, we choose to search the rest of the chain.
            then runArrayT $ getItFromTheFiller rid
            else pure r
        else do 
          -- We must take aliases of the actual role type into account.
          aliases <- catchError (lift $ lift $ propertyAliases roleType)
            \e -> pure OBJ.empty
          case OBJ.lookup (unwrap eprop) aliases of
            Just destination -> ArrayT do 
              r <- runArrayT $ getterFromPropertyType (ENP destination) rid
              if null r
                then runArrayT $ getItFromTheFiller rid
                else pure r
            Nothing -> getItFromTheFiller rid

    getItFromTheFiller :: RoleInstance ~~> DependencyPath
    getItFromTheFiller roleInstance = do
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

unsafe6argFunction :: HiddenFunction -> Array String -> Array String -> Array String -> Array String -> Array String -> String ~~> String
unsafe6argFunction = unsafeCoerce

first :: forall a. Array a -> a
first = flip (unsafePartial unsafeIndex) 0

second :: forall a. Array a -> a
second = flip (unsafePartial unsafeIndex) 1

third :: forall a. Array a -> a
third = flip (unsafePartial unsafeIndex) 2

fourth :: forall a. Array a -> a
fourth = flip (unsafePartial unsafeIndex) 3

fifth :: forall a. Array a -> a
fifth = flip (unsafePartial unsafeIndex) 4

sixth :: forall a. Array a -> a
sixth = flip (unsafePartial unsafeIndex) 5
