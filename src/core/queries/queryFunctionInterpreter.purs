-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

module Perspectives.Query.Interpreter where

import Control.Monad.AvarMonadAsk (modify)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Control.MonadZero (empty, guard, void)
import Data.Array (elemIndex, null, union, unsafeIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.List (List(..), head, singleton, snoc, uncons)
import Data.List.Types (List)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~>), MP, MPQ, (##>>))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.External.HiddenFunctionCache (lookupHiddenFunction, lookupHiddenFunctionNArgs)
import Perspectives.HiddenFunction (HiddenFunction)
import Perspectives.Identifiers (isExternalRole)
import Perspectives.Instances.Environment (_pushFrame)
import Perspectives.Instances.ObjectGetters (binding, binding_, bindsRole, boundByRole, context, contextType, externalRole, getEnumeratedRoleInstances, getProperty, getRoleBinders, roleType_)
import Perspectives.PerspectivesState (addBinding, getVariableBindings, pushFrame, restoreFrame)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.PersistentType (getPerspectType)
import Perspectives.Representation.Class.Property (calculation) as PC
import Perspectives.Representation.Class.Property (getProperType)
import Perspectives.Representation.Class.Role (allLocallyRepresentedProperties, calculation)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Prelude (class Eq, class Show, bind, discard, flip, pure, show, ($), (<$>), (<<<), (<>), (==), (>=>), (>>=))
import Unsafe.Coerce (unsafeCoerce)

data Dependency = C ContextInstance | R RoleInstance | V String Value | CT ContextType | RT RoleType

derive instance genericDependency :: Generic Dependency _
instance eqDependency :: Eq Dependency where
  eq = genericEq

instance showDependency :: Show Dependency where
  show (C cid) = "C " <> show cid
  show (R rid) = "R " <> show rid
  show (V ptype val) = "V " <> show ptype <> " " <> show val
  show (CT ctype) = "CT " <> show ctype
  show (RT rtype) = "RT " <> show rtype

lift2MPQ :: forall a. MP a -> MPQ a
lift2MPQ = lift <<< lift

interpret :: QueryFunctionDescription -> List Dependency ~~> (List Dependency)

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
  (r :: Array (List Dependency)) <- runArrayT $ interpret f1 a
  pure $ (Cons (V "ExistsF" $ Value (show $ null r))) <$> r
interpret (UQD _ (UnaryCombinator BindsF) f1 _ _ _) a = do
  (boundRoleL :: List Dependency) <- interpret f1 a
  -- If the head boundRole is a RoleInstance and a `bindsRole` head boundRole is true,
  -- cons V (Value "true") op boundRole
  case a, boundRoleL of
    (Cons (R bindingRole) _), (Cons (R boundRole) _) -> do
      b <- lift $ lift (boundRole ##>> bindsRole bindingRole)
      if b
        then pure (Cons (V "BindsF" (Value "true")) boundRoleL)
        else pure (Cons (V "BindsF" (Value "false")) boundRoleL)
    _, Nil -> pure (Cons (V "BindsF" (Value "false")) boundRoleL)
    _, _ -> throwError (error $ "Perspectives.Query.Interpreter: wrong argument types for BindsF")
interpret (UQD _ (UnaryCombinator BoundByF) f1 _ _ _) a =  do
  (boundRoleL :: List Dependency) <- interpret f1 a
  -- If the head boundRole is a RoleInstance and a `bindsRole` head boundRole is true,
  -- cons V (Value "true") op boundRole
  case a, boundRoleL of
    (Cons (R bindingRole) _), (Cons (R boundRole) _) -> do
      b <- lift $ lift (boundRole ##>> boundByRole bindingRole)
      if b
        then pure (Cons (V "BoundByF" (Value "true")) boundRoleL)
        else pure (Cons (V "BoundByF" (Value "false")) boundRoleL)
    _, Nil -> pure (Cons (V "BoundByF" (Value "false")) boundRoleL)
    _, _ -> throwError (error $ "Perspectives.Query.Interpreter: wrong argument types for BoundByF")

-- TODO
interpret (UQD _ (UnaryCombinator AvailableF) f1 _ _ _) a = throwError (error $ "Perspectives.Query.Interpreter: no implementation for AvailableF")
interpret (UQD _ (UnaryCombinator NotF) f1 _ _ _) a = throwError (error $ "Perspectives.Query.Interpreter: no implementation for NotF")

-----------------------------------------------------------
-- BQD
-----------------------------------------------------------
interpret (BQD _ (BinaryCombinator ComposeF) f1 f2@(SQD _ (Constant _ _) _ _ _) _ _ _) a = interpret f2 a
interpret (BQD _ (BinaryCombinator ComposeF) f1 f2 _ _ _) a =
  (interpret f1 >=> interpret f2) a
interpret (BQD _ (BinaryCombinator FilterF) source criterium _ _ _) a = do
  (value :: List Dependency) <- interpret source a
  (r :: List Dependency) <- interpret criterium value
  guard case head r of
    Just (V "FilterF" (Value "true")) -> true
    otherwise -> false
  -- TODO Also return the dependencies of the criterium!
  pure value
interpret (BQD _ (BinaryCombinator SequenceF) f1 f2 _ _ _) a = interpret f2 a
interpret (BQD _ (BinaryCombinator DisjunctionF) f1 f2 _ _ _) a = ArrayT do
  (l :: Array (List Dependency)) <- runArrayT $ interpret f1 a
  if null l
    then runArrayT $ interpret f2 a
    else pure l
interpret (BQD _ (BinaryCombinator ConjunctionF) f1 f2 _ _ _) a = ArrayT do
  (l :: Array (List Dependency)) <- runArrayT $ interpret f1 a
  (r :: Array (List Dependency)) <- runArrayT $ interpret f2 a
  pure (l `union` r)

-- TODO
interpret (BQD _ (BinaryCombinator g) f1 f2 _ _ _) a | isJust $ elemIndex g [EqualsF, NotEqualsF] = throwError (error $ "Perspectives.Query.Interpreter: no implementation for EqualsF, NotEqualsF")
interpret (BQD _ (BinaryCombinator g) f1 f2 _ _ _) a | isJust $ elemIndex g [LessThanF, LessThanEqualF, GreaterThanF, GreaterThanEqualF] = throwError (error $ "Perspectives.Query.Interpreter: no implementation for LessThanF, LessThanEqualF, GreaterThanF, GreaterThanEqualF")
interpret (BQD _ (BinaryCombinator g) f1 f2 _ _ _) a | isJust $ elemIndex g [AndF, OrF] = throwError (error $ "Perspectives.Query.Interpreter: no implementation for EqualsF, AndF, OrF")
interpret (BQD _ (BinaryCombinator g) f1 f2 ran _ _) a | isJust $ elemIndex g [AddF, SubtractF, DivideF, MultiplyF] = throwError (error $ "Perspectives.Query.Interpreter: no implementation for AddF, SubtractF, DivideF, MultiplyF")

-----------------------------------------------------------
-- MQD
-----------------------------------------------------------
interpret (MQD dom fun args _ _ _) a = do
  functionName <- case fun of
    (ExternalCoreRoleGetter f) -> pure f
    (ExternalCorePropertyGetter f) -> pure f
    otherwise -> throwError (error $ "Unknown function construction: " <> show fun)
  (f :: HiddenFunction) <- pure $ unsafeCoerce $ unsafePartial $ fromJust $ lookupHiddenFunction functionName
  (argValues :: Array (List Dependency)) <- traverse (flip interpret a) args
  -- TODO. Hieronder worden weliswaar de resultaten berekend, maar die worden
  -- niet als List Dependency teruggegeven!
  -- Tevens moeten we uit de argValues wel de echte argumenten extraheren!
  -- Bovendien moeten de resultaten uit de argument berekening ook teruggegeven worden.
  case unsafePartial $ fromJust $ lookupHiddenFunctionNArgs functionName of
    0 -> (unsafeCoerce f) (toString a)
    1 -> (unsafeCoerce f)
      (unsafePartial ( toString (unsafeIndex argValues 0)))
      (toString a)
    2 -> (unsafeCoerce f)
      (unsafePartial ( toString (unsafeIndex argValues 0)))
      (unsafePartial ( toString (unsafeIndex argValues 1)))
      (toString a)
    3 -> (unsafeCoerce f)
      (unsafePartial ( toString (unsafeIndex argValues 0)))
      (unsafePartial ( toString (unsafeIndex argValues 1)))
      (unsafePartial ( toString (unsafeIndex argValues 2)))
      (toString a)
    4 -> (unsafeCoerce f)
      (unsafePartial ( toString (unsafeIndex argValues 0)))
      (unsafePartial ( toString (unsafeIndex argValues 1)))
      (unsafePartial ( toString (unsafeIndex argValues 2)))
      (unsafePartial ( toString (unsafeIndex argValues 3)))
      (toString a)
    otherwise -> throwError (error "Too many arguments for external core module: maximum is 4")

-----------------------------------------------------------
-- SQD
-----------------------------------------------------------
interpret (SQD _ (DataTypeGetter IdentityF) _ _ _) a = throwError (error $ "No implementation in Perspectives.Query.Interpreter for IdentityF")

interpret (SQD dom Identity _ _ _) a = throwError (error $ "No implementation in Perspectives.Query.Interpreter for Identity")

interpret (SQD dom (DataTypeGetter ModelNameF) _ _ _) a = throwError (error $ "No implementation in Perspectives.Query.Interpreter for ModelNameF")

interpret (SQD dom (Constant range value) _ _ _) a = throwError (error $ "No implementation in Perspectives.Query.Interpreter for RoleTypesF")

interpret (SQD dom (RoleIndividual individual) _ _ _) a = throwError (error $ "No implementation in Perspectives.Query.Interpreter for RoleIndividual")

interpret (SQD dom (VariableLookup varName) range _ _) a = throwError (error $ "No implementation in Perspectives.Query.Interpreter for VariableLookup")

interpret qfd a = case uncons a of
  Nothing -> pure Nil
  Just {head, tail} -> case head of
    -----------------------------------------------------------
    -- ContextInstance
    -----------------------------------------------------------
    (C cid) -> case qfd of
      (SQD _ (RolGetter (ENR (EnumeratedRoleType r))) _ _ _) -> if
        isExternalRole r
          then (flip Cons a) <<< R <$> externalRole cid
          else (flip Cons a) <<< R <$> getEnumeratedRoleInstances (EnumeratedRoleType r) cid
      (SQD _ (RolGetter (CR cr)) _ _ _) -> do
        (ct :: CalculatedRole) <- lift $ lift $ getPerspectType cr
        (lift $ lift $ calculation ct) >>= flip interpret a
      (SQD _ (DataTypeGetter ExternalRoleF) _ _ _) -> (flip Cons a) <<< R <$> externalRole cid
      (SQD _ (TypeGetter TypeOfContextF) _ _ _) -> (flip Cons a) <<< CT <$> contextType cid

      otherwise -> throwError (error $ "No implementation in Perspectives.Query.Interpreter for " <> show qfd)

    -----------------------------------------------------------
    -- RoleInstance
    -----------------------------------------------------------
    dep@(R rid) -> case qfd of
      (SQD (RDOM roleAdt) (PropertyGetter (ENP (EnumeratedPropertyType pt))) _ _ _) -> getDynamicPropertyGetter pt roleAdt rid >>= \leadingDependencies -> do
        -- the leadingDependencies end with (R rid)
        pure (leadingDependencies <> tail)

      (SQD _ (PropertyGetter (CP pt)) _ _ _) -> do
        (cp :: CalculatedProperty) <- lift2MPQ $ getPerspectType pt
        (lift2MPQ $ PC.calculation cp) >>= flip interpret a
      (SQD _ (DataTypeGetter ContextF) _ _ _) -> (flip Cons a) <<< C <$> context rid
      (SQD _ (DataTypeGetter BindingF) _ _ _) -> (flip Cons a) <<< R <$> binding rid
      (SQD _ (DataTypeGetterWithParameter GetRoleBindersF parameter) _ _ _ ) -> (flip Cons a) <<< R <$> getRoleBinders (EnumeratedRoleType parameter) rid

      otherwise -> throwError (error $ "No implementation in Perspectives.Query.Interpreter for " <> show qfd)

    -----------------------------------------------------------
    -- Value
    -----------------------------------------------------------
    (V _ val) -> case qfd of

      -- TODO
      (SQD dom (Value2Role _) _ _ _) -> throwError (error $ "Perspectives.Query.Interpreter: no implementation for Value2Role")


      otherwise -> throwError (error $ "No implementation in Perspectives.Query.Interpreter for " <> show qfd)

    -----------------------------------------------------------
    -- ContextKind
    -----------------------------------------------------------
    (CT contextType) -> case qfd of
      otherwise -> throwError (error $ "No implementation in Perspectives.Query.Interpreter for " <> show qfd)
    -----------------------------------------------------------
    -- ContextKind
    -----------------------------------------------------------
    (RT roleType) -> case qfd of

      -- TODO
      (SQD _ (DataTypeGetterWithParameter SpecialisesRoleTypeF parameter) _ _ _ ) -> throwError (error $ "No implementation in Perspectives.Query.Interpreter for SpecialisesRoleTypeF " <> show parameter )
      (SQD _ (TypeGetter RoleTypesF) _ _ _) -> throwError (error $ "No implementation in Perspectives.Query.Interpreter for RoleTypesF.")

      otherwise -> throwError (error $ "No implementation in Perspectives.Query.Interpreter for " <> show qfd)

toBool :: List Dependency -> Boolean
toBool (Cons (V _ (Value s)) _) = s == "true"
toBool _ = false

toString :: List Dependency -> String
toString (Cons (V _ (Value s)) _) = s
toString (Cons (C (ContextInstance c)) _) = c
toString (Cons (R (RoleInstance r)) _) = r
toString (Cons (CT (ContextType r)) _) = r
toString (Cons (RT (ENR (EnumeratedRoleType r))) _) = r
toString (Cons (RT (CR (CalculatedRoleType r))) _) = r
toString Nil = ""

-- | From a string that represents either a Calculated or an Enumerated property,
-- | for a given abstract datatype of roles, retrieve the values from a role instance.
-- | Returns a List with (R roleId) as its last dependency, (V "SomeProperty" "value") as its first dependency.
getDynamicPropertyGetter :: String -> ADT EnumeratedRoleType -> RoleInstance ~~> List Dependency
getDynamicPropertyGetter p adt rid = do
  (pt :: PropertyType) <- lift2MPQ $ getProperType p
  allProps <- lift2MPQ $ allLocallyRepresentedProperties adt
  if (isJust $ elemIndex pt allProps)
    then getterFromPropertyType pt rid
    else f rid

  where
    f :: RoleInstance ~~> List Dependency
    f roleInstance = do
      (bnd :: Maybe RoleInstance) <- lift2MPQ $ binding_ roleInstance
      case bnd of
        Nothing -> empty
        Just bnd' -> do
          (bndType :: EnumeratedRoleType) <- lift2MPQ $ roleType_ bnd'
          (flip snoc (R roleInstance)) <$> getDynamicPropertyGetter p (ST bndType) bnd'

-- | From a PropertyType, retrieve or construct a function to get values for that Property from a Role instance.
-- | Returns a List with (R roleId) as its last dependency, (V "SomeProperty" "value") as its first dependency.
getterFromPropertyType :: PropertyType -> RoleInstance ~~> List Dependency
getterFromPropertyType (ENP ep@(EnumeratedPropertyType id)) roleId = (flip Cons (singleton $ R roleId)) <<< V id <$> getProperty ep roleId
getterFromPropertyType (CP cp@(CalculatedPropertyType id)) roleId =
  (lift $ lift $ getPerspectType cp) >>=
    lift <<< lift <<< PC.calculation >>=
      \calc -> do
        old <- lift $ lift $ pushFrame
        lift $ lift (addBinding "currentrole" [unwrap roleId])
        r <- interpret calc (Cons (R roleId) Nil)
        lift $ lift $ restoreFrame old
        pure r
