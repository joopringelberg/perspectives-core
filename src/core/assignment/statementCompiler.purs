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

-- | From the syntax tree that describes a Statement, we construct a QueryFunctionDescription.

module Perspectives.Query.StatementCompiler
  (compileStatement)
where

import Control.Monad.Trans.Class (lift)
import Data.Array (filter, filterA, foldM, head, length, null, uncons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.Traversable (for_, traverse)
import Foreign.Object (Object, keys, values)
import Partial.Unsafe (unsafePartial)
import Perspectives.Checking.Authorization (roleHasPerspectiveOnRoleWithVerb)
import Perspectives.CoreTypes ((###=))
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.External.CoreModuleList (isExternalCoreModule)
import Perspectives.External.HiddenFunctionCache (lookupHiddenFunction, lookupHiddenFunctionCardinality, lookupHiddenFunctionIsEffect, lookupHiddenFunctionNArgs)
import Perspectives.Identifiers (areLastSegmentsOf, buitenRol, typeUri2ModelUri, endsWithSegments, isExternalRole, isTypeUri)
import Perspectives.Instances.Combinators (filter')
import Perspectives.ModelDependencies (rootContext)
import Perspectives.Parsing.Arc.Expression (endOf, startOf)
import Perspectives.Parsing.Arc.Expression.AST (Step, VarBinding(..))
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, addBinding, getsDF, lift2, throwError, withFrame)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Parsing.Arc.Statement.AST (Assignment(..), AssignmentOperator(..), LetABinding(..), LetStep(..), Statements(..))
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.ExpressionCompiler (compileExpression, makeSequence)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), RoleInContext, adtContext2AdtRoleInContext, domain2contextType, domain2roleType, functional, mandatory, range, roleInContext2Context, roleInContext2Role, roleRange)
import Perspectives.Query.QueryTypes (RoleInContext(..)) as QT
import Perspectives.Representation.ADT (ADT(..), allLeavesInADT, equalsOrGeneralises_, equalsOrSpecialises_)
import Perspectives.Representation.CNF (CNF)
import Perspectives.Representation.Class.Identifiable (identifier_)
import Perspectives.Representation.Class.PersistentType (StateIdentifier, getEnumeratedProperty, getEnumeratedRole)
import Perspectives.Representation.Class.Property (range) as PT
import Perspectives.Representation.Class.Role (completeDeclaredFillerRestriction, roleKindOfRoleType, toConjunctiveNormalForm_)
import Perspectives.Representation.Class.Role (roleTypeIsFunctional) as ROLE
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..)) as QF
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..), pessimistic)
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedPropertyType, EnumeratedRoleType(..), PropertyType(..), RoleKind(..), RoleType(..))
import Perspectives.Representation.Verbs (RoleVerb(..), PropertyVerb(..)) as Verbs
import Perspectives.Types.ObjectGetters (externalRole, generalisesRoleType_, hasPerspectiveOnPropertyWithVerb, isDatabaseQueryRole, isEnumeratedProperty, lookForRoleTypeOfADT, lookForUnqualifiedPropertyType, lookForUnqualifiedRoleTypeOfADT)
import Prelude (bind, discard, pure, show, unit, ($), (&&), (-), (<$>), (<*>), (<<<), (<>), (==), (>), (>>=), (||))

------------------------------------------------------------------------------------
------ MONAD TYPE FOR DESCRIPTIONCOMPILER
------------------------------------------------------------------------------------
type FD = PhaseThree QueryFunctionDescription

-- The user RoleType is necessary for setting inverted queries.
-- It is also used to check whether the executing user has a sufficient perspective.
-- The domain should be a CDOM.
-- | The expressions in the statements are compiled and inverted as well.
compileStatement ::
  Array StateIdentifier ->
  Domain ->
  Domain ->
  Array RoleType ->
  Statements ->
  PhaseThree QueryFunctionDescription
compileStatement stateIdentifiers originDomain currentcontextDomain userRoleTypes statements =
  case statements of
    -- Compile a series of Assignments into a QueryDescription.
    Statements assignments -> sequenceOfAssignments userRoleTypes assignments
      -- Compile the LetStep into a QueryDescription.
    Let letstep -> do
      let_ <- compileLetStep letstep
      pure (UQD originDomain QF.WithFrame let_ (range let_) (functional let_) (mandatory let_))
  where

  compileLetStep :: LetStep -> PhaseThree QueryFunctionDescription
  compileLetStep (LetStep {bindings, assignments}) = withFrame
    case uncons bindings of
      -- no bindings at all. Just the body. This will probably never occur as the parser breaks on it.
      Nothing -> sequenceOfAssignments userRoleTypes assignments
      (Just {head: bnd, tail}) -> do
        -- compileVarBinding also adds a variable binding to the compile time environment.
        head_ <- compileVarBinding bnd
        makeSequence <$> foldM addVarBindingToSequence head_ tail <*> sequenceOfAssignments userRoleTypes assignments
    where
      -- Inverts the result as well.
      compileVarBinding :: LetABinding -> PhaseThree QueryFunctionDescription
      compileVarBinding (Expr (VarBinding varName step)) = do
          step_ <- compileExpression originDomain step
          addBinding varName step_
          pure $ UQD originDomain (QF.BindVariable varName) step_ (range step_) (functional step_) (mandatory step_)
      compileVarBinding (Stat varName ass) = do
        assignmentDescription <- describeAssignmentStatement userRoleTypes ass
        -- Add the binding to the compile time environment.
        addBinding varName assignmentDescription
        pure $ UQD originDomain (QF.BindResultFromCreatingAssignment varName) assignmentDescription (range assignmentDescription) (functional assignmentDescription) (mandatory assignmentDescription)

      -- The range of a sequence equals that of its second term.
      -- The fold is left associative: ((binding1 *> binding2) *> binding3). The compiler handles that ok.
      addVarBindingToSequence :: QueryFunctionDescription -> LetABinding -> FD
      addVarBindingToSequence seq v = makeSequence <$> pure seq <*> (compileVarBinding v)

  -- This will return a QueryFunctionDescription that describes either a single assignment, or
  -- a BQD with QueryFunction equal to (BinaryCombinator SequenceF)
  sequenceOfAssignments :: Array RoleType -> Array Assignment -> PhaseThree QueryFunctionDescription
  sequenceOfAssignments subjects assignments = case uncons assignments of
    Nothing -> throwError $ Custom "There must be at least one assignment in a let*"
    (Just {head, tail}) -> do
      head_ <- describeAssignmentStatement subjects head
      foldM addAssignmentToSequence head_ tail
    where
      -- Returns a BQD with QueryFunction (BinaryCombinator SequenceF)
      addAssignmentToSequence :: QueryFunctionDescription -> Assignment -> PhaseThree QueryFunctionDescription
      addAssignmentToSequence seq v = makeSequence <$> pure seq <*> (describeAssignmentStatement subjects v)

  -- we need the Object of the Perspective. Right now it is a RoleType, possibly a(n anonymous) CalculatedRole.
  -- The assignment functions arbitrarily return the currentContext, except for the Create statements CreateRole
  -- and CreateContext. Hence,
  -- we declare the functions to be both functional and mandatory.
  -- All inverted queries that need be created are created in this function.
  -- TODO: Controleer of de assignment operators wel toegestaan zijn gegeven de perspectieven van de subjects.
  describeAssignmentStatement :: Array RoleType -> Assignment -> PhaseThree QueryFunctionDescription
  describeAssignmentStatement subjects ass = case ass of
      RemoveRole {roleExpression, start, end} -> do
        (rle :: QueryFunctionDescription) <- ensureRole subjects roleExpression
        -- Check for each of the subjects whether they have a sufficient perspective on the range of the role expression.
        for_ subjects (\subject -> for_ (roleInContext2Role <$> (allLeavesInADT $ unsafePartial roleRange rle))
          (\object -> (lift $ lift $ roleHasPerspectiveOnRoleWithVerb subject object [Verbs.Remove] (Just start) (Just end)) >>= case _ of 
            Left e -> throwError e
            _ -> pure unit))
        pure $ UQD currentcontextDomain QF.RemoveRole rle currentcontextDomain True True
      RemoveContext {roleExpression, start, end} -> do
        rle <- ensureRole subjects roleExpression
        -- The range of the QueryFunctionDescription must consist of EnumeratedRoleTypes that have kind ContextRole.
        isContextRole <- foldM
          (\isContextRole (QT.RoleInContext{role}) -> do
            EnumeratedRole{kindOfRole} <- lift $ lift $ getEnumeratedRole role
            pure $ isContextRole && (ContextRole == kindOfRole))
          true
          (allLeavesInADT $ unsafePartial domain2roleType $ range rle)
        if isContextRole
          then do 
            -- Check for each of the subjects whether they have a sufficient perspective on the range of the role expression.
            for_ subjects (\subject -> for_ (roleInContext2Role <$> (allLeavesInADT $ unsafePartial roleRange rle))
              (\object -> (lift $ lift $ roleHasPerspectiveOnRoleWithVerb subject object [Verbs.DeleteContext, Verbs.RemoveContext] (Just start) (Just end)) >>= case _ of 
                Left e -> throwError e
                _ -> pure unit))
            pure $ UQD currentcontextDomain QF.RemoveContext rle currentcontextDomain True True
          else throwError $ NotAContextRole start end
      CreateRole {roleIdentifier, contextExpression, localName, start, end} -> do
        mnameGetterDescription <- ensureStringValue localName
        (cte :: QueryFunctionDescription) <- unsafePartial constructContextGetterDescription contextExpression
        qualifiedRoleIdentifier <- qualifyAsEnumeratedTypeWithRespectTo roleIdentifier cte start end
        -- Check for each of the subjects whether they have a sufficient perspective to create the qualifiedRoleIdentifier.
        for_ subjects (\subject -> (lift $ lift $ roleHasPerspectiveOnRoleWithVerb subject qualifiedRoleIdentifier [Verbs.Create] (Just start) (Just end)) >>= case _ of 
            Left e -> throwError e
            _ -> pure unit)
        -- Because we can use CreateRole in a binding in a letA, we return a meaningful range value.
        case mnameGetterDescription of
          Nothing -> pure $ UQD 
            originDomain 
            (QF.CreateRole qualifiedRoleIdentifier) 
            cte 
            (RDOM (adtContext2AdtRoleInContext (unsafePartial domain2contextType (range cte)) qualifiedRoleIdentifier)) 
            True 
            True
          Just nameGetterDescription -> pure $ BQD
            originDomain
            (QF.CreateRole qualifiedRoleIdentifier) 
            cte
            nameGetterDescription
            (RDOM (adtContext2AdtRoleInContext (unsafePartial domain2contextType (range cte)) qualifiedRoleIdentifier)) 
            True
            True        
      CreateContext {contextTypeIdentifier, localName, roleTypeIdentifier, contextExpression, start, end} -> do
        (cte :: QueryFunctionDescription) <- unsafePartial constructContextGetterDescription contextExpression
        -- The optional name for the instance (it is an expression).
        (mnameGetterDescription :: Maybe QueryFunctionDescription) <- ensureStringValue localName
        -- The context type to create.
        qualifiedContextTypeIdentifier <- qualifyContextType contextTypeIdentifier start end
        case roleTypeIdentifier of
          Just r -> do
            -- Create a new instance of qualifiedRoleIdentifier to fill with the new context instance.
            (qualifiedRoleIdentifier :: RoleType) <- qualifyWithRespectTo r cte start end
            case qualifiedRoleIdentifier of
              CR calculatedType -> do
                -- The calculation consists of retrieving instances from the database.
                -- NOTE. I do not know how to check the perspective, here. 
                -- I wonder whether this case arises, ever!
                isDBQRole <- lift2 $ isDatabaseQueryRole qualifiedRoleIdentifier
                if isDBQRole
                  then case mnameGetterDescription of
                    Nothing -> pure $ UQD
                      originDomain
                      (QF.CreateContext qualifiedContextTypeIdentifier qualifiedRoleIdentifier)
                      cte
                      (RDOM (adtContext2AdtRoleInContext (unsafePartial domain2contextType (range cte)) (EnumeratedRoleType $ buitenRol $ unwrap qualifiedContextTypeIdentifier)))
                      True
                      True
                    Just nameGetterDescription -> pure $ BQD
                      originDomain
                      (QF.CreateContext qualifiedContextTypeIdentifier qualifiedRoleIdentifier)
                      cte
                      nameGetterDescription
                      (RDOM (adtContext2AdtRoleInContext (unsafePartial domain2contextType (range cte)) (EnumeratedRoleType $ buitenRol $ unwrap qualifiedContextTypeIdentifier)))
                      True
                      True
                  else throwError $ CannotCreateCalculatedRole calculatedType start end
              ENR enumeratedType -> do 
                -- Check for each of the subjects whether they have a sufficient perspective to create a context in the qualifiedRoleIdentifier.
                for_ subjects (\subject -> (lift $ lift $ roleHasPerspectiveOnRoleWithVerb subject enumeratedType [Verbs.CreateAndFill] (Just start) (Just end)) >>= case _ of 
                    Left e -> throwError e
                    _ -> pure unit)
                case mnameGetterDescription of
                  Nothing -> pure $ UQD originDomain (QF.CreateContext qualifiedContextTypeIdentifier qualifiedRoleIdentifier) cte (RDOM (adtContext2AdtRoleInContext (unsafePartial domain2contextType (range cte)) enumeratedType)) True True
                  Just nameGetterDescription -> pure $ BQD
                    originDomain
                    (QF.CreateContext qualifiedContextTypeIdentifier qualifiedRoleIdentifier)
                    cte
                    nameGetterDescription
                    (RDOM (adtContext2AdtRoleInContext (unsafePartial domain2contextType (range cte)) enumeratedType))
                    True
                    True          
          Nothing -> do 
          -- It is an error if qualifiedContextTypeIdentifier is not a specialisation of RootContext.
          -- As there is no role to fill, the user cannot have a perspective on it. Hence we cannot check that perspective!
          -- In other words, any user can create a RootContext. In practice RootContexts are only used for 'apps'.
            er <- lift $ lift $ externalRole qualifiedContextTypeIdentifier
            allowed <- lift $ lift ((ENR $ EnumeratedRoleType rootContext) `generalisesRoleType_` (ENR er))
            if allowed
              then case mnameGetterDescription of
                Nothing -> pure $ UQD originDomain (QF.CreateRootContext qualifiedContextTypeIdentifier) cte (CDOM $ UET qualifiedContextTypeIdentifier) True True
                Just nameGetterDescription -> pure $ BQD originDomain (QF.CreateRootContext qualifiedContextTypeIdentifier) cte nameGetterDescription (CDOM $ UET qualifiedContextTypeIdentifier) True True
              else throwError $ NotARootContext start end qualifiedContextTypeIdentifier

      CreateContext_ {contextTypeIdentifier, localName, roleExpression, start, end} -> do
        roleQfd <- ensureRole subjects roleExpression
        mnameGetterDescription <- ensureStringValue localName
        qualifiedContextTypeIdentifier <- qualifyContextType contextTypeIdentifier start end
        -- Check for each of the subjects whether they have a sufficient perspective on the range of the role expression.
        for_ subjects (\subject -> for_ (roleInContext2Role <$> (allLeavesInADT $ unsafePartial roleRange roleQfd))
          (\object -> (lift $ lift $ roleHasPerspectiveOnRoleWithVerb subject object [Verbs.Fill] (Just start) (Just end)) >>= case _ of 
            Left e -> throwError e
            _ -> pure unit))
        case mnameGetterDescription of
          Nothing -> pure $ UQD originDomain (QF.CreateContext_ qualifiedContextTypeIdentifier) roleQfd originDomain True True
          Just nameGetterDescription -> pure $ BQD originDomain (QF.CreateContext_ qualifiedContextTypeIdentifier) roleQfd nameGetterDescription originDomain True True
      -- NOTE: it is currently not possible to specify the move action.
      Move {roleExpression, contextExpression} -> do
        rle <- ensureRole subjects roleExpression
        (cte :: QueryFunctionDescription) <- unsafePartial constructContextGetterDescription contextExpression >>= \qfd -> case contextExpression of
          Nothing -> pure qfd
          Just stp -> ensureFunctional stp qfd
        pure $ BQD originDomain QF.Move rle cte originDomain True True
      Bind f@{bindingExpression, roleIdentifier, contextExpression, start, end} -> do
        -- Bind <binding-expression> to <binderType> [in <context-expression>]. Check:
        -- bindingExpression should result in roles
        (bindings :: QueryFunctionDescription) <- ensureRole subjects bindingExpression
        (cte :: QueryFunctionDescription) <- unsafePartial constructContextGetterDescription contextExpression
        -- binderType should be an EnumeratedRoleType (local name should resolve w.r.t. the contextExpression)
        (qualifiedRoleIdentifier :: EnumeratedRoleType) <- qualifyAsEnumeratedTypeWithRespectTo roleIdentifier cte f.start f.end
        -- If the roleIdentifier is functional, the bindings should be functional too.
        (lift $ lift $ ROLE.roleTypeIsFunctional (ENR qualifiedRoleIdentifier)) >>= if _
          then case functional bindings of
            True -> pure unit
            Unknown -> throwError $ MaybeNotFunctional f.start f.end bindingExpression
            False -> throwError $ NotFunctional f.start f.end bindingExpression
          else pure unit
        -- Check for each of the subjects whether they have a sufficient perspective to fill the qualifiedRoleIdentifier.
        for_ subjects (\subject -> (lift $ lift $ roleHasPerspectiveOnRoleWithVerb subject qualifiedRoleIdentifier [Verbs.Fill] (Just start) (Just end)) >>= case _ of 
            Left e -> throwError e
            _ -> pure unit)
        -- the possible fillers of binderType (qualifiedRoleIdentifier) should be less specific (=more general) than or equal to the type of the results of binderExpression (fillers).
        qualifies <- do
          (mfillerRestriction :: Maybe (CNF RoleInContext)) <- lift $ lift (getEnumeratedRole qualifiedRoleIdentifier >>= completeDeclaredFillerRestriction >>= traverse toConjunctiveNormalForm_)
          (fillers :: CNF RoleInContext) <- lift $ lift $ toConjunctiveNormalForm_ (unsafePartial domain2roleType (range bindings))
          case mfillerRestriction of
            Nothing -> pure true
            Just fillerRestriction -> do
              -- fillerRestriction -> fillers
              pure (fillers `equalsOrSpecialises_` fillerRestriction)
        if qualifies
          -- Create a function description that describes the actual role creating and binding.
          then pure $ BQD originDomain (QF.Bind qualifiedRoleIdentifier) bindings cte originDomain True True
          else throwError $ RoleDoesNotBind f.start (ENR qualifiedRoleIdentifier) (unsafePartial $ domain2roleType (range bindings))

      Bind_ {bindingExpression, binderExpression, start, end} -> do
        -- bindingExpression should result in a functional role
        (bindings :: QueryFunctionDescription) <- ensureRole subjects  bindingExpression >>= ensureFunctional bindingExpression
        -- binderExpression should result in a functional role
        (binders :: QueryFunctionDescription) <- ensureRole subjects binderExpression >>= ensureFunctional binderExpression
        -- Check for each of the subjects whether they have a sufficient perspective on the range of the binders expression.
        for_ subjects (\subject -> for_ (roleInContext2Role <$> (allLeavesInADT $ unsafePartial roleRange binders))
          (\object -> (lift $ lift $ roleHasPerspectiveOnRoleWithVerb subject object [Verbs.Fill] (Just start) (Just end)) >>= case _ of 
            Left e -> throwError e
            _ -> pure unit))
        -- Now create a function description.
        pure $ BQD originDomain QF.Bind_ bindings binders originDomain True True

      Unbind f@{bindingExpression, roleIdentifier, start, end} -> do
        (bindings :: QueryFunctionDescription) <- ensureRole subjects bindingExpression
        -- the type of the binder (indicated by roleIdentifier) should be an EnumeratedRoleType 
        -- (local name should resolve w.r.t. the binders of the bindings). 
        -- We try to resolve in the model and then filter candidates on whether they bind the bindings. 
        -- If they don't, the expression has no meaning.
        (mqualifiedRoleIdentifier :: Maybe EnumeratedRoleType) <- qualifyBinderType roleIdentifier (unsafePartial $ domain2roleType $ range bindings) f.start f.end
        -- Check for each of the subjects whether they have a sufficient perspective to remove the filler of the qualifiedRoleIdentifier.
        case mqualifiedRoleIdentifier of 
          Just qualifiedRoleIdentifier -> 
            for_ subjects (\subject -> (lift $ lift $ roleHasPerspectiveOnRoleWithVerb subject qualifiedRoleIdentifier [Verbs.RemoveFiller] (Just start) (Just end)) >>= case _ of 
                Left e -> throwError e
                _ -> pure unit)
          Nothing -> pure unit
        pure $ UQD originDomain (QF.Unbind mqualifiedRoleIdentifier) bindings originDomain True True

      Unbind_ {bindingExpression, binderExpression, start, end} -> do
        -- bindingExpression should result in a functional role
        (bindings :: QueryFunctionDescription) <- ensureRole subjects  bindingExpression >>= ensureFunctional bindingExpression
        -- binderExpression should result in a functional role
        (binders :: QueryFunctionDescription) <- ensureRole subjects binderExpression >>= ensureFunctional binderExpression
        -- Check for each of the subjects whether they have a sufficient perspective on the range of the binders expression.
        for_ subjects (\subject -> for_ (roleInContext2Role <$> (allLeavesInADT $ unsafePartial roleRange binders))
          (\object -> (lift $ lift $ roleHasPerspectiveOnRoleWithVerb subject object [Verbs.RemoveFiller] (Just start) (Just end)) >>= case _ of 
            Left e -> throwError e
            _ -> pure unit))
        -- Now create a function description.
        pure $ BQD originDomain QF.Unbind_ bindings binders originDomain True True

      DeleteRole f@{roleIdentifier, contextExpression, start, end} -> do
        (cte :: QueryFunctionDescription) <- unsafePartial constructContextGetterDescription contextExpression
        (qualifiedRoleIdentifier :: EnumeratedRoleType) <- qualifyAsEnumeratedTypeWithRespectTo roleIdentifier cte f.start f.end
        -- Check for each of the subjects whether they have a sufficient perspective to Delete the qualifiedRoleIdentifier.
        for_ subjects (\subject -> (lift $ lift $ roleHasPerspectiveOnRoleWithVerb subject qualifiedRoleIdentifier [Verbs.Delete] (Just start) (Just end)) >>= case _ of 
            Left e -> throwError e
            _ -> pure unit)
        pure $ UQD originDomain (QF.DeleteRole qualifiedRoleIdentifier) cte originDomain True True

      DeleteContext f@{contextRoleIdentifier, contextExpression, start, end} -> do
        (cte :: QueryFunctionDescription) <- unsafePartial constructContextGetterDescription contextExpression
        (qualifiedRoleType :: RoleType) <- qualifyWithRespectTo contextRoleIdentifier cte f.start f.end
        case qualifiedRoleType of 
          ENR qualifiedRoleIdentifier -> 
            -- Check for each of the subjects whether they have a sufficient perspective to Delete the qualifiedRoleIdentifier.
            for_ subjects (\subject -> (lift $ lift $ roleHasPerspectiveOnRoleWithVerb subject qualifiedRoleIdentifier [Verbs.DeleteContext] (Just start) (Just end)) >>= case _ of 
                Left e -> throwError e
                _ -> pure unit)
          _ -> pure unit
        kindOfRole <- (lift $ lift $ roleKindOfRoleType qualifiedRoleType)
        if kindOfRole == ContextRole
          then pure $ UQD originDomain (QF.DeleteContext qualifiedRoleType) cte originDomain True True
          else throwError $ NotAContextRole start end

      DeleteProperty f@{propertyIdentifier, roleExpression, start, end} -> do
        (roleQfd :: QueryFunctionDescription) <- case roleExpression of
          Nothing -> pure $ SQD originDomain (QF.DataTypeGetter QF.IdentityF) originDomain True True
          Just e -> ensureRole subjects e
        (qualifiedProperty :: EnumeratedPropertyType) <- qualifyPropertyWithRespectTo propertyIdentifier roleQfd f.start f.end
        -- Check for each of the subjects whether they have a sufficient perspective to Delete the property.
        for_ subjects (\subject -> for_ (roleInContext2Role <$> (allLeavesInADT $ unsafePartial roleRange roleQfd))
          -- I assume the object of the perspective has been compiled.
          (\object -> (lift $ lift $ unsafePartial hasPerspectiveOnPropertyWithVerb subject object qualifiedProperty Verbs.DeleteProperty) >>= if _ 
            then pure unit
            else throwError (UnauthorizedForProperty "Auteur" subject (ENR object) (ENP qualifiedProperty) (Verbs.DeleteProperty) (Just start) (Just end))))
        pure $ UQD originDomain (QF.DeleteProperty qualifiedProperty) roleQfd originDomain True True

      PropertyAssignment f@{propertyIdentifier, operator, valueExpression, roleExpression, start, end} -> do
        (roleQfd :: QueryFunctionDescription) <- case roleExpression of
          Nothing -> pure $ SQD originDomain (QF.DataTypeGetter QF.IdentityF) originDomain True True
          Just e -> do
            qfd <- compileExpression originDomain e
            case range qfd of
              (RDOM _) -> pure qfd
              otherwise -> throwError $ NotARoleDomain (range qfd) (startOf e) (endOf e)

        (qualifiedProperty :: EnumeratedPropertyType) <- qualifyPropertyWithRespectTo propertyIdentifier roleQfd f.start f.end
        -- Compile the value expression to a QueryFunctionDescription. Its range must comply with the range of the qualifiedProperty. It is compiled relative to the current context; not relative to the object!
        valueQfd <- compileExpression originDomain valueExpression
        rangeOfProperty <- lift $ lift $ getEnumeratedProperty qualifiedProperty >>= PT.range
        fname <- case operator of
          Set _ -> pure $ QF.SetPropertyValue qualifiedProperty
          AddTo _ -> pure $ QF.AddPropertyValue qualifiedProperty
          DeleteFrom _ -> pure $ QF.RemovePropertyValue qualifiedProperty
        verb <- case operator of
          Set _ -> pure $ Verbs.SetPropertyValue
          AddTo _ -> pure $ Verbs.AddPropertyValue
          DeleteFrom _ -> pure $ Verbs.RemovePropertyValue
        -- Check for each of the subjects whether they have a sufficient perspective to apply the operator the property.
        for_ subjects (\subject -> for_ (roleInContext2Role <$> (allLeavesInADT $ unsafePartial roleRange roleQfd))
          -- I assume the object of the perspective has been compiled.
          (\object -> (lift $ lift $ unsafePartial hasPerspectiveOnPropertyWithVerb subject object qualifiedProperty verb) >>= if _ 
            then pure unit
            else throwError (UnauthorizedForProperty "Auteur" subject (ENR object) (ENP qualifiedProperty) verb (Just start) (Just end))))
        case range valueQfd of
          (VDOM r _) | r == rangeOfProperty -> pure unit
          (VDOM r _) -> throwError $ WrongPropertyRange (startOf valueExpression) (endOf valueExpression) rangeOfProperty r
          otherwise -> throwError $ NotAPropertyRange (startOf valueExpression) (endOf valueExpression) rangeOfProperty
        pure $ BQD originDomain fname valueQfd roleQfd originDomain True True
      CreateFile f@{roleExpression, contentExpression, propertyIdentifier, mimeType, fileNameExpression} -> do
        filenameQfd <- compileExpression originDomain fileNameExpression
        -- Check that it returns a string!
        contentQfd <- compileExpression originDomain contentExpression
        (roleQfd :: QueryFunctionDescription) <- case roleExpression of
                  Nothing -> pure $ SQD originDomain (QF.DataTypeGetter QF.IdentityF) originDomain True True
                  Just e -> do
                    qfd <- compileExpression originDomain e
                    case range qfd of
                      (RDOM _) -> if pessimistic $ functional qfd 
                        then pure qfd
                        else throwError $ NotFunctional (startOf e) (endOf e) e
                      otherwise -> throwError $ NotARoleDomain (range qfd) (startOf e) (endOf e)
        (qualifiedProperty :: EnumeratedPropertyType) <- qualifyPropertyWithRespectTo propertyIdentifier roleQfd f.start f.end
        pure $ MQD originDomain (QF.CreateFileF mimeType qualifiedProperty) [filenameQfd, contentQfd, roleQfd] originDomain True False

      ExternalEffect f@{start, end, effectName, arguments} -> case lookupHiddenFunction effectName of 
        Nothing -> throwError (UnknownExternalFunction start end effectName)
        Just _ ->  if lookupHiddenFunctionIsEffect effectName
          then do
            case (typeUri2ModelUri effectName) of
              Nothing -> throwError (NotWellFormedName start effectName)
              Just modelName -> if isExternalCoreModule modelName
                then do
                  mexpectedNrOfArgs <- pure $ lookupHiddenFunctionNArgs effectName
                  -- TODO. Check here whether effectName refers to an Effect and not to a query function!
                  case mexpectedNrOfArgs of
                    Nothing -> throwError (UnknownExternalFunction start end effectName)
                    Just expectedNrOfArgs -> if expectedNrOfArgs == length arguments || expectedNrOfArgs == length arguments - 1
                      then do
                        isFunctional <- pure $ unsafePartial $ fromJust $ lookupHiddenFunctionCardinality effectName
                        -- The argument is an expression that can yield a ContextInstance, a RoleInstance or a Value.
                        -- If it yields a Value taken from some Property, then the subject has an implicit Perspective in this State on that PropertyType.
                        compiledArguments <- traverse (\s -> compileExpression originDomain s) arguments
                        pure $ MQD originDomain (QF.ExternalEffectFullFunction effectName) compiledArguments originDomain isFunctional Unknown
                      else throwError (WrongNumberOfArguments start end effectName expectedNrOfArgs (length arguments))
                -- TODO: behandel hier Foreign functions.
                else throwError (UnknownExternalFunction start end effectName)
          else throwError (NotAnEffect start end effectName)
      ExternalDestructiveEffect f@{start, end, effectName, arguments} -> do
        case (typeUri2ModelUri effectName) of
          Nothing -> throwError (NotWellFormedName start effectName)
          Just modelName -> if isExternalCoreModule modelName
            then do
              mexpectedNrOfArgs <- pure $ lookupHiddenFunctionNArgs effectName
              -- TODO. Check here whether effectName refers to an Effect and not to a query function!
              case mexpectedNrOfArgs of
                Nothing -> throwError (UnknownExternalFunction start end effectName)
                Just expectedNrOfArgs -> if expectedNrOfArgs == length arguments || expectedNrOfArgs == length arguments - 1
                  then do
                    isFunctional <- pure $ unsafePartial $ fromJust $ lookupHiddenFunctionCardinality effectName
                    -- The argument is an expression that can yield a ContextInstance, a RoleInstance or a Value.
                    -- If it yields a Value taken from some Property, then the subject has an implicit Perspective in this State on that PropertyType.
                    compiledArguments <- traverse (\s -> compileExpression originDomain s) arguments
                    pure $ MQD originDomain (QF.ExternalDestructiveFunction effectName) compiledArguments originDomain isFunctional Unknown
                  else throwError (WrongNumberOfArguments start end effectName expectedNrOfArgs (length arguments))
            -- TODO: behandel hier Foreign functions.
            else throwError (UnknownExternalFunction start end effectName)
      where

        qualifyAsEnumeratedTypeWithRespectTo :: String -> QueryFunctionDescription -> ArcPosition -> ArcPosition -> PhaseThree EnumeratedRoleType
        qualifyAsEnumeratedTypeWithRespectTo roleIdentifier contextFunctionDescription start end = do
          t <- qualifyWithRespectTo roleIdentifier contextFunctionDescription start end
          case t of
            ENR et -> pure et
            CR ct' -> throwError $ CannotCreateCalculatedRole ct' start end

        qualifyWithRespectTo :: String -> QueryFunctionDescription -> ArcPosition -> ArcPosition -> PhaseThree RoleType
        qualifyWithRespectTo roleIdentifier contextFunctionDescription start end = do
          (ct :: ADT ContextType) <- case range contextFunctionDescription of
            (CDOM ct') -> pure ct'
            otherwise -> throwError $ NotAContextDomain contextFunctionDescription otherwise start end
          rtarr <- if isTypeUri roleIdentifier
            then if isExternalRole roleIdentifier
              then pure [ENR $ EnumeratedRoleType roleIdentifier]
              else lift2 $ runArrayT $ lookForRoleTypeOfADT roleIdentifier ct
            else if roleIdentifier == "External"
              then case ct of
                (ST (ContextType cid)) -> pure [ENR (EnumeratedRoleType (cid <> "$External"))]
                (UET (ContextType cid)) -> pure [ENR (EnumeratedRoleType (cid <> "$External"))]
                otherwise -> throwError $ Custom ("Cannot get the external role of a compound type: " <> show otherwise)
              else lift2 (ct ###= lookForUnqualifiedRoleTypeOfADT roleIdentifier)
          case head rtarr of
            Just et@(ENR _) -> pure et
            Just ct'@(CR _) -> pure ct'
            otherwise -> throwError $ ContextHasNoRole ct roleIdentifier start end

        -- Either the identifier is qualified, or we qualify it with respect to the model.
        qualifyContextType :: String -> ArcPosition -> ArcPosition -> PhaseThree ContextType
        qualifyContextType contextIdentifier start end = if isTypeUri contextIdentifier
          then pure $ ContextType contextIdentifier
          else do
            ctxts <- getsDF (keys <<< _.contexts)
            case filter (areLastSegmentsOf contextIdentifier) ctxts of
              toomany | length toomany > 1 -> throwError $ NotUniquelyIdentifying start contextIdentifier toomany
              justone | length justone == 1 -> pure $ ContextType $ unsafePartial fromJust $ head justone
              none -> throwError $ CannotFindContextType start end contextIdentifier

        qualifyPropertyWithRespectTo :: String -> QueryFunctionDescription -> ArcPosition -> ArcPosition -> PhaseThree EnumeratedPropertyType
        qualifyPropertyWithRespectTo propertyIdentifier roleQfd start end = do
          (rt :: ADT EnumeratedRoleType) <- case range roleQfd of
            (RDOM rt') -> pure $ roleInContext2Role <$> rt'
            otherwise -> throwError $ NotARoleDomain otherwise start end
          (candidates :: Array PropertyType) <- lift2 (rt ###= (filter' (lookForUnqualifiedPropertyType propertyIdentifier) isEnumeratedProperty))
          case head candidates of
            Just (ENP et) | length candidates == 1 -> pure et
            otherwise -> throwError $ RoleHasNoEnumeratedProperty rt propertyIdentifier start end

        -- | If the name is unqualified, look for an EnumeratedRole with matching local name in the Domain.
        -- | Then, we check whether a candidate's binding type equals the second argument, or is less specialised. 
        -- | In other words: whether the candidate could bind it (the second argument).
        qualifyBinderType :: Maybe String -> ADT QT.RoleInContext -> ArcPosition -> ArcPosition -> PhaseThree (Maybe EnumeratedRoleType)
        qualifyBinderType Nothing _ _ _ = pure Nothing
        qualifyBinderType (Just ident) fillers start end = if isTypeUri ident
          then pure $ Just $ EnumeratedRoleType ident
          else do
            -- The expansion of the fillers
            (expandedFillers :: CNF RoleInContext) <- lift $ lift $ toConjunctiveNormalForm_ fillers
            -- EnumeratedRoles in the model with (end)matching name.
            (enumeratedRoles :: Object EnumeratedRole) <- getsDF _.enumeratedRoles
            (nameMatches :: Array EnumeratedRole) <- pure (filter (\(EnumeratedRole{id:roleId}) -> (unwrap roleId) `endsWithSegments` ident) (values enumeratedRoles))
            -- EnumeratedRoles that can be filled with `fillers`.
            (candidates :: Array EnumeratedRole) <- lift $ lift (filterA (\(candidate :: EnumeratedRole) -> do
              (mexpandedCandidateRestriction :: Maybe (CNF RoleInContext)) <- completeDeclaredFillerRestriction candidate >>= traverse toConjunctiveNormalForm_
              case mexpandedCandidateRestriction of 
                Nothing -> pure true
                -- The restriction on filling the candidate must be equal to or more general (less specialised) than the fillers.
                Just expandedCandidateRestriction -> pure (expandedCandidateRestriction `equalsOrGeneralises_` expandedFillers))
              nameMatches)
            case head candidates of
              Nothing -> if null nameMatches
                then throwError $ UnknownRole start ident
                else throwError $ LocalRoleDoesNotBind start end ident fillers
              (Just (EnumeratedRole {id:candidate})) | length candidates == 1 -> pure $ Just candidate
              otherwise -> throwError $ NotUniquelyIdentifying start ident (identifier_ <$> candidates)

        -- Compiles the Step and inverts it as well.
        -- NOTE: parameter userTypes is not used.
        ensureContext :: Array RoleType -> Step -> PhaseThree QueryFunctionDescription
        ensureContext userTypes stp  = do
          -- An expression that results in a ContextInstance, in this state, for this usertype.
          qfd <- compileExpression originDomain stp
          case range qfd of
            (CDOM _) -> pure qfd
            otherwise -> throwError $ NotAContextDomain qfd (range qfd) (startOf stp) (endOf stp)

        ensureStringValue :: Maybe Step -> PhaseThree (Maybe QueryFunctionDescription)
        ensureStringValue mstp = case mstp of
          Just stp -> do
            qfd <- compileExpression originDomain stp
            case range qfd of 
              (VDOM PString _) -> pure $ Just qfd
              otherwise -> throwError $ NotAStringDomain qfd (startOf stp) (endOf stp)
          Nothing -> pure Nothing

        -- Compiles the Step and inverts it as well.
        -- NOTE: parameter userTypes is not used.
        ensureRole :: Array RoleType -> Step -> PhaseThree QueryFunctionDescription
        ensureRole userTypes stp = do
          -- An expression that results in a RoleInstance, in this state, for this usertype.
          qfd <- compileExpression originDomain stp
          case range qfd of
            (RDOM _) -> pure qfd
            otherwise -> throwError $ NotARoleDomain (range qfd) (startOf stp) (endOf stp)

        ensureFunctional :: Step -> QueryFunctionDescription -> PhaseThree QueryFunctionDescription
        ensureFunctional stp qfd = case functional qfd of
          True -> pure qfd
          Unknown -> throwError $ MaybeNotFunctional (startOf stp) (endOf stp) stp
          False -> throwError $ NotFunctional (startOf stp) (endOf stp) stp

        constructContextGetterDescription :: Partial => Maybe Step -> PhaseThree QueryFunctionDescription
        constructContextGetterDescription contextExpression =
          case contextExpression of
            -- TODO. Pas dit toe in alle gevallen.
            Nothing -> case originDomain of
              -- Apply the identity function if the current domain is a context;
              CDOM _ -> pure (SQD originDomain (QF.DataTypeGetter QF.IdentityF) originDomain True True)
              -- Apply the context function if it is a role!
              RDOM roleADT -> pure (SQD originDomain (QF.DataTypeGetter QF.ContextF) (CDOM $ roleInContext2Context <$> roleADT) True True)
              -- RDOM _ membeddingContext -> case membeddingContext of
              --   Nothing -> pure (SQD originDomain (QF.DataTypeGetter QF.ContextF) currentcontextDomain True True)
              --   Just embeddingContext -> pure (SQD originDomain (QF.DataTypeGetter QF.ContextF) (CDOM $ ST embeddingContext) True True)
            (Just (stp :: Step)) -> ensureContext subjects stp
