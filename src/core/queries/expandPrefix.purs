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

module Perspectives.Query.ExpandPrefix where

import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Perspectives.Parsing.Arc.AST (ActionE(..), AutomaticEffectE(..), ColumnE(..), ContextActionE(..), FormE(..), NotificationE(..), PropertyVerbE(..), PropsOrView(..), RoleIdentification(..), RoleVerbE(..), RowE(..), ScreenE(..), ScreenElement(..), SelfOnly(..), StateE(..), StateQualifiedPart(..), StateSpecification(..), StateTransitionE(..), TabE(..), TableE(..), WidgetCommonFields)
import Perspectives.Parsing.Arc.Expression.AST (BinaryStep(..), ComputationStep(..), PureLetStep(..), SimpleStep(..), Step(..), UnaryStep(..), VarBinding(..))
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwo, expandNamespace)
import Perspectives.Parsing.Arc.Statement.AST (Assignment(..), LetABinding(..), LetStep(..), Statements(..))
import Perspectives.Query.QueryTypes (Calculation(..))
import Perspectives.Representation.Sentence (Sentence(..), SentencePart(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), EnumeratedRoleType(..), RoleType(..))
import Prelude (bind, flip, pure, ($), (<$>), (<*>), (<<<), (>>=))

class ContainsPrefixes s where
  expandPrefix :: s -> PhaseTwo s

instance containsPrefixesStep :: ContainsPrefixes Step where
  expandPrefix (Simple s) = Simple <$> expandPrefix s
  expandPrefix (Binary s) = Binary <$> expandPrefix s
  expandPrefix (Unary s) = Unary <$> expandPrefix s
  expandPrefix (PureLet s) = PureLet <$> expandPrefix s
  expandPrefix (Computation s) = Computation <$> expandPrefix s

instance containsPrefixesSimpleStep :: ContainsPrefixes SimpleStep where
  expandPrefix (ArcIdentifier pos s) = ArcIdentifier pos <$> expandNamespace s
  expandPrefix (ContextTypeIndividual pos s) = ContextTypeIndividual pos <$> expandNamespace s
  expandPrefix (RoleTypeIndividual pos s) = RoleTypeIndividual pos <$> expandNamespace s
  expandPrefix (Filler pos mcontext) = Filler pos <$> (traverse expandNamespace mcontext)
  expandPrefix (Filled pos s mcontext) = Filled pos <$> expandNamespace s <*> (traverse expandNamespace mcontext)
  expandPrefix (SpecialisesRoleType pos s) = SpecialisesRoleType pos <$> expandNamespace s
  expandPrefix (IsInState pos s) = IsInState pos <$> expandNamespace s
  expandPrefix (PublicRole pos s) = PublicRole pos <$> expandNamespace s
  expandPrefix (PublicContext pos s) = PublicContext pos <$> expandNamespace s
  expandPrefix (TypeTimeOnlyContext pos s) = TypeTimeOnlyContext pos <$> expandNamespace s
  expandPrefix (TypeTimeOnlyEnumeratedRole pos s1 s2) = TypeTimeOnlyEnumeratedRole pos <$> expandNamespace s1 <*> expandNamespace s2
  expandPrefix (TypeTimeOnlyCalculatedRole pos s) = TypeTimeOnlyCalculatedRole pos <$> expandNamespace s
  

  expandPrefix x = pure x

instance containsPrefixesVarBinding :: ContainsPrefixes VarBinding where
  expandPrefix (VarBinding s step) = VarBinding s <$> expandPrefix step

instance containsPrefixesBinaryStep :: ContainsPrefixes BinaryStep where
  expandPrefix (BinaryStep r@{left, right}) = do
    eleft <- expandPrefix left
    eright <- expandPrefix right
    pure $ BinaryStep r {left = eleft, right = eright}

instance containsPrefixesUnaryStep :: ContainsPrefixes UnaryStep where
  expandPrefix (LogicalNot pos s) = LogicalNot pos <$> expandPrefix s
  expandPrefix (Exists pos s) = Exists pos <$> expandPrefix s
  expandPrefix (FilledBy pos s) = FilledBy pos <$> expandPrefix s
  expandPrefix (Fills pos s) = Fills pos <$> expandPrefix s
  expandPrefix (Available pos s) = Available pos <$> expandPrefix s
  expandPrefix (DurationOperator pos op s) = DurationOperator pos op <$> expandPrefix s

instance containsPrefixesLetStep :: ContainsPrefixes LetStep where
  expandPrefix (LetStep r@{bindings, assignments}) = do
    ebindings <- traverse expandPrefix bindings
    eassignments <- traverse expandPrefix assignments
    pure $ LetStep r {bindings = ebindings, assignments = eassignments}

instance containsPrefixesLetABinding :: ContainsPrefixes LetABinding where
  expandPrefix (Expr varbinding) = Expr <$> expandPrefix varbinding
  expandPrefix (Stat varname assignment) = Stat <$> pure varname <*> expandPrefix assignment

instance containsPrefixesPureLetStep :: ContainsPrefixes PureLetStep where
  expandPrefix (PureLetStep r@{bindings, body}) = do
    ebindings <- traverse expandPrefix bindings
    ebody <- expandPrefix body
    pure $ PureLetStep r {bindings = ebindings, body = ebody}

instance containsPrefixesComputationStep :: ContainsPrefixes ComputationStep where
  expandPrefix (ComputationStep r@{functionName, arguments, computedType}) = do
    efunctionName <- expandNamespace functionName
    earguments <- traverse expandPrefix arguments
    ecomputedType <- expandNamespace computedType
    pure $ ComputationStep r {functionName = efunctionName, arguments = earguments, computedType = ecomputedType}

instance containsPrefixesAssignment :: ContainsPrefixes Assignment where
  expandPrefix (RemoveRole r@{roleExpression}) = do
    eroleExpression <- expandPrefix roleExpression
    pure $ RemoveRole r{roleExpression = eroleExpression}
  expandPrefix (RemoveContext r@{roleExpression}) = do
    eroleExpression <- expandPrefix roleExpression
    pure $ RemoveContext r{roleExpression = eroleExpression}
  expandPrefix (CreateRole r@{contextExpression, localName}) = do
    econtextExpression <- traverse expandPrefix contextExpression
    elocalName <- traverse expandPrefix localName
    pure $ CreateRole r {contextExpression = econtextExpression, localName = elocalName}
  expandPrefix (CreateContext r@{contextExpression, localName}) = do
    econtextExpression <- traverse expandPrefix contextExpression
    elocalName <- traverse expandPrefix localName
    pure $ CreateContext r {contextExpression = econtextExpression, localName = elocalName}
  expandPrefix (CreateContext_ r@{roleExpression}) = do
    eroleExpression <- expandPrefix roleExpression
    pure $ CreateContext_ r {roleExpression = eroleExpression}
  expandPrefix (Move r@{roleExpression, contextExpression}) = do
    econtextExpression <- traverse expandPrefix contextExpression
    eroleExpression <- expandPrefix roleExpression
    pure $ Move r{roleExpression = eroleExpression, contextExpression = econtextExpression}
  expandPrefix (Bind r@{bindingExpression, contextExpression}) = do
    ebindingExpression <- expandPrefix bindingExpression
    econtextExpression <- traverse expandPrefix contextExpression
    pure $ Bind r {bindingExpression = ebindingExpression, contextExpression = econtextExpression}
  expandPrefix (Bind_ r@{bindingExpression, binderExpression}) = do
    ebindingExpression <- expandPrefix bindingExpression
    ebinderExpression <- expandPrefix binderExpression
    pure $ Bind_ r {binderExpression = ebinderExpression, bindingExpression = ebindingExpression}
  expandPrefix (Unbind r@{bindingExpression}) = do
    ebindingExpression <- expandPrefix bindingExpression
    pure $ Unbind r {bindingExpression = ebindingExpression}
  expandPrefix (Unbind_ r@{bindingExpression, binderExpression}) = do
    ebindingExpression <- expandPrefix bindingExpression
    ebinderExpression <- expandPrefix binderExpression
    pure $ Unbind_ r {bindingExpression = ebindingExpression, binderExpression = ebinderExpression}
  expandPrefix (DeleteRole r@{contextExpression}) = do
    econtextExpression <- traverse expandPrefix contextExpression
    pure $ DeleteRole r{contextExpression = econtextExpression}
  expandPrefix (DeleteContext r@{contextExpression}) = do
    econtextExpression <- traverse expandPrefix contextExpression
    pure $ DeleteContext r{contextExpression = econtextExpression}
  expandPrefix (DeleteProperty r@{roleExpression}) = do
    eroleExpression <- traverse expandPrefix roleExpression
    pure $ DeleteProperty r{roleExpression = eroleExpression}
  expandPrefix (PropertyAssignment r@{valueExpression, roleExpression}) = do
    eroleExpression <- traverse expandPrefix roleExpression
    evalueExpression <- expandPrefix valueExpression
    pure $ PropertyAssignment r {valueExpression = evalueExpression, roleExpression = eroleExpression}
  expandPrefix (CreateFile r@{roleExpression, fileNameExpression, contentExpression}) = do
    eRoleExpression <- traverse expandPrefix roleExpression
    eContentExpression <- expandPrefix contentExpression
    eFileNameExpression <- expandPrefix fileNameExpression
    pure $ CreateFile r {roleExpression = eRoleExpression, fileNameExpression = eFileNameExpression, contentExpression = eContentExpression}
  expandPrefix (ExternalEffect r@{effectName, arguments}) = do
    eeffectName <- expandNamespace effectName
    earguments <- traverse expandPrefix arguments
    pure $ ExternalEffect r {effectName = eeffectName, arguments = earguments}
  expandPrefix (ExternalDestructiveEffect r@{effectName, arguments}) = do
    eeffectName <- expandNamespace effectName
    earguments <- traverse expandPrefix arguments
    pure $ ExternalEffect r {effectName = eeffectName, arguments = earguments}

instance containsPrefixesStateQualifiedPart :: ContainsPrefixes StateQualifiedPart where
  expandPrefix (R r) = R <$> (expandPrefix r)
  expandPrefix (P p) = P <$> (expandPrefix p)
  expandPrefix (AC (ActionE r@{subject, object, state, effect})) = do
    subject' <- expandPrefix subject
    object' <- expandPrefix object
    state' <- expandPrefix state
    effect' <- expandPrefix effect
    pure (AC (ActionE r {subject = subject', object = object', state = state', effect = effect'}))
  expandPrefix (CA (ContextActionE r@{subject, object, state, effect})) = do
    subject' <- expandPrefix subject
    state' <- expandPrefix state
    effect' <- expandPrefix effect
    pure (CA (ContextActionE r {subject = subject', state = state', effect = effect'}))
  expandPrefix (SO (SelfOnly r@{subject, object, state})) = do
    subject' <- expandPrefix subject
    object' <- expandPrefix object
    state' <- expandPrefix state
    pure (SO (SelfOnly r {subject = subject', object = object', state = state'}))
  expandPrefix (N (NotificationE r@{user, transition, message, object})) = do
    user' <- expandPrefix user
    transition' <- expandPrefix transition
    message' <- expandPrefix message
    object' <- traverse expandPrefix object
    pure (N (NotificationE r {user = user', transition = transition', message = message', object = object'}))
  expandPrefix (AE (AutomaticEffectE r@{subject, object, transition, effect})) = do
    subject' <- expandPrefix subject
    object' <- traverse expandPrefix object
    transition' <- expandPrefix transition
    effect' <- expandPrefix effect
    pure (AE (AutomaticEffectE r {subject = subject', object = object', transition = transition', effect = effect'}))
  expandPrefix (SUBSTATE s) = SUBSTATE <$> expandPrefix s

instance expandPrefixRoleVerbE :: ContainsPrefixes RoleVerbE where
  expandPrefix (RoleVerbE r@{subject, object, state}) = do
    subject' <- expandPrefix subject
    object' <- expandPrefix object
    state' <- expandPrefix state
    pure (RoleVerbE r {subject = subject', object = object', state = state'})

instance expandPrefixPropertyE :: ContainsPrefixes PropertyVerbE where
  expandPrefix (PropertyVerbE r@{subject, object, state, propsOrView}) = do
    subject' <- expandPrefix subject
    object' <- expandPrefix object
    state' <- expandPrefix state
    propsOrView' <- expandPrefix propsOrView
    pure (PropertyVerbE r {subject = subject', object = object', state = state', propsOrView = propsOrView'})

instance containsPrefixesPropsOrView :: ContainsPrefixes PropsOrView where
  expandPrefix (View s) = View <$> (expandNamespace s)
  expandPrefix x = pure x

instance containsPrefixesRoleIdentification :: ContainsPrefixes RoleIdentification where
  expandPrefix r@(ExplicitRole ct roleType pos) = do
    roleType' <- expandPrefix roleType
    pure (ExplicitRole ct roleType' pos)
  expandPrefix (ImplicitRole ct s) = expandPrefix s >>= pure <<< ImplicitRole ct

instance containsPrefixesRoleType :: ContainsPrefixes RoleType where
  expandPrefix (ENR (EnumeratedRoleType s)) = expandNamespace s >>= pure <<< ENR <<< EnumeratedRoleType
  expandPrefix (CR (CalculatedRoleType s)) = expandNamespace s >>= pure <<< CR <<< CalculatedRoleType

instance containsPrefixesStateSpecification :: ContainsPrefixes StateSpecification where
  expandPrefix c@(ContextState _ _) = pure c
  expandPrefix (SubjectState rid spath) = SubjectState <$> expandPrefix rid <*> (traverse expandNamespace spath)
  expandPrefix (ObjectState rid spath) = ObjectState <$> expandPrefix rid <*> (traverse expandNamespace spath)

instance containsPrefixesStateTransitionE :: ContainsPrefixes StateTransitionE where
  expandPrefix (Entry s) = Entry <$> expandPrefix s
  expandPrefix (Exit s) = Exit <$> expandPrefix s

instance containsPrefixesSentence :: ContainsPrefixes Sentence where
  expandPrefix (Sentence sparts) = traverse expandPrefix sparts >>= pure <<< Sentence

instance containsPrefixesSentencPart :: ContainsPrefixes SentencePart where
  expandPrefix (HR s) = pure $ HR s
  expandPrefix (CP c) = CP <$> expandPrefix c

instance containsprefixesStatements :: ContainsPrefixes Statements where
  expandPrefix (Let stp) = Let <$> expandPrefix stp
  expandPrefix (Statements stmts) = Statements <$> traverse expandPrefix stmts

instance containsPrefixesStateE :: ContainsPrefixes StateE where
  expandPrefix (StateE r@{condition, stateParts, subStates}) = do
    condition' <- expandPrefix condition
    stateParts' <- traverse expandPrefix stateParts
    subStates' <- traverse expandPrefix subStates
    pure (StateE r {condition = condition', stateParts = stateParts', subStates = subStates'})

instance containsPrefixesCalculation :: ContainsPrefixes Calculation where
  expandPrefix (S stp isFunctional) = flip S isFunctional <$> expandPrefix stp
  expandPrefix q@(Q _) = pure q

instance containsPrefixesScreenE :: ContainsPrefixes ScreenE where
  expandPrefix (ScreenE rec@{tabs, rows, columns, subject}) = do
    subject' <- expandPrefix subject
    tabs' <- case tabs of 
      Nothing -> pure Nothing
      Just tbs -> Just <$> traverse expandPrefix tbs
    rows' <- case rows of
      Nothing -> pure Nothing
      Just rws -> Just <$> traverse expandPrefix rws
    columns' <- case columns of
      Nothing -> pure Nothing
      Just cols -> Just <$> traverse expandPrefix cols
    pure $ ScreenE rec {rows = rows', columns = columns', subject = subject', tabs = tabs'}

instance containsPrefixesTableE :: ContainsPrefixes TableE where
  expandPrefix (TableE wcf)= TableE <$> expandPrefixWidgetCommonFields wcf

instance containsPrefixesFormE :: ContainsPrefixes FormE where
  expandPrefix (FormE wcf)= FormE <$> expandPrefixWidgetCommonFields wcf

expandPrefixWidgetCommonFields :: WidgetCommonFields-> PhaseTwo WidgetCommonFields
expandPrefixWidgetCommonFields cf@{perspective} = do
    perspective' <- expandPrefix perspective
    pure cf {perspective = perspective'}

instance containsPrefixesRowE :: ContainsPrefixes RowE where
  expandPrefix (RowE scrEls) = RowE <$> (traverse expandPrefix scrEls)

instance containsPrefixesColumnE :: ContainsPrefixes ColumnE where
  expandPrefix (ColumnE scrEls) = ColumnE <$> (traverse expandPrefix scrEls)

instance containsPrefixesTabE :: ContainsPrefixes TabE where
  expandPrefix (TabE title scrEls) = do
    scrEls' <- traverse expandPrefix scrEls
    pure $ TabE title scrEls'

instance containsPrefixesScreenElement :: ContainsPrefixes ScreenElement where
  expandPrefix (RowElement r) = RowElement <$> expandPrefix r
  expandPrefix (ColumnElement r) = ColumnElement <$> expandPrefix r
  expandPrefix (TableElement r) = TableElement <$> expandPrefix r
  expandPrefix (FormElement r) = FormElement <$> expandPrefix r
