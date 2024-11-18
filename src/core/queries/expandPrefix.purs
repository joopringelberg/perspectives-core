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

import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.Identifiers (isTypeUri, typeUri2typeNameSpace)
import Perspectives.Parsing.Arc.AST (ActionE(..), AuthorOnly(..), AutomaticEffectE(..), ChatE(..), ColumnE(..), ContextActionE(..), FormE(..), MarkDownE(..), NotificationE(..), PropertyVerbE(..), PropsOrView(..), RoleIdentification(..), RoleVerbE(..), RowE(..), ScreenE(..), ScreenElement(..), SelfOnly(..), SentenceE(..), SentencePartE(..), StateE(..), StateQualifiedPart(..), StateSpecification(..), StateTransitionE(..), TabE(..), TableE(..), WidgetCommonFields)
import Perspectives.Parsing.Arc.Expression.AST (BinaryStep(..), ComputationStep(..), ComputedType(..), PureLetStep(..), SimpleStep(..), Step(..), UnaryStep(..), VarBinding(..))
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwo, expandNamespace)
import Perspectives.Parsing.Arc.Statement.AST (Assignment(..), LetABinding(..), LetStep(..), Statements(..))
import Perspectives.Query.QueryTypes (Calculation(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), DomeinFileId(..), EnumeratedRoleType(..), RoleType(..))
import Prelude (class Monad, bind, discard, flip, pure, unit, void, ($), (<$>), (<*>), (<<<), (>>=))

type SymbolHandler m = String -> m String

f :: forall m. Monad m => String -> ReaderT (SymbolHandler m) m String
f s = do 
  g <- ask
  lift (g s)

-- | Just apply expandNamespace to all identifiers.
expandPrefix :: forall s. ScanSymbols s => s -> PhaseTwo s
expandPrefix s = runReaderT (scan s) expandNamespace

-- | On detecting a namespace for which no model is yet available, load that model.
ensureModel :: forall s. ScanSymbols s => s -> MonadPerspectives s
ensureModel s = runReaderT (scan s) 
  \symbol -> if isTypeUri symbol 
    then do 
      case typeUri2typeNameSpace symbol of
        Just namespace -> void $ retrieveDomeinFile (DomeinFileId namespace)
        Nothing -> pure unit
      pure symbol
    else pure symbol

class ScanSymbols s where
  scan :: forall m. Monad m => s -> ReaderT (SymbolHandler m) m s

instance containsPrefixesStep :: ScanSymbols Step where
  scan (Simple s) = Simple <$> scan s
  scan (Binary s) = Binary <$> scan s 
  scan (Unary s) = Unary <$> scan s
  scan (PureLet s) = PureLet <$> scan s
  scan (Computation s) = Computation <$> scan s

instance containsPrefixesSimpleStep :: ScanSymbols SimpleStep where
  scan (ArcIdentifier pos s) = ArcIdentifier pos <$> f s
  scan (ContextTypeIndividual pos s) = ContextTypeIndividual pos <$> f s
  scan (RoleTypeIndividual pos s) = RoleTypeIndividual pos <$> f s
  scan (Filler pos mcontext) = Filler pos <$> (traverse f mcontext)
  scan (Filled pos s mcontext) = Filled pos <$> f s <*> (traverse f mcontext)
  scan (SpecialisesRoleType pos s) = SpecialisesRoleType pos <$> f s
  scan (IsInState pos s) = IsInState pos <$> f s
  scan (PublicRole pos s) = PublicRole pos <$> f s
  scan (PublicContext pos s) = PublicContext pos <$> f s
  scan (TypeTimeOnlyContext pos s) = TypeTimeOnlyContext pos <$> f s
  scan (TypeTimeOnlyEnumeratedRole pos s1 s2) = TypeTimeOnlyEnumeratedRole pos <$> f s1 <*> f s2
  scan (TypeTimeOnlyCalculatedRole pos s) = TypeTimeOnlyCalculatedRole pos <$> f s
  

  scan x = pure x

instance containsPrefixesVarBinding :: ScanSymbols VarBinding where
  scan (VarBinding s step) = VarBinding s <$> scan step

instance containsPrefixesBinaryStep :: ScanSymbols BinaryStep where
  scan (BinaryStep r@{left, right}) = do
    eleft <- scan left
    eright <- scan right
    pure $ BinaryStep r {left = eleft, right = eright}

instance containsPrefixesUnaryStep :: ScanSymbols UnaryStep where
  scan (LogicalNot pos s) = LogicalNot pos <$> scan s
  scan (Exists pos s) = Exists pos <$> scan s
  scan (FilledBy pos s) = FilledBy pos <$> scan s
  scan (Fills pos s) = Fills pos <$> scan s
  scan (Available pos s) = Available pos <$> scan s
  scan (DurationOperator pos op s) = DurationOperator pos op <$> scan s
  scan (ContextIndividual pos tp s) = ContextIndividual pos <$> f tp <*> scan s
  scan (RoleIndividual pos tp s) = RoleIndividual pos <$> f tp <*> scan s

instance containsPrefixesLetStep :: ScanSymbols LetStep where
  scan (LetStep r@{bindings, assignments}) = do
    ebindings <- traverse scan bindings
    eassignments <- traverse scan assignments
    pure $ LetStep r {bindings = ebindings, assignments = eassignments}

instance containsPrefixesLetABinding :: ScanSymbols LetABinding where
  scan (Expr varbinding) = Expr <$> scan varbinding
  scan (Stat varname assignment) = Stat <$> pure varname <*> scan assignment

instance containsPrefixesPureLetStep :: ScanSymbols PureLetStep where
  scan (PureLetStep r@{bindings, body}) = do
    ebindings <- traverse scan bindings
    ebody <- scan body
    pure $ PureLetStep r {bindings = ebindings, body = ebody}

instance containsPrefixesComputationStep :: ScanSymbols ComputationStep where
  scan (ComputationStep r@{functionName, arguments, computedType}) = do
    efunctionName <- f functionName
    earguments <- traverse scan arguments
    ecomputedType <- scan computedType
    pure $ ComputationStep r {functionName = efunctionName, arguments = earguments, computedType = ecomputedType}

instance ScanSymbols ComputedType where
  scan (OtherType s) = OtherType <$> f s
  scan x = pure x

instance containsPrefixesAssignment :: ScanSymbols Assignment where
  scan (RemoveRole r@{roleExpression}) = do
    eroleExpression <- scan roleExpression
    pure $ RemoveRole r{roleExpression = eroleExpression}
  scan (RemoveContext r@{roleExpression}) = do
    eroleExpression <- scan roleExpression
    pure $ RemoveContext r{roleExpression = eroleExpression}
  scan (CreateRole r@{contextExpression, localName}) = do
    econtextExpression <- traverse scan contextExpression
    elocalName <- traverse scan localName
    pure $ CreateRole r {contextExpression = econtextExpression, localName = elocalName}
  scan (CreateContext r@{contextExpression, localName}) = do
    econtextExpression <- traverse scan contextExpression
    elocalName <- traverse scan localName
    pure $ CreateContext r {contextExpression = econtextExpression, localName = elocalName}
  scan (CreateContext_ r@{roleExpression}) = do
    eroleExpression <- scan roleExpression
    pure $ CreateContext_ r {roleExpression = eroleExpression}
  scan (Move r@{roleExpression, contextExpression}) = do
    econtextExpression <- traverse scan contextExpression
    eroleExpression <- scan roleExpression
    pure $ Move r{roleExpression = eroleExpression, contextExpression = econtextExpression}
  scan (Bind r@{bindingExpression, contextExpression}) = do
    ebindingExpression <- scan bindingExpression
    econtextExpression <- traverse scan contextExpression
    pure $ Bind r {bindingExpression = ebindingExpression, contextExpression = econtextExpression}
  scan (Bind_ r@{bindingExpression, binderExpression}) = do
    ebindingExpression <- scan bindingExpression
    ebinderExpression <- scan binderExpression
    pure $ Bind_ r {binderExpression = ebinderExpression, bindingExpression = ebindingExpression}
  scan (Unbind r@{bindingExpression}) = do
    ebindingExpression <- scan bindingExpression
    pure $ Unbind r {bindingExpression = ebindingExpression}
  scan (Unbind_ r@{bindingExpression, binderExpression}) = do
    ebindingExpression <- scan bindingExpression
    ebinderExpression <- scan binderExpression
    pure $ Unbind_ r {bindingExpression = ebindingExpression, binderExpression = ebinderExpression}
  scan (DeleteRole r@{contextExpression}) = do
    econtextExpression <- traverse scan contextExpression
    pure $ DeleteRole r{contextExpression = econtextExpression}
  scan (DeleteContext r@{contextExpression}) = do
    econtextExpression <- traverse scan contextExpression
    pure $ DeleteContext r{contextExpression = econtextExpression}
  scan (DeleteProperty r@{roleExpression}) = do
    eroleExpression <- traverse scan roleExpression
    pure $ DeleteProperty r{roleExpression = eroleExpression}
  scan (PropertyAssignment r@{valueExpression, roleExpression}) = do
    eroleExpression <- traverse scan roleExpression
    evalueExpression <- scan valueExpression
    pure $ PropertyAssignment r {valueExpression = evalueExpression, roleExpression = eroleExpression}
  scan (CreateFile r@{roleExpression, fileNameExpression, contentExpression}) = do
    eRoleExpression <- traverse scan roleExpression
    eContentExpression <- scan contentExpression
    eFileNameExpression <- scan fileNameExpression
    pure $ CreateFile r {roleExpression = eRoleExpression, fileNameExpression = eFileNameExpression, contentExpression = eContentExpression}
  scan (ExternalEffect r@{effectName, arguments}) = do
    eeffectName <- f effectName
    earguments <- traverse scan arguments
    pure $ ExternalEffect r {effectName = eeffectName, arguments = earguments}
  scan (ExternalDestructiveEffect r@{effectName, arguments}) = do
    eeffectName <- f effectName
    earguments <- traverse scan arguments
    pure $ ExternalEffect r {effectName = eeffectName, arguments = earguments}

instance containsPrefixesStateQualifiedPart :: ScanSymbols StateQualifiedPart where
  scan (R r) = R <$> (scan r)
  scan (P p) = P <$> (scan p)
  scan (AC (ActionE r@{subject, object, state, effect})) = do
    subject' <- scan subject
    object' <- scan object
    state' <- scan state
    effect' <- scan effect
    pure (AC (ActionE r {subject = subject', object = object', state = state', effect = effect'}))
  scan (CA (ContextActionE r@{subject, object, state, effect})) = do
    subject' <- scan subject
    state' <- scan state
    effect' <- scan effect
    pure (CA (ContextActionE r {subject = subject', state = state', effect = effect'}))
  scan (SO (SelfOnly r@{subject, object, state})) = do
    subject' <- scan subject
    object' <- scan object
    state' <- scan state
    pure (SO (SelfOnly r {subject = subject', object = object', state = state'}))
  scan (PO (AuthorOnly r@{subject, object, state})) = do
    subject' <- scan subject
    object' <- scan object
    state' <- scan state
    pure (PO (AuthorOnly r {subject = subject', object = object', state = state'}))
  scan (N (NotificationE r@{user, transition, message, object})) = do
    user' <- scan user
    transition' <- scan transition
    message' <- scan message
    object' <- traverse scan object
    pure (N (NotificationE r {user = user', transition = transition', message = message', object = object'}))
  scan (AE (AutomaticEffectE r@{subject, object, transition, effect})) = do
    subject' <- scan subject
    object' <- traverse scan object
    transition' <- scan transition
    effect' <- scan effect
    pure (AE (AutomaticEffectE r {subject = subject', object = object', transition = transition', effect = effect'}))
  scan (SUBSTATE s) = SUBSTATE <$> scan s

instance ScanSymbols SentenceE where
  scan (SentenceE {sentence, parts}) = do 
    parts' <- traverse scan parts
    pure $ SentenceE {sentence, parts: parts'}

instance containsPrefixesSentencPart :: ScanSymbols SentencePartE where
  scan (HRpart s) = pure $ HRpart s
  scan (CPpart c) = CPpart <$> scan c

instance expandPrefixRoleVerbE :: ScanSymbols RoleVerbE where
  scan (RoleVerbE r@{subject, object, state}) = do
    subject' <- scan subject
    object' <- scan object
    state' <- scan state
    pure (RoleVerbE r {subject = subject', object = object', state = state'})

instance expandPrefixPropertyE :: ScanSymbols PropertyVerbE where
  scan (PropertyVerbE r@{subject, object, state, propsOrView}) = do
    subject' <- scan subject
    object' <- scan object
    state' <- scan state
    propsOrView' <- scan propsOrView
    pure (PropertyVerbE r {subject = subject', object = object', state = state', propsOrView = propsOrView'})

instance containsPrefixesPropsOrView :: ScanSymbols PropsOrView where
  scan (View s) = View <$> (f s)
  scan x = pure x

instance containsPrefixesRoleIdentification :: ScanSymbols RoleIdentification where
  scan r@(ExplicitRole ct roleType pos) = do
    roleType' <- scan roleType
    pure (ExplicitRole ct roleType' pos)
  scan (ImplicitRole ct s) = scan s >>= pure <<< ImplicitRole ct

instance containsPrefixesRoleType :: ScanSymbols RoleType where
  scan (ENR (EnumeratedRoleType s)) = f s >>= pure <<< ENR <<< EnumeratedRoleType
  scan (CR (CalculatedRoleType s)) = f s >>= pure <<< CR <<< CalculatedRoleType

instance containsPrefixesStateSpecification :: ScanSymbols StateSpecification where
  scan c@(ContextState _ _) = pure c
  scan (SubjectState rid spath) = SubjectState <$> scan rid <*> (traverse f spath)
  scan (ObjectState rid spath) = ObjectState <$> scan rid <*> (traverse f spath)

instance containsPrefixesStateTransitionE :: ScanSymbols StateTransitionE where
  scan (Entry s) = Entry <$> scan s
  scan (Exit s) = Exit <$> scan s

instance containsprefixesStatements :: ScanSymbols Statements where
  scan (Let stp) = Let <$> scan stp
  scan (Statements stmts) = Statements <$> traverse scan stmts

instance containsPrefixesStateE :: ScanSymbols StateE where
  scan (StateE r@{condition, stateParts, subStates}) = do
    condition' <- scan condition
    stateParts' <- traverse scan stateParts
    subStates' <- traverse scan subStates
    pure (StateE r {condition = condition', stateParts = stateParts', subStates = subStates'})

instance containsPrefixesCalculation :: ScanSymbols Calculation where
  scan (S stp isFunctional) = flip S isFunctional <$> scan stp
  scan q@(Q _) = pure q

instance containsPrefixesScreenE :: ScanSymbols ScreenE where
  scan (ScreenE rec@{tabs, rows, columns, subject}) = do
    subject' <- scan subject
    tabs' <- case tabs of 
      Nothing -> pure Nothing
      Just tbs -> Just <$> traverse scan tbs
    rows' <- case rows of
      Nothing -> pure Nothing
      Just rws -> Just <$> traverse scan rws
    columns' <- case columns of
      Nothing -> pure Nothing
      Just cols -> Just <$> traverse scan cols
    pure $ ScreenE rec {rows = rows', columns = columns', subject = subject', tabs = tabs'}

instance containsPrefixesTableE :: ScanSymbols TableE where
  scan (TableE wcf)= TableE <$> expandPrefixWidgetCommonFields wcf

instance containsPrefixesFormE :: ScanSymbols FormE where
  scan (FormE wcf)= FormE <$> expandPrefixWidgetCommonFields wcf

instance ScanSymbols MarkDownE where
  scan (MarkDownConstant {text, condition, context, start, end}) = do 
    condition' <- traverse scan condition
    pure $ MarkDownConstant {text, condition:condition', context, start, end}
  scan (MarkDownExpression {text, condition, context, start, end}) = do 
    text' <- scan text
    condition' <- traverse scan condition
    pure $ MarkDownExpression {text:text', condition:condition', context, start, end}
  scan (MarkDownPerspective {widgetFields, condition, start, end}) = do 
    widgetFields' <- expandPrefixWidgetCommonFields widgetFields
    condition' <- traverse f condition
    pure $ MarkDownPerspective {widgetFields:widgetFields', condition:condition', start, end}


expandPrefixWidgetCommonFields :: forall m. Monad m => WidgetCommonFields-> ReaderT (SymbolHandler m) m WidgetCommonFields
expandPrefixWidgetCommonFields cf@{perspective} = do
    perspective' <- scan perspective
    pure cf {perspective = perspective'}

instance containsPrefixesRowE :: ScanSymbols RowE where
  scan (RowE scrEls) = RowE <$> (traverse scan scrEls)

instance containsPrefixesColumnE :: ScanSymbols ColumnE where
  scan (ColumnE scrEls) = ColumnE <$> (traverse scan scrEls)

instance containsPrefixesTabE :: ScanSymbols TabE where
  scan (TabE title isDefault scrEls) = do
    scrEls' <- traverse scan scrEls
    pure $ TabE title isDefault scrEls'

instance ScanSymbols ChatE where
  scan (ChatE {chatRole, messagesProperty, mediaProperty, start, end}) = do
    chatRole' <- scan chatRole
    messagesProperty' <- f messagesProperty
    mediaProperty' <- f mediaProperty
    pure $ ChatE{ chatRole: chatRole', messagesProperty: messagesProperty', mediaProperty: mediaProperty', start, end}

instance containsPrefixesScreenElement :: ScanSymbols ScreenElement where
  scan (RowElement r) = RowElement <$> scan r
  scan (ColumnElement r) = ColumnElement <$> scan r
  scan (TableElement r) = TableElement <$> scan r
  scan (FormElement r) = FormElement <$> scan r
  scan (MarkDownElement r) = MarkDownElement <$> scan r
  scan (ChatElement r) = ChatElement <$> scan r
