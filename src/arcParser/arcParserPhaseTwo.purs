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

module Perspectives.Parsing.Arc.PhaseTwo where

import Perspectives.Parsing.Arc.PhaseTwoDefs

import Control.Monad.Except (lift, throwError)
import Control.Monad.State (gets, modify)
import Data.Array (cons, elemIndex, length, group)
import Data.Array (filter, fromFoldable) as ARR
import Data.Array.NonEmpty (NonEmptyArray, head) as ARNE
import Data.Array.NonEmpty (toArray)
import Data.Array.Partial (head) as ARRP
import Data.Either (Either(..))
import Data.Foldable (foldl, for_)
import Data.Lens (over) as LN
import Data.Lens.Record (prop)
import Data.List (List(..), filter, findIndex, foldM, head, sort)
import Data.Map (insert, lookup, empty) as MAP
import Data.Map (toUnfoldable)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Foreign.Object (Object, insert, keys, lookup, singleton)
import Partial.Unsafe (unsafePartial)
import Perspectives.Data.EncodableMap (EncodableMap(..))
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.Identifiers (Namespace, areLastSegmentsOf, isQualifiedWithDomein)
import Perspectives.Parsing.Arc.AST (ActionE(..), AutomaticEffectE(..), ContextE(..), ContextPart(..), NotificationE(..), PropertyE(..), PropertyPart(..), PropertyVerbE(..), PropsOrView(..), RoleE(..), RolePart(..), RoleVerbE(..), StateE(..), StateQualifiedPart(..), StateTransitionE(..), ViewE(..))
import Perspectives.Parsing.Arc.Expression.AST (Step) as Expr
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.ExpandPrefix (expandPrefix)
import Perspectives.Query.QueryTypes (Calculation(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..), defaultCalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole(..), defaultCalculatedRole)
import Perspectives.Representation.Class.Property (Property(..)) as Property
import Perspectives.Representation.Class.Role (Role(..))
import Perspectives.Representation.Context (Context(..), defaultContext, ContextRecord)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..), defaultEnumeratedProperty)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..), defaultEnumeratedRole)
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..))
import Perspectives.Representation.SideEffect (SideEffect(..))
import Perspectives.Representation.State (State(..), StateIdentifier(..), StateRecord, constructState)
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleKind(..), RoleType(..), ViewType(..), externalRoleType_, roletype2string)
import Perspectives.Representation.View (View(..)) as VIEW
import Prelude (Unit, bind, discard, pure, void, ($), (<$>), (<<<), (<>), (==), (>>=))

-------------------
traverseDomain :: ContextE -> Namespace -> PhaseTwo DomeinFile
traverseDomain c ns = do
  -- Traverse the model parse tree and construct a DomeinFileRecord in PhaseTwoState.
  (Context {_id}) <- traverseContextE c ns

  -- Add perspectives to PhaseTwoState.
  handlePostponedStateQualifiedParts

  -- Modify the DomeinFileRecord in PhaseTwoState so that all perspectives
  -- are stored in their subject roles.
  perspectives <- lift $ gets _.perspectives
  (perRole :: Array (ARNE.NonEmptyArray (Tuple (Tuple RoleType Expr.Step) Perspective))) <- pure $ group (toUnfoldable perspectives)
  modifyDF \domeinFileRecord -> foldl
    (\dfr@{enumeratedRoles, calculatedRoles} perspectivesForOneRole -> let
      (rname :: String) = roletype2string $ fst $ fst $ ARNE.head perspectivesForOneRole
      -- modify the role in the domeinfile
      in case lookup rname enumeratedRoles of
        Nothing -> case lookup rname calculatedRoles of
          Nothing -> dfr
          Just (CalculatedRole cr) -> dfr { calculatedRoles =
            insert
              rname
              (CalculatedRole cr { perspectives = toArray (snd <$> perspectivesForOneRole)})
              calculatedRoles
          }
        Just (EnumeratedRole er) -> dfr { enumeratedRoles =
          insert
            rname
            (EnumeratedRole er { perspectives = toArray (snd <$> perspectivesForOneRole)})
            enumeratedRoles})
    domeinFileRecord
    perRole
  domeinFileRecord <- getDF
  pure $ DomeinFile (domeinFileRecord {_id = unwrap _id})

-- | Traverse the members of the ContextE AST type to construct a new Context type
-- | and insert it into a DomeinFileRecord.
traverseContextE :: ContextE -> Namespace -> PhaseTwo Context
traverseContextE (ContextE {id, kindOfContext, contextParts, pos}) ns = do
  context <- pure $ defaultContext (addNamespace ns id) id kindOfContext (if ns == "model:" then Nothing else (Just ns)) pos
  unsafePartial $ withNamespaces
    (filter (case _ of
      (PREFIX _ _) -> true
      otherwise -> false) contextParts)
    do
      contextParts' <- case (head (filter (case _ of
        RE (RoleE{id:rid}) -> rid == "External"
        otherwise -> false) contextParts)) of
          Nothing -> pure $ Cons (RE (RoleE{id: "External", kindOfRole: ExternalRole, roleParts: Nil, pos})) contextParts
          otherwise -> pure contextParts
      -- RoleE elements that are BotRoles are handled last, so we can lookup their users.
      context' <- foldM handleParts context (sort contextParts')
      -- context' <- foldM handleParts context contextParts'
      modifyDF (\domeinFile -> addContextToDomeinFile context' domeinFile)
      pure context'

  where

    -- Construct members for the new Context type according to the type of
    -- parts found in the ContextE AST type. Insert these members into the new Context.
    handleParts :: Context -> ContextPart -> PhaseTwo Context
    -- Construct a nested Context.
    handleParts contextUnderConstruction (CE c) = do
      subContext <- traverseContextE c (addNamespace ns id)
      pure (subContext `insertInto` contextUnderConstruction)

    -- Construct a Role
    handleParts contextUnderConstruction (RE r) = do
      role <- traverseRoleE r (addNamespace ns id)
      pure (unsafePartial (role `insertRoleInto` contextUnderConstruction))

    -- Prefixes are handled earlier, so this can be a no-op
    handleParts contextUnderConstruction (PREFIX pre model) = pure contextUnderConstruction

    handleParts (Context contextUnderConstruction@{contextAspects}) (ContextAspect contextName pos') = do
      expandedAspect <- expandNamespace contextName
      if isQualifiedWithDomein expandedAspect
        then pure (Context $ contextUnderConstruction {contextAspects = cons (ContextType expandedAspect) contextAspects})
        else throwError $ NotWellFormedName pos' contextName

    handleParts (Context contextUnderConstruction) (IndexedContext indexedName pos') = do
      qualifiedIndexedName <- expandNamespace indexedName
      pure (Context $ contextUnderConstruction {indexedContext = Just $ ContextInstance qualifiedIndexedName})

    handleParts (Context contextUnderConstruction) (STATE s@(StateE{id:stateId})) = do
      state@(State{id:ident}) <- traverseStateE s (AllStates $ addNamespace ns stateId)
      pure (ident `insertStateInto` contextUnderConstruction)
      where
        insertStateInto :: StateIdentifier -> ContextRecord -> Context
        insertStateInto stateIdentifier cr@{states} = if isJust $ elemIndex stateIdentifier states then Context cr else Context $ cr {states = cons stateIdentifier states}

    addContextToDomeinFile :: Context -> DomeinFileRecord -> DomeinFileRecord
    addContextToDomeinFile c@(Context{_id: (ContextType ident)}) domeinFile = LN.over
      (prop (SProxy :: SProxy "contexts"))
      (insert ident c)
      domeinFile

    -- Insert a sub-Context type into a Context type.
    insertInto :: Context -> Context -> Context
    insertInto (Context{_id}) (Context cr@{nestedContexts}) = Context $ cr {nestedContexts = cons _id nestedContexts}

    -- Insert a Role type into a Context type. This function is partial, because we ignore
    -- BotRole (a BotRole is represented with the UserRole that it serves).
    insertRoleInto :: Partial => Role -> Context -> Context
    insertRoleInto (E (EnumeratedRole {_id, kindOfRole})) c = case kindOfRole, c of
      RoleInContext, (Context cr@{rolInContext}) -> Context $ cr {rolInContext = cons (ENR _id) rolInContext}
      ContextRole, (Context cr@{contextRol}) -> Context $ cr {contextRol = cons (ENR _id) contextRol}
      ExternalRole, ctxt -> ctxt
      -- We may have added the user before, on handling his BotRole.
      UserRole, (Context cr@{gebruikerRol}) -> Context $ cr {gebruikerRol = case elemIndex (ENR _id) gebruikerRol of
        Nothing -> cons (ENR _id) gebruikerRol
        (Just _) -> gebruikerRol}

    insertRoleInto (C (CalculatedRole {_id, kindOfRole})) c = case kindOfRole, c of
      RoleInContext, (Context cr@{rolInContext}) -> Context $ cr {rolInContext = cons (CR _id) rolInContext}
      ContextRole, (Context cr@{contextRol}) -> Context $ cr {contextRol = cons (CR _id) contextRol}
      UserRole, (Context cr@{gebruikerRol}) -> Context $ cr {gebruikerRol = cons (CR _id) gebruikerRol}
      -- A catchall case that just returns the context. Calculated roles for ExternalRole,
      -- UserRole and BotRole should be ignored.
      _, _ -> c

    addNamespace :: String -> String -> String
    addNamespace ns' ln = if ns == "model:" then (ns' <> ln) else (ns' <> "$" <> ln)

-- | Traverse the members of the RoleE AST type to construct a new Role type
-- | and insert it into a DomeinFileRecord.
traverseRoleE :: RoleE -> Namespace -> PhaseTwo Role
traverseRoleE r ns = if isCalculatedRole r
  then traverseCalculatedRoleE r ns
  else traverseEnumeratedRoleE r ns
  -- else if isBotRole r
  --   then traverseBotRole r ns
  --   else traverseEnumeratedRoleE r ns
  where
    isCalculatedRole :: RoleE -> Boolean
    -- isCalculatedRole _ = true
    isCalculatedRole (RoleE {roleParts}) = (isJust (findIndex (case _ of
      (Calculation _) -> true
      otherwise -> false) roleParts))
    -- isBotRole :: RoleE -> Boolean
    -- isBotRole (RoleE{kindOfRole}) = kindOfRole == BotRole

traverseEnumeratedRoleE :: RoleE -> Namespace -> PhaseTwo Role
traverseEnumeratedRoleE (RoleE {id, kindOfRole, roleParts, pos}) ns = do
  role <- pure (defaultEnumeratedRole (ns <> "$" <> id) id kindOfRole ns pos)
  traverseEnumeratedRoleE_ role roleParts

traverseEnumeratedRoleE_ :: EnumeratedRole -> List RolePart -> PhaseTwo Role
traverseEnumeratedRoleE_ role@(EnumeratedRole{_id:rn, kindOfRole}) roleParts = do
  role' <- foldM (unsafePartial $ handleParts (unwrap rn)) role roleParts
  modifyDF (\domeinFile -> addRoleToDomeinFile (E role') domeinFile)
  pure (E role')

  where

    -- Construct members for the new Role type according to the type of
    -- parts found in the RoleE AST type. Insert these members into the new Role.
    -- The Calculation case is not handled for an EnumeratedRole, hence the Partial constraint.
    handleParts :: Partial => Namespace -> EnumeratedRole -> RolePart -> PhaseTwo EnumeratedRole

    -- PROPERTY
    handleParts roleName roleUnderConstruction (PE pe) = do
      property <- traversePropertyE pe roleName
      pure (property `insertPropertyInto` roleUnderConstruction)

    -- PERSPECTIVE AS STATEQUALIFIEDPARTS
    -- Here we encounter forward references to roles that may be calculated or enumerated;
    -- we can't tell, here. Yet we need to know in order to construct
    -- perspectives and states. Hence we postpone them to the very last.
    -- Notice that no perspectives are created!
    handleParts roleName erole (SQP stateQualifiedParts) = do
      lift $ void $ modify \s@{postponedStateQualifiedParts} -> s {postponedStateQualifiedParts = postponedStateQualifiedParts <> stateQualifiedParts}
      pure erole

    -- VIEW
    handleParts roleName (EnumeratedRole roleUnderConstruction@{views}) (VE pe) = do
      viewType <- traverseViewE pe roleName
      pure (EnumeratedRole $ roleUnderConstruction {views = cons viewType views})

    -- FUNCTIONALATTRIBUTE
    handleParts roleName (EnumeratedRole roleUnderConstruction) (FunctionalAttribute bool) = pure (EnumeratedRole $ roleUnderConstruction {functional = bool})

    -- MANDATORYATTRIBUTE
    handleParts roleName (EnumeratedRole roleUnderConstruction) (MandatoryAttribute bool) = pure (EnumeratedRole $ roleUnderConstruction {mandatory = bool})

    --UNLINKEDATTRIBUTE
    handleParts roleName (EnumeratedRole roleUnderConstruction) UnlinkedAttribute = pure (EnumeratedRole $ roleUnderConstruction {unlinked = true})

    -- FILLEDBYATTRIBUTE
    handleParts roleName (EnumeratedRole roleUnderConstruction@{binding}) (FilledByAttribute bnd) = do
      if bnd == "None"
        then pure (EnumeratedRole $ roleUnderConstruction {binding = UNIVERSAL})
        else do
          -- If the RoleKind is ContextRole, we should construct the name of the External
          -- Role of the binding (which then is a Context)
          expandedBnd <- if kindOfRole == ContextRole
            then expandNamespace (externalRoleType_ bnd)
            else expandNamespace bnd
          -- By default, comma separated types form a SUM wrt binding.
          pure (EnumeratedRole $ roleUnderConstruction {binding = addToADT binding expandedBnd})

    -- ROLEASPECT
    handleParts roleName (EnumeratedRole roleUnderConstruction@{roleAspects}) (RoleAspect a pos') = do
      expandedAspect <- expandNamespace a
      if isQualifiedWithDomein expandedAspect
        then pure (EnumeratedRole $ roleUnderConstruction {roleAspects = cons (EnumeratedRoleType expandedAspect) roleAspects})
        else throwError $ NotWellFormedName pos' a

    -- INDEXEDROLE
    handleParts roleName (EnumeratedRole roleUnderConstruction) (IndexedRole indexedName pos') = do
      expandedIndexedName <- expandNamespace indexedName
      pure (EnumeratedRole $ roleUnderConstruction {indexedRole = Just (RoleInstance expandedIndexedName)})

    -- We we add roleName as another disjunct of a sum type.
    -- `roleName` should be qualified.
    -- Notice that we treat roles as units here; not as collections of properties!
    addToADT :: ADT EnumeratedRoleType -> String -> ADT EnumeratedRoleType
    addToADT adt roleName = case adt of
      EMPTY -> ST $ EnumeratedRoleType roleName
      SUM terms -> SUM $ cons (ST $ EnumeratedRoleType roleName) terms
      p@(PROD _) -> SUM [p, ST $ EnumeratedRoleType roleName]
      s@(ST _) -> SUM [s, ST $ EnumeratedRoleType roleName]
      UNIVERSAL -> ST $ EnumeratedRoleType roleName

    -- We we add roleName as another conjunct of a product type.
    -- `roleName` should be qualified.
    multiplyWithADT :: ADT EnumeratedRoleType -> String -> ADT EnumeratedRoleType
    multiplyWithADT adt roleName = case adt of
      EMPTY -> EMPTY
      p@(SUM _) -> PROD [p, ST $ EnumeratedRoleType roleName]
      PROD terms -> PROD $ cons (ST $ EnumeratedRoleType roleName) terms
      s@(ST _) -> PROD [s, ST $ EnumeratedRoleType roleName]
      UNIVERSAL -> UNIVERSAL

    -- Insert a Property type into a Role type.
    insertPropertyInto :: Property.Property -> EnumeratedRole -> EnumeratedRole
    insertPropertyInto (Property.E (EnumeratedProperty {_id})) (EnumeratedRole rr@{properties}) = EnumeratedRole $ rr {properties = cons (ENP _id) properties}
    insertPropertyInto (Property.C (CalculatedProperty{_id})) (EnumeratedRole rr@{properties}) = EnumeratedRole $ rr {properties = cons (CP _id) properties}

traverseStateE :: StateE -> StateIdentifier -> PhaseTwo State
traverseStateE (StateE {id, condition, stateParts}) ns = do
  state <- pure $ constructState ns condition
  -- Postpone all stateParts because there may be forward references to user and subject.
  void $ lift $ modify \s@{postponedStateQualifiedParts} -> s {postponedStateQualifiedParts = postponedStateQualifiedParts <> stateParts}
  pure state

addStateToDomeinFile :: State -> DomeinFileRecord -> DomeinFileRecord
addStateToDomeinFile state@(State{id}) dfr@{states} = dfr {states = EncodableMap (MAP.insert id state (unwrap states))}

getState :: StateIdentifier -> PhaseTwo (Maybe State)
getState id = gets _.dfr >>= \{states: (EncodableMap states)} -> pure $ MAP.lookup id states

-- Traverse the members of ViewE to construct a new View type and insert it into the
-- DomeinFileRecord.
traverseViewE :: ViewE -> Namespace -> PhaseTwo ViewType
traverseViewE (ViewE {id, viewParts, pos}) ns = do
  viewName <- pure (ns <> "$" <> id)
  (expandedPropertyReferences :: Array PropertyType) <- traverse qualifyProperty (ARR.fromFoldable viewParts)
  view <- pure $ VIEW.View
    { _id: ViewType viewName
    , _rev: Nothing
    , displayName: id
    , propertyReferences: expandedPropertyReferences
    , role: EnumeratedRoleType ns
    , pos: pos}
  modifyDF (\(df@{views}) -> df {views = insert viewName view views})
  pure (ViewType viewName)

  where
    -- TODO. Is het een calculated of een enumerated property?
    -- | Expand prefixed names.
    qualifyProperty :: String -> PhaseTwo PropertyType
    qualifyProperty pname = do
      expandedPname <- expandNamespace pname
      pure $ ENP $ EnumeratedPropertyType expandedPname

addRoleToDomeinFile :: Role -> DomeinFileRecord -> DomeinFileRecord
addRoleToDomeinFile (E r@(EnumeratedRole{_id})) domeinFile = LN.over
  (prop (SProxy :: SProxy "enumeratedRoles"))
  (insert (unwrap _id) r)
  domeinFile
addRoleToDomeinFile (C r@(CalculatedRole{_id})) domeinFile = LN.over
  (prop (SProxy :: SProxy "calculatedRoles"))
  (insert (unwrap _id) r)
  domeinFile
-- addRoleToDomeinFile role df@{enumeratedRoles, calculatedRoles} = case role of
--   (E r@(EnumeratedRole{_id})) -> df {enumeratedRoles = insert (unwrap _id) r enumeratedRoles}
--   (C r@(CalculatedRole{_id})) -> df {calculatedRoles = insert (unwrap _id) r calculatedRoles}

-- | Traverse a RoleE that results in an CalculatedRole.
traverseCalculatedRoleE :: RoleE -> Namespace -> PhaseTwo Role
traverseCalculatedRoleE (RoleE {id, kindOfRole, roleParts, pos}) ns = do
  role <- pure (defaultCalculatedRole (ns <> "$" <> id) id kindOfRole ns pos)
  role' <- traverseCalculatedRoleE_ role roleParts
  modifyDF (\domeinFile -> addRoleToDomeinFile role' domeinFile)
  pure role'

traverseCalculatedRoleE_ :: CalculatedRole -> List RolePart -> PhaseTwo Role
traverseCalculatedRoleE_ role@(CalculatedRole{_id:roleName, kindOfRole}) roleParts = do
  role' <- foldM (unsafePartial $ handleParts) role roleParts
  modifyDF (\domeinFile -> addRoleToDomeinFile (C role') domeinFile)
  pure (C role')

  where
    handleParts :: Partial => CalculatedRole -> RolePart -> PhaseTwo CalculatedRole
    -- Parse the query expression.

    -- CALCULATION
    handleParts (CalculatedRole roleUnderConstruction) (Calculation calc) = do
      expandedCalc <- expandPrefix calc
      pure $ CalculatedRole (roleUnderConstruction {calculation = S expandedCalc})

    -- PERSPECTIVE
    handleParts crole (SQP stateQualifiedParts) = do
      void $ lift $ modify \s@{postponedStateQualifiedParts} -> s {postponedStateQualifiedParts = postponedStateQualifiedParts <> stateQualifiedParts}
      pure crole

    -- -- FORUSER
    -- handleParts (CalculatedRole roleUnderConstruction) (ForUser _) = pure (CalculatedRole $ roleUnderConstruction)


-- | Traverse the members of the PropertyE AST type to construct a new Property type
-- | and insert it into a DomeinFileRecord.
traversePropertyE :: PropertyE -> Namespace -> PhaseTwo Property.Property
traversePropertyE r ns = if isCalculatedProperty r
  then traverseCalculatedPropertyE r ns
  else traverseEnumeratedPropertyE r ns
  where
    isCalculatedProperty :: PropertyE -> Boolean
    isCalculatedProperty (PropertyE {propertyParts}) = (isJust (findIndex (case _ of
        (Calculation' _) -> true
        otherwise -> false) propertyParts))

traverseEnumeratedPropertyE :: PropertyE -> Namespace -> PhaseTwo Property.Property
traverseEnumeratedPropertyE (PropertyE {id, range, propertyParts, pos}) ns = do
  property <- pure $ defaultEnumeratedProperty (ns <> "$" <> id) id ns (unsafePartial $ fromJust range) pos
  property' <- foldM (unsafePartial handleParts) property propertyParts
  modifyDF (\df -> addPropertyToDomeinFile (Property.E property') df)
  pure (Property.E property')

  where
    -- Construct members for the new Property type according to the type of
    -- parts found in the PropertyE AST type. Insert these members into the new Property.
    -- The Calculation case is not handled for an EnumeratedRole, hence the Partial constraint.
    handleParts :: Partial => EnumeratedProperty -> PropertyPart -> PhaseTwo EnumeratedProperty
    -- FUNCTIONALATTRIBUTE
    handleParts (EnumeratedProperty propertyUnderConstruction) (FunctionalAttribute' bool) = pure (EnumeratedProperty $ propertyUnderConstruction {functional = bool})

    -- MANDATORYATTRIBUTE
    handleParts (EnumeratedProperty propertyUnderConstruction) (MandatoryAttribute' bool) = pure (EnumeratedProperty $ propertyUnderConstruction {mandatory = bool})

-- | Traverse a PropertyE that results in an CalculatedProperty.
traverseCalculatedPropertyE :: PropertyE -> Namespace -> PhaseTwo Property.Property
traverseCalculatedPropertyE (PropertyE {id, range, propertyParts, pos}) ns = do
  (CalculatedProperty property@{calculation}) <- pure $ defaultCalculatedProperty (ns <> "$" <> id) id ns pos
  calculation' <- case head propertyParts of
    -- TODO: fish out the actually parsed calculation and use that!
    (Just (Calculation' c)) -> expandPrefix c >>= pure <<< S
    otherwise -> pure calculation
  property' <- pure $ Property.C $ CalculatedProperty (property {calculation = calculation'})
  modifyDF (\df -> addPropertyToDomeinFile property' df)
  pure property'

addPropertyToDomeinFile :: Property.Property -> DomeinFileRecord -> DomeinFileRecord
addPropertyToDomeinFile property df@{enumeratedProperties, calculatedProperties} = case property of
  (Property.E r@(EnumeratedProperty{_id})) -> df {enumeratedProperties = insert (unwrap _id) r enumeratedProperties}
  (Property.C r@(CalculatedProperty{_id})) -> df {calculatedProperties = insert (unwrap _id) r calculatedProperties}

handlePostponedStateQualifiedParts  :: PhaseTwo Unit
handlePostponedStateQualifiedParts = do
  postponedStateQualifiedParts <- lift $ gets _.postponedStateQualifiedParts
  for_ postponedStateQualifiedParts handlePart
  where

    handlePart :: StateQualifiedPart -> PhaseTwo Unit
    handlePart (N (NotificationE{user, transition, level, start, end})) = do
      qualifiedUser <- findRole user start
      modifyPartOfState transition start end
        \(sr@{notifyOnEntry, notifyOnExit}) -> case transition of
          Entry _ -> sr {notifyOnEntry = EncodableMap $ MAP.insert qualifiedUser level (unwrap notifyOnEntry)}
          Exit _ -> sr {notifyOnExit = EncodableMap $ MAP.insert qualifiedUser level (unwrap notifyOnExit)}

    handlePart (AE (AutomaticEffectE{subject, object, transition, effect, start, end})) = do
      qualifiedSubject <- findRole subject start
      -- TODO. If there is an object, the compilation of the effect can have an 'object' variable.
      modifyPartOfState transition start end
        \(sr@{automaticOnEntry, automaticOnExit}) -> let
          sideEffect = case effect of
            Left assignments -> A (ARR.fromFoldable assignments)
            Right letstep -> L letstep
          in case transition of
            Entry _ -> sr {automaticOnEntry = EncodableMap $ MAP.insert qualifiedSubject sideEffect (unwrap automaticOnEntry)}
            Exit _ -> sr {automaticOnExit = EncodableMap $ MAP.insert qualifiedSubject sideEffect (unwrap automaticOnExit)}

    handlePart (R (RoleVerbE{subject, object, state, roleVerbs:rv, start})) =
      modifyPerspective subject object start
        \(Perspective pr@{roleVerbs}) -> Perspective pr {roleVerbs = EncodableMap $ MAP.insert state rv (unwrap roleVerbs)}

    handlePart (P (PropertyVerbE{subject, object, state, propertyVerbs, propsOrView, start})) = do
      -- Construct the map of property types and property verbs.
      porv <- f propsOrView
      (propertyVerbs' :: PropertyVerbs) <- pure $ PropertyVerbs porv (ARR.fromFoldable propertyVerbs)
      modifyPerspective subject object start
        \(Perspective pr@{propertyVerbs:pverbs}) -> Perspective $ pr {propertyVerbs =
          EncodableMap $ MAP.insert
            state
            case MAP.lookup state (unwrap pverbs) of
              Nothing -> [propertyVerbs']
              Just pv -> cons propertyVerbs' pv
            (unwrap pverbs)}
      where
        f :: PropsOrView -> PhaseTwo (ExplicitSet PropertyType)
        f AllProperties = pure Universal
        f (Properties ps) =
          -- The (partial) names for properties used here may be defined outside
          -- of the model (due to role filling). Hence we postpone looking up their
          -- real referents to phase three. Here we assume an Enumerated PropertyType.
          pure $ PSet (ENP <<< EnumeratedPropertyType <$> (ARR.fromFoldable ps))
        f (View view) = do
          (views :: Object VIEW.View) <- getsDF _.views
          case lookup view views of
            Nothing -> throwError $ UnknownView start view
            Just (VIEW.View {propertyReferences}) -> pure $ PSet propertyReferences

    handlePart (AC (ActionE{id, subject, object, state, effect, start})) = modifyPerspective subject object start
      \(Perspective pr@{actions}) -> let
        sideEffect = case effect of
          Left assignments -> A (ARR.fromFoldable assignments)
          Right letstep -> L letstep
        in case MAP.lookup state (unwrap actions) of
          Nothing -> Perspective $ pr { actions = EncodableMap $ MAP.insert
            state
            (singleton id sideEffect)
            (unwrap actions)}
          Just effects -> Perspective $ pr { actions = EncodableMap $ MAP.insert
            state
            (insert id sideEffect effects)
            (unwrap actions) }

    modifyPerspective :: String -> Expr.Step -> ArcPosition -> (Perspective -> Perspective) -> PhaseTwo Unit
    modifyPerspective subject object start modifier = do
      qualifiedSubject <- findRole subject start
      -- Find the perspective by subject and object in PhaseTwoState.
      mperspective <- findPerspective qualifiedSubject object
      (perspective :: Perspective) <- case mperspective of
        Nothing -> pure $ Perspective
          { object: S object
          , roleVerbs: EncodableMap MAP.empty
          , propertyVerbs: EncodableMap MAP.empty
          , actions: EncodableMap MAP.empty
          }
        Just p -> pure p
      -- Save in state
      void $ modify \s@{perspectives} -> s {perspectives = MAP.insert
        (Tuple qualifiedSubject object)
        -- Modify it
        (modifier perspective)
        perspectives}

    modifyPartOfState :: StateTransitionE -> ArcPosition -> ArcPosition -> (StateRecord -> StateRecord) -> PhaseTwo Unit
    modifyPartOfState transition start end modifyState = do
      stateId <- case transition of
        Entry s -> pure s
        Exit s -> pure s
      -- The state is a fully qualified StateIdentifier.
      mstate <- getState stateId
      case mstate of
        Nothing -> throwError $ StateDoesNotExist stateId start end
        Just (State sr) -> do
          -- modify the state
          state' <- pure $ State (modifyState sr)
          modifyDF (\domeinFile -> addStateToDomeinFile state' domeinFile)

    -- Look up the role in the DomainFile, because it must be defined locally.
    -- This is because we can only specify perspectives for roles that are locally defined.
    -- In the same vein, we only define notifications and automatic effects for locally
    -- defined user roles.
    -- It may be both qualified and unqualified, where insufficient qualification is an error.
    findRole :: String -> ArcPosition -> PhaseTwo RoleType
    findRole partialRoleName start = do
      {enumeratedRoles, calculatedRoles} <- gets _.dfr
      ecandidates <- pure $ ARR.filter (areLastSegmentsOf partialRoleName) (keys enumeratedRoles)
      case length ecandidates of
        0 -> do
          ccandidates <- pure $ ARR.filter (areLastSegmentsOf partialRoleName) (keys calculatedRoles)
          case length ccandidates of
            0 -> throwError $ UnknownRole start partialRoleName
            1 -> pure $ CR $ CalculatedRoleType (unsafePartial $ ARRP.head ccandidates)
            _ -> throwError $ NotUniquelyIdentifying start partialRoleName ccandidates
        1 -> pure $ ENR $ EnumeratedRoleType (unsafePartial $ ARRP.head ecandidates)
        _ -> throwError $ NotUniquelyIdentifying start partialRoleName ecandidates
