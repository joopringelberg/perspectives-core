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

import Control.Monad.Except (lift)
import Control.Monad.State (modify)
import Data.Array (cons, elemIndex, foldl)
import Data.Array (fromFoldable) as ARR
import Data.Lens (over) as LN
import Data.Lens.Record (prop)
import Data.List (List(..), filter, findIndex, foldM, head)
import Data.List.NonEmpty (head) as LNE
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)
import Data.String.Regex (test)
import Type.Proxy (Proxy(..))
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, fromFoldable, insert, union)
import Partial.Unsafe (unsafePartial)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.Identifiers (Namespace, isTypeUri, newModelRegex, qualifyWith, typeUri2typeNameSpace_)
import Perspectives.Parsing.Arc.AST (ContextE(..), ContextPart(..), FilledByAttribute(..), FilledBySpecification(..), PropertyE(..), PropertyMapping(..), PropertyPart(..), RoleE(..), RoleIdentification(..), RolePart(..), ScreenE(..), StateE(..), StateSpecification(..), ViewE(..))
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.ExpandPrefix (expandPrefix)
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), RoleInContext(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..), defaultCalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole(..), defaultCalculatedRole)
import Perspectives.Representation.Class.Property (Property(..)) as Property
import Perspectives.Representation.Class.Role (Role(..))
import Perspectives.Representation.Context (Context(..), defaultContext)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..), defaultEnumeratedProperty)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..), defaultEnumeratedRole)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.State (State(..), StateFulObject(..), constructState)
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), DomeinFileId(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), StateIdentifier(..), ViewType(..), externalRoleType_, roletype2string)
import Perspectives.Representation.TypeIdentifiers (RoleKind(..)) as TI
import Perspectives.Representation.View (View(..)) as VIEW
import Prelude (bind, discard, pure, show, void, ($), (&&), (<$>), (<<<), (<>), (==), (>>=), (||))

-------------------
traverseDomain :: ContextE -> PhaseTwo DomeinFile
traverseDomain c = do
  -- Traverse the model parse tree and construct a DomeinFileRecord in PhaseTwoState.
  (Context {id:namespace}) <- traverseContextE c "domain"
  domeinFileRecord <- getDF
  pure $ DomeinFile (domeinFileRecord 
    { id = DomeinFileId $ unwrap namespace
    , namespace = unwrap namespace
    })

-- | Traverse the members of the ContextE AST type to construct a new Context type
-- | and insert it into a DomeinFileRecord.
traverseContextE :: ContextE -> Namespace -> PhaseTwo Context
traverseContextE (ContextE {id, kindOfContext, public, contextParts, pos}) ns = do
  -- TODO. Controleer op dubbele definities.
  contextIdentifier <- pure $ modelName id
  -- Notice that we use the test ns == "domain" to establish that this context is a domain, in which case the surrounding context is Nothing.
  context <- pure $ defaultContext contextIdentifier id kindOfContext (if ns == "domain" then Nothing else (Just ns)) pos public
  withNamespaces
    contextParts
    do
      contextParts' <- case (head (filter (case _ of
        RE (RoleE{id:rid}) -> rid == "External"
        otherwise -> false) contextParts)) of          
          Nothing -> do
            -- Add a default state for the external role.
            state <- pure $ constructState (StateIdentifier $ contextIdentifier <> "$External") (Q $ trueCondition (CDOM $ UET (ContextType contextIdentifier))) (Cnt (ContextType contextIdentifier)) []
            modifyDF (\domeinFile -> addStatesToDomeinFile [state] domeinFile)
            -- Add a definition for the external role; it apparently hasn't been declared in the source.
            pure $ Cons (RE (RoleE{id: "External", kindOfRole: TI.ExternalRole, roleParts: Nil, pos})) contextParts
          otherwise -> pure contextParts
      context' <- foldM handleParts context contextParts'
      modifyDF (\domeinFile -> addContextToDomeinFile context' domeinFile)
      pure context'

  where

    modelName :: String -> String
    modelName contextId = if ns == "domain" && test newModelRegex contextId 
      then contextId
      else addNamespace ns contextId

    trueCondition :: Domain -> QueryFunctionDescription
    trueCondition dom = SQD dom (Constant PBool "true") (VDOM PBool Nothing) True True

    -- Construct members for the new Context type according to the type of
    -- parts found in the ContextE AST type. Insert these members into the new Context.
    handleParts :: Context -> ContextPart -> PhaseTwo Context
    -- Construct a nested Context.
    handleParts contextUnderConstruction (CE c) = do
      subContext <- traverseContextE c (modelName id)
      pure (subContext `insertInto` contextUnderConstruction)

    -- Construct a Role
    handleParts contextUnderConstruction (RE r) = do
      role <- traverseRoleE r (modelName id)
      context' <- pure (role `insertRoleInto` contextUnderConstruction)
      case role of 
        E (EnumeratedRole {id:roleId, roleAspects}) -> do
          case context' of 
            Context cr@{roleAliases} -> pure $ Context $ cr { roleAliases = foldl (\als (RoleInContext{role:role'}) -> insert (unwrap role') roleId als) roleAliases roleAspects }
        _ -> pure context'

    -- Prefixes are handled earlier, so this can be a no-op
    handleParts contextUnderConstruction (PREFIX pre model) = pure contextUnderConstruction

    handleParts (Context contextUnderConstruction@{contextAspects}) (ContextAspect contextName pos') = do
      expandedAspect <- expandNamespace contextName
      -- TODO. This is strange: surely the unqualified name should be REPLACED by the qualified one!?
      -- Probably we currently just allow fully qualified aspect names.
      if isTypeUri expandedAspect
        then pure (Context $ contextUnderConstruction {contextAspects = cons (ContextType expandedAspect) contextAspects})
        else throwError $ NotWellFormedName pos' contextName

    handleParts (Context contextUnderConstruction) (IndexedContext indexedName pos') = do
      qualifiedIndexedName <- expandNamespace indexedName
      pure (Context $ contextUnderConstruction {indexedContext = Just $ ContextInstance qualifiedIndexedName})

    handleParts c@(Context contextUnderConstruction@({id:contextId})) (STATE s@(StateE{id:stateId, subStates})) = do
      state@(State{id:ident}) <- traverseStateE (Cnt contextId) s
      substates <- for subStates (traverseStateE (Cnt contextId))
      modifyDF (\domeinFile -> addStatesToDomeinFile (cons state (ARR.fromFoldable substates)) domeinFile)
      -- If this state is the context root state, we could register it as such with the context.
      -- However, if not, we should register it with its parent and that need not be available.
      -- Hence, we postpone registering the state to PhaseThree.
      pure c

    -- Add the aspect role in the appropriate category.
    -- We don't know yet if this aspect role is calculated or enumerated.
    -- Hence, we add it as enumerated and repair later.
    handleParts c@(Context cr@{rolInContext, contextRol,gebruikerRol}) (AspectRole ident kind _) = do 
      expandedName <- expandNamespace ident
      case kind of
        TI.RoleInContext -> pure $ Context $ cr {rolInContext = cons (ENR $ EnumeratedRoleType expandedName) rolInContext}
        TI.ContextRole -> pure $ Context $ cr {contextRol = cons (ENR $ EnumeratedRoleType expandedName) contextRol}
        TI.UserRole -> pure $ Context $ cr {gebruikerRol = cons (ENR $ EnumeratedRoleType expandedName) gebruikerRol}
        _ -> pure c

    -- We can safely ignore nested lists of StateQualifiedParts here, as they are already removed by the parser.
    handleParts c (CSQP _) = pure c

    addContextToDomeinFile :: Context -> DomeinFileRecord -> DomeinFileRecord
    addContextToDomeinFile c@(Context{id: (ContextType ident)}) domeinFile = LN.over
      (prop (Proxy :: Proxy "contexts"))
      (insert ident c)
      domeinFile

    -- Insert a sub-Context type into a Context type.
    insertInto :: Context -> Context -> Context
    insertInto (Context{id:contextId}) (Context cr@{nestedContexts}) = Context $ cr {nestedContexts = cons contextId nestedContexts}

    -- Insert a Role type into a Context type.
    insertRoleInto :: Role -> Context -> Context
    insertRoleInto (E (EnumeratedRole {id:roleId, kindOfRole})) c = case kindOfRole, c of
      TI.RoleInContext, (Context cr@{rolInContext}) -> Context $ cr {rolInContext = cons (ENR roleId) rolInContext}
      TI.ContextRole, (Context cr@{contextRol}) -> Context $ cr {contextRol = cons (ENR roleId) contextRol}
      TI.ExternalRole, ctxt -> ctxt
      -- We may have added the user before, on handling his BotRole.
      TI.UserRole, (Context cr@{gebruikerRol}) -> Context $ cr {gebruikerRol = case elemIndex (ENR roleId) gebruikerRol of
        Nothing -> cons (ENR roleId) gebruikerRol
        (Just _) -> gebruikerRol}
      -- This is the Enumerated variant of the public role:
      TI.PublicProxy, (Context cr@{gebruikerRol}) -> Context $ cr {gebruikerRol = case elemIndex (ENR roleId) gebruikerRol of
        Nothing -> cons (ENR roleId) gebruikerRol
        (Just _) -> gebruikerRol}
      TI.Public, ctxt -> ctxt

    insertRoleInto (C (CalculatedRole {id:roleId, kindOfRole})) c = case kindOfRole, c of
      TI.RoleInContext, (Context cr@{rolInContext}) -> Context $ cr {rolInContext = cons (CR roleId) rolInContext}
      TI.ContextRole, (Context cr@{contextRol}) -> Context $ cr {contextRol = cons (CR roleId) contextRol}
      TI.UserRole, (Context cr@{gebruikerRol}) -> Context $ cr {gebruikerRol = cons (CR roleId) gebruikerRol}
      TI.Public, (Context cr@{gebruikerRol}) -> Context $ cr {gebruikerRol = cons (CR roleId) gebruikerRol}
      -- A catchall case that just returns the context. Calculated roles for ExternalRole and UserRole should be ignored.
      _, _ -> c

addNamespace :: String -> String -> String
addNamespace ns' ln = if ns' == "domain" then ln else (ns' <> "$" <> ln)

-- | Traverse the members of the RoleE AST type to construct a new Role type
-- | and insert it into a DomeinFileRecord.
traverseRoleE :: RoleE -> Namespace -> PhaseTwo Role
traverseRoleE r ns = if isCalculatedRole r
  then traverseCalculatedRoleE r ns
  else traverseEnumeratedRoleE r ns
  where
    isCalculatedRole :: RoleE -> Boolean
    isCalculatedRole (RoleE {roleParts}) = (isJust (findIndex (case _ of
      (Calculation _ _) -> true
      otherwise -> false) roleParts))

traverseEnumeratedRoleE :: RoleE -> Namespace -> PhaseTwo Role
traverseEnumeratedRoleE (RoleE {id, kindOfRole, roleParts, pos}) ns = do
  -- TODO. Controleer op dubbele definities.
  role <- pure (defaultEnumeratedRole (ns <> "$" <> id) id kindOfRole ns pos)
  traverseEnumeratedRoleE_ role roleParts

traverseEnumeratedRoleE_ :: EnumeratedRole -> List RolePart -> PhaseTwo Role
traverseEnumeratedRoleE_ role@(EnumeratedRole{id:rn, kindOfRole}) roleParts = do
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
      parts <- traverse expandPrefix stateQualifiedParts
      lift $ void $ modify \s@{postponedStateQualifiedParts} -> s {postponedStateQualifiedParts = postponedStateQualifiedParts <> parts}
      pure erole

    -- VIEW
    handleParts roleName (EnumeratedRole roleUnderConstruction@{views}) (VE pe) = do
      viewType <- traverseViewE pe (ENR rn)
      pure (EnumeratedRole $ roleUnderConstruction {views = cons viewType views})

    -- FUNCTIONALATTRIBUTE
    handleParts roleName (EnumeratedRole roleUnderConstruction) (FunctionalAttribute bool) = pure (EnumeratedRole $ roleUnderConstruction {functional = bool})

    -- MANDATORYATTRIBUTE
    handleParts roleName (EnumeratedRole roleUnderConstruction) (MandatoryAttribute bool) = pure (EnumeratedRole $ roleUnderConstruction {mandatory = bool})

    --UNLINKEDATTRIBUTE
    handleParts roleName (EnumeratedRole roleUnderConstruction) UnlinkedAttribute = pure (EnumeratedRole $ roleUnderConstruction {unlinked = true})

    -- FILLEDBYATTRIBUTE
    handleParts roleName (EnumeratedRole roleUnderConstruction@{id, binding}) (FilledBySpecifications spec) = case spec of
      Alternatives attrs -> case LNE.head attrs of
        FilledByAttribute "None" context -> pure (EnumeratedRole $ roleUnderConstruction {binding = Just $ ST $ RoleInContext {role: id, context}})
        _ -> do 
          restrictions <- for attrs \(FilledByAttribute bnd context) -> do 
            ebnd <- expandBinding bnd
            pure $ ST $ RoleInContext {role: EnumeratedRoleType ebnd, context}
          pure $ EnumeratedRole $ roleUnderConstruction {binding = Just $ SUM (ARR.fromFoldable restrictions)}

      Combination attrs -> do 
        restrictions <- for attrs \(FilledByAttribute bnd context) -> do 
          ebnd <- expandBinding bnd
          pure $ ST $ RoleInContext {role: EnumeratedRoleType ebnd, context}
        pure $ EnumeratedRole $ roleUnderConstruction {binding = Just $ PROD (ARR.fromFoldable restrictions)}
      -- We assume the result of expandBinding refers to an EnumeratedRoleType. This need not be so;
      -- it will be repaired in PhaseThree.
      -- Note that `context` may not yet be fully qualified.
      -- It may also be unspecified, coded as an empty string.
      -- PhaseThree will resolve both issues.
      where 
        expandBinding :: String -> PhaseTwo String
        expandBinding bnd = if kindOfRole == TI.ContextRole
          -- If the RoleKind is ContextRole, we should construct the name of the External
          -- Role of the binding (which then is a Context)
          -- This is because the ArcIdentifier following 'filledBy' for a ContextRole, identifies the context - not its
          -- external role.
          then expandNamespace (externalRoleType_ bnd)
          else expandNamespace bnd 

    -- ROLEASPECT
    handleParts roleName (EnumeratedRole roleUnderConstruction@{context, roleAspects, propertyAliases}) (RoleAspect a pos' mPropertyMapping) = do
      expandedAspect <- expandNamespace a
      if isTypeUri expandedAspect
        then do 
          (mPropertyMapping' :: Maybe (Object EnumeratedPropertyType)) <- case mPropertyMapping of 
            Nothing -> pure Nothing
            Just (PropertyMapping propertyMapping) -> Just <<< fromFoldable <$> for propertyMapping (\(Tuple origin destination) -> do 
              destination' <- expandNamespace destination
              pure $ Tuple (qualifyWith expandedAspect origin) (EnumeratedPropertyType destination'))
          pure (EnumeratedRole $ roleUnderConstruction 
            { roleAspects = cons
              (RoleInContext{context: ContextType (typeUri2typeNameSpace_ expandedAspect), role: (EnumeratedRoleType expandedAspect)})
              roleAspects
              -- There may be a mapping already, as each Aspect can contribute to the mapping.
              -- The properties-to-be-mapped (the origins) are qualified by the aspect and so are guaranteed to be unique.
              -- Therefore we can just construct the union.
              -- NOTICE that the destinations may be underqualified. We will fix that in PhaseThree.
            , propertyAliases = maybe propertyAliases (union propertyAliases) mPropertyMapping'
            })
        else throwError $ NotWellFormedName pos' a 

    -- INDEXEDROLE
    handleParts roleName (EnumeratedRole roleUnderConstruction) (IndexedRole indexedName pos') = do
      expandedIndexedName <- expandNamespace indexedName
      pure (EnumeratedRole $ roleUnderConstruction {indexedRole = Just (RoleInstance expandedIndexedName)})

    -- ROLESTATE
    handleParts roleName e@(EnumeratedRole roleUnderConstruction@{id, context, kindOfRole:kind}) (ROLESTATE s@(StateE{id:stateId, subStates})) = do
      stateKind <- pure (case kind of
        TI.UserRole -> Srole id
        TI.PublicProxy -> Srole id
        _ -> Orole id)
      state@(State{id:ident}) <- traverseStateE stateKind s
      substates <- for subStates (traverseStateE stateKind)
      modifyDF (\domeinFile -> addStatesToDomeinFile (cons state (ARR.fromFoldable substates)) domeinFile)
      -- If this state is the role root state, we could register it as such with the role.
      -- However, if not, we should register it with its parent and that need not be available.
      -- Hence, we postpone registering the state to PhaseThree.
      pure e

    -- SCREENE
    handleParts roleName e@(EnumeratedRole roleUnderConstruction@{kindOfRole:kind}) (Screen s@(ScreenE{start, end})) = if kind == TI.UserRole || kind == TI.PublicProxy
      then do
        screen' <- expandPrefix s
        void $ lift $ modify \st@{screens} -> st {screens = Cons screen' screens}
        pure e
      else throwError (ScreenForUserRoleOnly start end)

    handleParts roleName e@(EnumeratedRole roleUnderConstruction@{}) (PublicUrl calc) = do 
      expandedCalc <- expandPrefix calc
      pure $ EnumeratedRole (roleUnderConstruction {publicUrl = Just $ S expandedCalc true})

    -- Insert a Property type into a Role type.
    insertPropertyInto :: Property.Property -> EnumeratedRole -> EnumeratedRole
    insertPropertyInto (Property.E (EnumeratedProperty {id})) (EnumeratedRole rr@{properties}) = EnumeratedRole $ rr {properties = cons (ENP id) properties}
    insertPropertyInto (Property.C (CalculatedProperty{id})) (EnumeratedRole rr@{properties}) = EnumeratedRole $ rr {properties = cons (CP id) properties}

traverseStateE :: StateFulObject -> StateE -> PhaseTwo State
traverseStateE stateFulObect (StateE {id, condition, stateParts, subStates}) = do
  subStateIds <- for subStates (toStateIdentifier <<< \(StateE{id:id'}) -> id')
  stateId <- toStateIdentifier id
  expandedCondition <- expandPrefix condition
  state <- pure $ constructState stateId (S expandedCondition false) stateFulObect (ARR.fromFoldable subStateIds)
  -- Postpone all stateParts because there may be forward references to user and subject.
  parts <- traverse expandPrefix stateParts
  void $ lift $ modify \s@{postponedStateQualifiedParts} -> s {postponedStateQualifiedParts = postponedStateQualifiedParts <> parts}
  -- Traverse substates and add to DomeinFile.
  substates <- for subStates (traverseStateE stateFulObect)
  modifyDF (\domeinFile -> addStatesToDomeinFile (ARR.fromFoldable substates) domeinFile)

  pure state
  where
    -- NOTE that the StateIdentifiers constructed out of SubjectState need not be fully qualified.
    toStateIdentifier :: StateSpecification -> PhaseTwo StateIdentifier
    toStateIdentifier (ContextState (ContextType ctxt) spath)  = case spath of
      Nothing -> pure $ StateIdentifier ctxt
      Just segments -> pure $ StateIdentifier (ctxt <> "$" <> segments)
    toStateIdentifier (SubjectState (ExplicitRole _ rt _) spath) = case spath of
      Nothing -> pure $ StateIdentifier $ roletype2string rt
      Just segments -> pure $ StateIdentifier (roletype2string rt <> "$" <> segments)
    toStateIdentifier (ObjectState (ExplicitRole _ rt _) spath) = case spath of
      Nothing -> pure $ StateIdentifier $ roletype2string rt
      Just segments -> pure $ StateIdentifier (roletype2string rt <> "$" <> segments)
    toStateIdentifier _ = throwError (Custom "Programming error: State should be specified with a ContextState, or an ExplicitRole.")

addStatesToDomeinFile :: Array State -> DomeinFileRecord -> DomeinFileRecord
-- addStatesToDomeinFile state@(State{id}) dfr@{states} = dfr {states = insert (unwrap id) state states}
addStatesToDomeinFile extraStates dfr@{states} = dfr { states =
  -- TODO. Controleer op dubbele definities.
  states `union` (fromFoldable $ (\s@(State{id}) -> Tuple (unwrap id) s) <$> extraStates) }

-- Traverse the members of ViewE to construct a new View type and insert it into the
-- DomeinFileRecord.
traverseViewE :: ViewE -> RoleType -> PhaseTwo ViewType
traverseViewE (ViewE {id, viewParts, pos}) rtype = do
  -- TODO. Controleer op dubbele definities.
  viewName <- pure (roletype2string rtype <> "$" <> id)
  (expandedPropertyReferences :: Array PropertyType) <- traverse qualifyProperty (ARR.fromFoldable viewParts)
  view <- pure $ VIEW.View
    { id: ViewType viewName
    , _rev: Nothing
    , displayName: id
    , propertyReferences: expandedPropertyReferences
    , role: rtype
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
addRoleToDomeinFile (E r@(EnumeratedRole{id})) domeinFile = LN.over
  (prop (Proxy :: Proxy "enumeratedRoles"))
  (insert (unwrap id) r)
  domeinFile
addRoleToDomeinFile (C r@(CalculatedRole{id})) domeinFile = LN.over
  (prop (Proxy :: Proxy "calculatedRoles"))
  (insert (unwrap id) r)
  domeinFile

-- | Traverse a RoleE that results in an CalculatedRole.
traverseCalculatedRoleE :: RoleE -> Namespace -> PhaseTwo Role
traverseCalculatedRoleE (RoleE {id, kindOfRole, roleParts, pos}) ns = do
  -- TODO. Controleer op dubbele definities.
  role <- pure (defaultCalculatedRole (ns <> "$" <> id) id kindOfRole ns pos)
  role' <- traverseCalculatedRoleE_ role roleParts
  modifyDF (\domeinFile -> addRoleToDomeinFile role' domeinFile)
  pure role'

traverseCalculatedRoleE_ :: CalculatedRole -> List RolePart -> PhaseTwo Role
traverseCalculatedRoleE_ role@(CalculatedRole{id:roleName, kindOfRole}) roleParts = do
  role' <- foldM (handleParts) role roleParts
  modifyDF (\domeinFile -> addRoleToDomeinFile (C role') domeinFile)
  pure (C role')

  where
    -- Notice that we ignore an eventual PublicUrl RolePart; it is not useful for the Calculated version of the Public role. 
    -- The same holds for the State part.
    handleParts :: CalculatedRole -> RolePart -> PhaseTwo CalculatedRole
    -- Parse the query expression.

    -- CALCULATION
    handleParts (CalculatedRole roleUnderConstruction) (Calculation calc isFunctional) = do
      expandedCalc <- expandPrefix calc
      pure $ CalculatedRole (roleUnderConstruction {calculation = S expandedCalc isFunctional})

    -- PERSPECTIVE
    handleParts crole (SQP stateQualifiedParts) = do
      parts <- traverse expandPrefix stateQualifiedParts
      void $ lift $ modify \s@{postponedStateQualifiedParts} -> s {postponedStateQualifiedParts = postponedStateQualifiedParts <> parts}
      pure crole

    -- VIEW
    handleParts (CalculatedRole roleUnderConstruction@{views}) (VE pe) = do
      viewType <- traverseViewE pe (CR roleName)
      pure (CalculatedRole $ roleUnderConstruction {views = cons viewType views})

    -- SCREENE
    handleParts e@(CalculatedRole roleUnderConstruction@{kindOfRole:kind}) (Screen s@(ScreenE{start, end})) = if kind == TI.UserRole || kind == TI.Public
      then do
        screen' <- expandPrefix s
        void $ lift $ modify \st@{screens} -> st {screens = Cons screen' screens}
        pure e
      else throwError (ScreenForUserRoleOnly start end)

    handleParts crole (PublicUrl _) = pure crole

    handleParts crole (ROLESTATE _) = pure crole


    handleParts crole p = throwError $ Custom ("Cannot handle part '" <> show p <> "' in PhaseTwo in a CalculatedRole: " <> show roleName)

-- | Traverse the members of the PropertyE AST type to construct a new Property type
-- | and insert it into a DomeinFileRecord.
traversePropertyE :: PropertyE -> Namespace -> PhaseTwo Property.Property
traversePropertyE r ns = if isCalculatedProperty r
  then traverseCalculatedPropertyE r ns
  else traverseEnumeratedPropertyE r ns
  where
    isCalculatedProperty :: PropertyE -> Boolean
    isCalculatedProperty (PropertyE {propertyParts}) = (isJust (findIndex (case _ of
        (Calculation' _ _) -> true
        otherwise -> false) propertyParts))

traverseEnumeratedPropertyE :: PropertyE -> Namespace -> PhaseTwo Property.Property
traverseEnumeratedPropertyE (PropertyE {id, range, propertyParts, propertyFacets, pos}) ns = do
  -- TODO. Controleer op dubbele definities.
  property <- pure $ defaultEnumeratedProperty (ns <> "$" <> id) id ns
    (case range of
      Nothing -> PString
      Just r -> r)
    pos
  EnumeratedProperty pr <- foldM (unsafePartial handleParts) property propertyParts
  property' <- pure $ EnumeratedProperty pr {constrainingFacets = ARR.fromFoldable propertyFacets}
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

    -- SELFONLYATTRIBUTE
    handleParts (EnumeratedProperty propertyUnderConstruction) SelfonlyAttribute = pure (EnumeratedProperty $ propertyUnderConstruction {selfonly = true})

    -- AUTHORONLYATTRIBUTE
    handleParts (EnumeratedProperty propertyUnderConstruction) AuthoronlyAttribute = pure (EnumeratedProperty $ propertyUnderConstruction {selfonly = true})

-- | Traverse a PropertyE that results in an CalculatedProperty.
traverseCalculatedPropertyE :: PropertyE -> Namespace -> PhaseTwo Property.Property
traverseCalculatedPropertyE (PropertyE {id, range, propertyParts, pos}) ns = do
  -- TODO. Controleer op dubbele definities.
  (CalculatedProperty property@{calculation}) <- pure $ defaultCalculatedProperty (ns <> "$" <> id) id ns pos
  calculation' <- case head propertyParts of
    -- TODO: fish out the actually parsed calculation and use that!
    (Just (Calculation' c b)) -> expandPrefix c >>= pure <<< \calc -> S calc b
    otherwise -> pure calculation
  property' <- pure $ Property.C $ CalculatedProperty (property {calculation = calculation'})
  modifyDF (\df -> addPropertyToDomeinFile property' df)
  pure property'

addPropertyToDomeinFile :: Property.Property -> DomeinFileRecord -> DomeinFileRecord
addPropertyToDomeinFile property df@{enumeratedProperties, calculatedProperties} = case property of
  (Property.E r@(EnumeratedProperty{id})) -> df {enumeratedProperties = insert (unwrap id) r enumeratedProperties}
  (Property.C r@(CalculatedProperty{id})) -> df {calculatedProperties = insert (unwrap id) r calculatedProperties}
