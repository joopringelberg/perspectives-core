module Perspectives.Parsing.Arc.PhaseTwo where

import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Control.Monad.State (State, evalState, gets, modify, runState)
import Data.Array (cons, elemIndex, fromFoldable)
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Lens (over, set, traversed)
import Data.Lens.Record (prop)
import Data.List (List, filter, findIndex, foldM, head)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (insert, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord, defaultDomeinFileRecord)
import Perspectives.Identifiers (Namespace, deconstructNamespace_, isQualifiedWithDomein)
import Perspectives.Parsing.Arc.AST (ActionE(..), ActionPart(..), ContextE(..), ContextPart(..), PerspectiveE(..), PerspectivePart(..), PropertyE(..), PropertyPart(..), RoleE(..), RolePart(..), ViewE(..))
import Perspectives.Parsing.Arc.IndentParser (ArcPosition)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Action (Action(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..), defaultCalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole(..), defaultCalculatedRole)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.Property (Property(..)) as Property
import Perspectives.Representation.Class.Role (Role(..))
import Perspectives.Representation.Context (Context(..), defaultContext)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..), defaultEnumeratedProperty)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..), defaultEnumeratedRole)
import Perspectives.Representation.TypeIdentifiers (ActionType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleKind(..), RoleType(..), ViewType(..))
import Perspectives.Representation.View (View(..))
import Prelude (Unit, bind, discard, map, pure, void, ($), (<>), (==))

-- TODO
-- (1) In a view, we need to indicate whether the property is calculated or enumerated.
-- However, we don't know when traversing the Arc AST.
-- (2) We need a way to indicate PRODUCT types for bindings.
-- (3) We might want a way to generate some names (like for Perspectives).
-- (4) We might want a way to indicate that the system should be able to find a qualified name.

type PhaseTwoState = {bot :: Boolean, dfr :: DomeinFileRecord}

-- | A Monad with state that indicates whether the Subject of an Action is a Bot, and allows exceptions.
type PhaseTwo a = ExceptT PerspectivesError (State PhaseTwoState) a

-- | Run a computation in `PhaseTwo`, returning Errors or a Tuple holding both the state and the result of the computation.
runPhaseTwo :: forall a. PhaseTwo a -> (Tuple (Either PerspectivesError a) PhaseTwoState)
runPhaseTwo computation = runPhaseTwo_ computation defaultDomeinFileRecord

runPhaseTwo_ :: forall a. PhaseTwo a -> DomeinFileRecord -> (Tuple (Either PerspectivesError a) PhaseTwoState)
runPhaseTwo_ computation dfr = runState (runExceptT computation) {bot: false, dfr: dfr}

-- | Run a computation in `PhaseTwo`, returning Errors or the result of the computation.
evalPhaseTwo :: forall a. PhaseTwo a -> (Either PerspectivesError a)
evalPhaseTwo computation = evalPhaseTwo_ computation defaultDomeinFileRecord

evalPhaseTwo_ :: forall a. PhaseTwo a -> DomeinFileRecord -> (Either PerspectivesError a)
evalPhaseTwo_ computation drf = evalState (runExceptT computation) {bot: false, dfr: drf}

subjectIsBot :: PhaseTwo Unit
subjectIsBot = lift $ void $ modify (\s -> s {bot = true})

subjectIsNotABot :: PhaseTwo Unit
subjectIsNotABot = lift $ void $ modify (\s -> s {bot = false})

isSubjectBot :: PhaseTwo Boolean
isSubjectBot = lift $ gets _.bot

modifyDF :: (DomeinFileRecord -> DomeinFileRecord) -> PhaseTwo Unit
modifyDF f = void $ modify \s@{dfr} -> s {dfr = f dfr}

getDF :: PhaseTwo DomeinFileRecord
getDF = lift $ gets _.dfr

traverseDomain :: ContextE -> Namespace -> PhaseTwo DomeinFile
traverseDomain c@(ContextE {id, kindOfContext, contextParts, pos}) ns = do
  _ <- traverseContextE c ns
  -- TODO: We might correct the type of references to Properties in Views here.
  domeinFileRecord <- getDF
  pure $ DomeinFile domeinFileRecord

-- | Traverse the members of the ContextE AST type to construct a new Context type
-- | and insert it into a DomeinFileRecord.
traverseContextE :: ContextE -> Namespace -> PhaseTwo Context
traverseContextE (ContextE {id, kindOfContext, contextParts, pos}) ns = do
  context <- pure $ defaultContext (addNamespace ns id) id kindOfContext (if ns == "model:" then Nothing else (Just ns)) pos
  context' <- foldM handleParts context contextParts
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

    addContextToDomeinFile :: Context -> DomeinFileRecord -> DomeinFileRecord
    addContextToDomeinFile c@(Context{_id: (ContextType ident)}) domeinFile = over
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
      ExternalRole, (Context cr) -> Context $ cr {externeRol = _id}
      -- We may have added the user before, on handling his BotRole.
      UserRole, (Context cr@{gebruikerRol}) -> Context $ cr {gebruikerRol = case elemIndex _id gebruikerRol of
        Nothing -> cons _id gebruikerRol
        (Just _) -> gebruikerRol}

    insertRoleInto (C (CalculatedRole {_id, kindOfRole})) c = case kindOfRole, c of
      RoleInContext, (Context cr@{rolInContext}) -> Context $ cr {rolInContext = cons (CR _id) rolInContext}
      ContextRole, (Context cr@{contextRol}) -> Context $ cr {contextRol = cons (CR _id) contextRol}
      -- A catchall case that just returns the context. Calculated roles for ExternalRole,
      -- UserRole and BotRole should be ignored.
      -- _, _ -> c

    addNamespace :: String -> String -> String
    addNamespace ns' ln = if ns == "model:" then (ns' <> ln) else (ns' <> "$" <> ln)

-- | Traverse the members of the RoleE AST type to construct a new Role type
-- | and insert it into a DomeinFileRecord.
traverseRoleE :: RoleE -> Namespace -> PhaseTwo Role
traverseRoleE r ns = if isCalculatedRole r
  then traverseCalculatedRoleE r ns
  else traverseEnumeratedRoleE r ns
  where
    isCalculatedRole :: RoleE -> Boolean
    -- isCalculatedRole _ = true
    isCalculatedRole (RoleE {roleParts}) = (isJust (findIndex (case _ of
      (Calculation _) -> true
      otherwise -> false) roleParts))

-- | Traverse a RoleE that results in an EnumeratedRole.
traverseEnumeratedRoleE :: RoleE -> Namespace -> PhaseTwo Role
traverseEnumeratedRoleE (RoleE {id, kindOfRole, roleParts, pos}) ns = do
  -- If we have a BotRole, we add its parts (perspectives with actions) to the UserRole
  -- that it serves.
  df@{enumeratedRoles} <- getDF
  role@(EnumeratedRole{_id:roleName}) <-
    case kindOfRole of
      BotRole -> do
        subjectIsBot
        servedUserLocalName <- userServedByBot pos id roleParts
        servedUserId <- pure (ns <> "$" <> servedUserLocalName)
        case lookup servedUserId enumeratedRoles of
          (Just user) -> pure user
          Nothing -> pure (defaultEnumeratedRole servedUserId servedUserLocalName UserRole ns pos)
      UserRole -> do
        userId <- pure (ns <> "$" <> id)
        case lookup userId enumeratedRoles of
          (Just user) -> pure user
          Nothing -> pure (defaultEnumeratedRole userId id kindOfRole ns pos)
      otherwise -> do
        roleName <- pure (ns <> "$" <> id)
        pure (defaultEnumeratedRole roleName id kindOfRole ns pos)
  role' <- foldM (unsafePartial $ handleParts (unwrap roleName)) role roleParts
  subjectIsNotABot
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

    -- PERSPECTIVE
    handleParts roleName (EnumeratedRole roleUnderConstruction@{perspectives}) (PRE pe) = do
      (Tuple roleIdentifier actions) <- traversePerspectiveE pe roleName
      pure (EnumeratedRole roleUnderConstruction {perspectives = (insert roleIdentifier actions perspectives)})

    -- VIEW
    handleParts roleName (EnumeratedRole roleUnderConstruction@{views}) (VE pe) = do
      viewType <- traverseViewE pe roleName
      pure (EnumeratedRole $ roleUnderConstruction {views = cons viewType views})

    -- FUNCTIONALATTRIBUTE
    handleParts roleName (EnumeratedRole roleUnderConstruction) (FunctionalAttribute bool) = pure (EnumeratedRole $ roleUnderConstruction {functional = bool})

    -- MANDATORYATTRIBUTE
    handleParts roleName (EnumeratedRole roleUnderConstruction) (MandatoryAttribute bool) = pure (EnumeratedRole $ roleUnderConstruction {mandatory = bool})

    -- FILLEDBYATTRIBUTE
    handleParts roleName (EnumeratedRole roleUnderConstruction@{binding}) (FilledByAttribute bnd) = pure (EnumeratedRole $ roleUnderConstruction {binding = augmentADT binding bnd})

    -- FORUSER
    handleParts roleName (EnumeratedRole roleUnderConstruction) (ForUser _) = pure (EnumeratedRole $ roleUnderConstruction)

    userServedByBot :: ArcPosition -> String -> List RolePart -> PhaseTwo String
    userServedByBot pos' localBotName parts = let
      f = foldl
        (\found next -> if isJust found then found else case next of
          (ForUser user) -> Just user
          otherwise -> Nothing)
        Nothing
        parts
      in case f of
        (Just user) -> pure user
        otherwise -> throwError (MissingForUser pos' localBotName)

    -- TODO (augmentADT) We assume a sum type and we assume a qualified name. Handle PROD types.
    -- TODO (augmentADT). We do not know the namespace of the roleName that we augment the ADT with.
    augmentADT :: ADT EnumeratedRoleType -> String -> ADT EnumeratedRoleType
    augmentADT adt roleName = case adt of
      NOTYPE -> ST $ EnumeratedRoleType roleName
      x -> SUM [ST $ EnumeratedRoleType roleName, x]

    -- Insert a Property type into a Role type.
    insertPropertyInto :: Property.Property -> EnumeratedRole -> EnumeratedRole
    insertPropertyInto (Property.E (EnumeratedProperty {_id})) (EnumeratedRole rr@{properties}) = EnumeratedRole $ rr {properties = cons (ENP _id) properties}
    insertPropertyInto (Property.C (CalculatedProperty{_id})) (EnumeratedRole rr@{properties}) = EnumeratedRole $ rr {properties = cons (CP _id) properties}

-- Traverse the members of ViewE to construct a new View type and insert it into the
-- DomeinFileRecord.
traverseViewE :: ViewE -> Namespace -> PhaseTwo ViewType
traverseViewE (ViewE {id, viewParts, pos}) ns = do
  viewName <- pure (ns <> "$" <> id)
  view <- pure $ View
    { _id: ViewType viewName
    , _rev: Nothing
    , displayName: id
    , propertyReferences: map (qualifyProperty ns) (fromFoldable viewParts)
    , role: EnumeratedRoleType ns
    , pos: pos}
  modifyDF (\(df@{views}) -> df {views = insert viewName view views})
  pure (ViewType viewName)

  where
    -- TODO. Is het een calculated of een enumerated property?
    -- | If the second argument is a fully qualified name, return it as a PropertyType;
    -- | otherwise, assume it is just a local name and prefix it with the namespace first.
    qualifyProperty :: Namespace -> String -> PropertyType
    qualifyProperty roleName pname = if isQualifiedWithDomein pname then (ENP $ EnumeratedPropertyType pname) else (ENP $ EnumeratedPropertyType (roleName <> "$" <> pname))

addRoleToDomeinFile :: Role -> DomeinFileRecord -> DomeinFileRecord
addRoleToDomeinFile (E r@(EnumeratedRole{_id})) domeinFile = over
  (prop (SProxy :: SProxy "enumeratedRoles"))
  (insert (unwrap _id) r)
  domeinFile
addRoleToDomeinFile (C r@(CalculatedRole{_id})) domeinFile = over
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
  role' <- foldM (unsafePartial $ handleParts) role roleParts
  modifyDF (\domeinFile -> addRoleToDomeinFile (C role') domeinFile)
  pure (C role')

  where
    handleParts :: Partial => CalculatedRole -> RolePart -> PhaseTwo CalculatedRole
    -- Parse the query expression.
    handleParts roleUnderConstruction (Calculation calc) =
      -- let
      -- TODO: actually call the parser.
      -- calculation = parseQuery calc
      -- in Tuple domeinFile' (roleUnderConstruction {calculation = calculation})
      pure roleUnderConstruction

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
  property <- pure $ defaultEnumeratedProperty (ns <> "$" <> id) id ns range pos
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
    -- TODO: actually call the parser, here.
    (Just (Calculation' c)) -> pure calculation
    otherwise -> pure calculation
  property' <- pure $ Property.C $ CalculatedProperty (property {calculation = calculation'})
  modifyDF (\df -> addPropertyToDomeinFile property' df)
  pure property'

addPropertyToDomeinFile :: Property.Property -> DomeinFileRecord -> DomeinFileRecord
addPropertyToDomeinFile property df@{enumeratedProperties, calculatedProperties} = case property of
  (Property.E r@(EnumeratedProperty{_id})) -> df {enumeratedProperties = insert (unwrap _id) r enumeratedProperties}
  (Property.C r@(CalculatedProperty{_id})) -> df {calculatedProperties = insert (unwrap _id) r calculatedProperties}

-- | Traverse a PerspectiveE. Add each Action seperately to the DomeinFile.
-- | Returns the fully qualified string that identifies the Object of the Action and the qualified identifiers of the Actions
-- | (we need not know what kind of Role that Object is, to be able to store the
-- | Perspective in the Role itself).
traversePerspectiveE :: PerspectiveE -> Namespace -> PhaseTwo (Tuple String (Array ActionType))
traversePerspectiveE (PerspectiveE {id, perspectiveParts, pos}) rolename = do

  -- First identify the Object of the Perspective. We need to hand it down to treatment
  -- of each separate Action.
  (object :: String) <- case head $ filter (case _ of
    (Object _) -> true
    otherwise -> false) perspectiveParts of
      (Just (Object o)) -> pure (deconstructNamespace_ rolename <> "$" <> o)
      otherwise -> throwError (MissingObject pos id)

  -- Similarly, find and use the DefaultObjectView, if any. Notice that if no
  -- DefaultObjectView is found, each Action could provide its own View (or we will use
  -- the Allproperties View).
  -- We do not (yet) allow a View with a CalculatedRole, so the View is taken from the EnumeratedRole's that
  -- underly the calculation of such a role. However, that still allows the View to be defined in another
  -- namespace than the Role itself.
  -- In other words, the namespace of the View is unknown.
  (defaultObjectView :: Maybe ViewType) <- pure $ map (unsafePartial \(DefaultView v) -> ViewType v)
    (head $ filter (case _ of
      (DefaultView _) -> true
      otherwise -> false) perspectiveParts)

  -- Now construct all Actions.
  actions <- foldM (unsafePartial $ traverseActionE object defaultObjectView rolename) [] (filter (case _ of
      (Act _) -> true
      otherwise -> false) perspectiveParts)

  pure (Tuple object actions)

-- | Constructs an Action, using the provided Object and maybe the View on that Object,
-- | from the ActionE. Returns the fully qualified name of the Action in the ActionType.
-- | Adds each Action to the DomeinFileRecord.
traverseActionE :: Partial =>                     -- The function is partial because we just handle ActionE.
  String ->                                       -- The qualified identifier of the Object.
  Maybe ViewType ->                               -- The unqualified identifier of the Default View on the Object.
  Namespace ->                                    -- The namespace, i.e. the qualified identifier of the Role.
  (Array ActionType) ->                           -- Accumulator: an array of Actions.
  PerspectivePart ->                              -- The ActionE element.
  PhaseTwo (Array ActionType)
traverseActionE object defaultObjectView rolename actions (Act (ActionE{id, verb, actionParts, pos})) = do
  actionId <- pure (rolename <> "$" <> id)
  executedByBot <- isSubjectBot
  action <- pure $ Action
    { _id: ActionType actionId
    , _rev: Nothing
    , displayName: id
    , subject: [EnumeratedRoleType rolename]
    , verb: verb
    , object: (ENR $ EnumeratedRoleType object) -- But it may be Calculated!
    , requiredObjectProperties: defaultObjectView
    , requiredSubjectProperties: Nothing
    , requiredIndirectObjectProperties: Nothing
    , indirectObject: Nothing
    , condition: Nothing
    , effect: Nothing
    , executedByBot: executedByBot
  }
  action' <- foldM (handleParts $ deconstructNamespace_ rolename) action actionParts
  modifyDF (\df -> df {actions = (insert actionId action' df.actions)})
  pure (cons (identifier action') actions)

  where

    handleParts :: Namespace -> Action -> ActionPart -> PhaseTwo Action

    -- INDIRECTOBJECT
    handleParts contextName (Action ar) (IndirectObject ido) = pure $ Action ar {indirectObject = Just (ENR $ EnumeratedRoleType (contextName <> "$" <> ido))}

    -- SUBJECTVIEW
    handleParts _ (Action ar) (SubjectView sv) = pure $ Action (ar {requiredSubjectProperties = Just $ ViewType sv})

    -- OBJECTVIEW
    handleParts _ (Action ar) (ObjectView ov) = pure $ Action (ar {requiredObjectProperties = Just $ ViewType ov})

    -- INDIRECTOBJECTVIEW
    handleParts _ (Action ar) (IndirectObjectView iov) = pure $ Action (ar {requiredIndirectObjectProperties = Just $ ViewType iov})
