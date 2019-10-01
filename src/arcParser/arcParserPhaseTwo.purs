module Perspectives.Parsing.Arc.PhaseTwo where

import Control.Monad.Except (Except, throwError)
import Data.Array (cons, elemIndex, fromFoldable)
import Data.Foldable (foldl)
import Data.Lens (over)
import Data.Lens.Record (prop)
import Data.List (List, findIndex, foldM, head)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (insert, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.Identifiers (isQualifiedWithDomein)
import Perspectives.Parsing.Arc.AST (ContextE(..), ContextPart(..), PropertyE(..), PropertyPart(..), RoleE(..), RolePart(..), ViewE(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..), defaultCalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole(..), defaultCalculatedRole)
import Perspectives.Representation.Class.Property (Property(..)) as Property
import Perspectives.Representation.Class.Role (Role(..))
import Perspectives.Representation.Context (Context(..), defaultContext)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..), defaultEnumeratedProperty)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..), defaultEnumeratedRole)
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleKind(..), RoleType(..), ViewType(..))
import Perspectives.Representation.View (View(..))
import Prelude (map, pure, ($), (<>), (==), bind)

-- TODO
-- (1) In a view, we need to indicate whether the property is calculated or enumerated.
-- However, we don't know when traversing the Arc AST.
-- (2) We need a way to indicate PRODUCT types for bindings.
-- (3) We might want a way to generate some names (like for Perspectives).
-- (4) We might want a way to indicate that the system should be able to find a qualified name.

traverseDomain :: ContextE -> DomeinFileRecord -> String -> Except String DomeinFile
traverseDomain c@(ContextE {id, kindOfContext, contextParts}) df ns = do
  (Tuple domeinFileRecord domain) <- traverseContextE c df ns
  pure $ DomeinFile domeinFileRecord

-- | Traverse the members of the ContextE AST type to construct a new Context type
-- | and insert it into a DomeinFileRecord.
traverseContextE :: ContextE -> DomeinFileRecord -> String -> Except String (Tuple DomeinFileRecord Context)
traverseContextE (ContextE {id, kindOfContext, contextParts}) df ns = do
  context <- pure $ defaultContext (addNamespace ns id) id kindOfContext (if ns == "model:" then Nothing else (Just ns))
  (Tuple domeinFile context') <- foldM handleParts (Tuple df context) contextParts
  pure $ Tuple (addContextToDomeinFile context' domeinFile) context'

  where

    -- Construct members for the new Context type according to the type of
    -- parts found in the ContextE AST type. Insert these members into the new Context.
    handleParts :: Tuple DomeinFileRecord Context -> ContextPart -> Except String (Tuple DomeinFileRecord Context)
    -- Construct a nested Context.
    handleParts (Tuple domeinFile contextUnderConstruction) (CE c) = do
      (Tuple domeinFile' subContext) <- traverseContextE c domeinFile (addNamespace ns id)
      pure $ Tuple domeinFile' (subContext `insertInto` contextUnderConstruction)

    -- Construct a Role
    handleParts (Tuple domeinFile contextUnderConstruction) (RE r) = do
      (Tuple domeinFile' role) <- traverseRoleE r domeinFile (addNamespace ns id)
      pure $ Tuple domeinFile' (unsafePartial (role `insertRoleInto` contextUnderConstruction))

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
traverseRoleE :: RoleE -> DomeinFileRecord -> String -> Except String (Tuple DomeinFileRecord Role)
traverseRoleE r df ns = if isCalculatedRole r
  then traverseCalculatedRoleE r df ns
  else traverseEnumeratedRoleE r df ns
  where
    isCalculatedRole :: RoleE -> Boolean
    -- isCalculatedRole _ = true
    isCalculatedRole (RoleE {roleParts}) = (isJust (findIndex (case _ of
      (Calculation _) -> true
      otherwise -> false) roleParts))

-- | Traverse a RoleE that results in an EnumeratedRole.
traverseEnumeratedRoleE :: RoleE -> DomeinFileRecord -> String -> Except String (Tuple DomeinFileRecord Role)
traverseEnumeratedRoleE (RoleE {id, kindOfRole, roleParts}) df@{enumeratedRoles} ns = do
  -- If we have a BotRole, we add its parts (perspectives with actions) to the UserRole
  -- that it serves.
  role@(EnumeratedRole{_id:roleName}) <-
    case kindOfRole of
      BotRole -> do
        servedUserLocalName <- userServedByBot roleParts
        servedUserId <- pure (ns <> "$" <> servedUserLocalName)
        case lookup servedUserId enumeratedRoles of
          (Just user) -> pure user
          Nothing -> pure (defaultEnumeratedRole servedUserId servedUserLocalName UserRole ns)
      UserRole -> do
        userId <- pure (ns <> "$" <> id)
        case lookup userId enumeratedRoles of
          (Just user) -> pure user
          Nothing -> pure (defaultEnumeratedRole userId id kindOfRole ns)
      otherwise -> do
        roleName <- pure (ns <> "$" <> id)
        pure (defaultEnumeratedRole roleName id kindOfRole ns)
  (Tuple domeinFile role') <- foldM (unsafePartial $ handleParts (unwrap roleName)) (Tuple df role) roleParts
  pure $ Tuple (addRoleToDomeinFile (E role') domeinFile) (E role')

  where

    -- Construct members for the new Role type according to the type of
    -- parts found in the RoleE AST type. Insert these members into the new Role.
    -- The Calculation case is not handled for an EnumeratedRole, hence the Partial constraint.
    handleParts :: Partial => String -> Tuple DomeinFileRecord EnumeratedRole -> RolePart -> Except String (Tuple DomeinFileRecord EnumeratedRole)

    -- PROPERTY
    handleParts roleName (Tuple domeinFile roleUnderConstruction) (PE pe) = do
      (Tuple domeinFile' property) <- traversePropertyE pe domeinFile roleName
      pure $ Tuple domeinFile' (property `insertPropertyInto` roleUnderConstruction)

    -- PERSPECTIVE
    -- handleParts (Tuple domeinFile roleUnderConstruction) (PRE pe) = let
    --   (Tuple domeinFile' perspective) = traversePerspectiveE pe domeinFile roleName
    --   in Tuple domeinFile' (property `insertPropertyInto` roleUnderConstruction)

    -- VIEW
    handleParts roleName (Tuple domeinFile (EnumeratedRole roleUnderConstruction@{views})) (VE pe) = do
      (Tuple domeinFile' viewType) <- traverseViewE pe domeinFile roleName
      pure $ Tuple domeinFile' (EnumeratedRole $ roleUnderConstruction {views = cons viewType views})

    -- FUNCTIONALATTRIBUTE
    handleParts roleName (Tuple domeinFile (EnumeratedRole roleUnderConstruction)) (FunctionalAttribute bool) = pure $
      Tuple domeinFile (EnumeratedRole $ roleUnderConstruction {functional = bool})

    -- MANDATORYATTRIBUTE
    handleParts roleName (Tuple domeinFile (EnumeratedRole roleUnderConstruction)) (MandatoryAttribute bool) = pure $
      Tuple domeinFile (EnumeratedRole $ roleUnderConstruction {mandatory = bool})

    -- FILLEDBYATTRIBUTE
    handleParts roleName (Tuple domeinFile (EnumeratedRole roleUnderConstruction@{binding})) (FilledByAttribute bnd) = pure $ Tuple domeinFile (EnumeratedRole $ roleUnderConstruction {binding = augmentADT binding bnd})

    -- FORUSER
    handleParts roleName (Tuple domeinFile (EnumeratedRole roleUnderConstruction)) (ForUser _) = pure $ Tuple domeinFile (EnumeratedRole $ roleUnderConstruction)

    userServedByBot :: List RolePart -> Except String String
    userServedByBot parts = let
      f = foldl
        (\found next -> if isJust found then found else case next of
          (ForUser user) -> Just user
          otherwise -> Nothing)
        Nothing
        parts
      in case f of
        (Just user) -> pure user
        otherwise -> throwError "A BotRole must have a 'ForUser' subexpression"

    -- TODO. We assume a sum type and we assume a qualified name. Handle PROD types.
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
traverseViewE :: ViewE -> DomeinFileRecord -> String -> Except String (Tuple DomeinFileRecord ViewType)
traverseViewE (ViewE {id, viewParts}) df@({views}) ns = let
  viewName = ns <> "$" <> id
  view = View
    { _id: ViewType viewName
    , _rev: Nothing
    , displayName: id
    , propertyReferences: map (qualifyProperty ns) (fromFoldable viewParts)
    , role: EnumeratedRoleType ns }
  in pure $ Tuple (df {views = insert viewName view views}) (ViewType viewName)
  where
    -- TODO. Is het een calculated of een enumerated property?
    -- | If the second argument is a fully qualified name, return it as a PropertyType;
    -- | otherwise, assume it is just a local name and prefix it with the namespace first.
    qualifyProperty :: String -> String -> PropertyType
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
traverseCalculatedRoleE :: RoleE -> DomeinFileRecord -> String -> Except String (Tuple DomeinFileRecord Role)
traverseCalculatedRoleE (RoleE {id, kindOfRole, roleParts}) df ns = do
  role <- pure (defaultCalculatedRole (ns <> "$" <> id) id kindOfRole ns)
  (Tuple domeinFile role') <- foldM (unsafePartial $ handleParts) (Tuple df role) roleParts
  pure $ Tuple (addRoleToDomeinFile (C role') domeinFile) (C role')
  -- in Tuple df (C role')

  where
    handleParts :: Partial => Tuple DomeinFileRecord CalculatedRole -> RolePart -> Except String (Tuple DomeinFileRecord CalculatedRole)
    -- Parse the query expression.
    handleParts (Tuple domeinFile roleUnderConstruction) (Calculation calc) =
      -- let
      -- TODO: actually call the parser.
      -- calculation = parseQuery calc
      -- in Tuple domeinFile' (roleUnderConstruction {calculation = calculation})
      pure $ Tuple domeinFile roleUnderConstruction

-- | Traverse the members of the PropertyE AST type to construct a new Property type
-- | and insert it into a DomeinFileRecord.
traversePropertyE :: PropertyE -> DomeinFileRecord -> String -> Except String (Tuple DomeinFileRecord Property.Property)
traversePropertyE r df ns = if isCalculatedProperty r
  then traverseCalculatedPropertyE r df ns
  else traverseEnumeratedPropertyE r df ns
  where
    isCalculatedProperty :: PropertyE -> Boolean
    isCalculatedProperty (PropertyE {propertyParts}) = (isJust (findIndex (case _ of
        (Calculation' _) -> true
        otherwise -> false) propertyParts))

traverseEnumeratedPropertyE :: PropertyE -> DomeinFileRecord -> String -> Except String (Tuple DomeinFileRecord Property.Property)
traverseEnumeratedPropertyE (PropertyE {id, range, propertyParts}) df ns = do
  property <- pure $ defaultEnumeratedProperty (ns <> "$" <> id) id ns range
  (Tuple domeinFile property') <- foldM (unsafePartial handleParts) (Tuple df property) propertyParts
  pure $ Tuple (addPropertyToDomeinFile (Property.E property') domeinFile) (Property.E property')

  where
    -- Construct members for the new Property type according to the type of
    -- parts found in the PropertyE AST type. Insert these members into the new Property.
    -- The Calculation case is not handled for an EnumeratedRole, hence the Partial constraint.
    handleParts :: Partial => Tuple DomeinFileRecord EnumeratedProperty -> PropertyPart -> Except String (Tuple DomeinFileRecord EnumeratedProperty)
    -- FUNCTIONALATTRIBUTE
    handleParts (Tuple domeinFile (EnumeratedProperty propertyUnderConstruction)) (FunctionalAttribute' bool) = pure $
      Tuple domeinFile (EnumeratedProperty $ propertyUnderConstruction {functional = bool})

    -- MANDATORYATTRIBUTE
    handleParts (Tuple domeinFile (EnumeratedProperty propertyUnderConstruction)) (MandatoryAttribute' bool) = pure $
      Tuple domeinFile (EnumeratedProperty $ propertyUnderConstruction {mandatory = bool})

-- | Traverse a PropertyE that results in an CalculatedProperty.
traverseCalculatedPropertyE :: PropertyE -> DomeinFileRecord -> String -> Except String (Tuple DomeinFileRecord Property.Property)
traverseCalculatedPropertyE (PropertyE {id, range, propertyParts}) df ns = do
  (CalculatedProperty property@{calculation}) <- pure $ defaultCalculatedProperty (ns <> "$" <> id) id ns
  calculation' <- case head propertyParts of
    -- TODO: actually call the parser, here.
    (Just (Calculation' c)) -> pure calculation
    otherwise -> pure calculation
  property' <- pure $ Property.C $ CalculatedProperty (property {calculation = calculation'})
  pure $ Tuple (addPropertyToDomeinFile property' df) property'

addPropertyToDomeinFile :: Property.Property -> DomeinFileRecord -> DomeinFileRecord
addPropertyToDomeinFile property df@{enumeratedProperties, calculatedProperties} = case property of
  (Property.E r@(EnumeratedProperty{_id})) -> df {enumeratedProperties = insert (unwrap _id) r enumeratedProperties}
  (Property.C r@(CalculatedProperty{_id})) -> df {calculatedProperties = insert (unwrap _id) r calculatedProperties}