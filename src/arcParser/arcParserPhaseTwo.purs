module Perspectives.Parsing.Arc.PhaseTwo where

import Data.Array (cons)
import Data.Foldable (foldl)
import Data.Lens (over)
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (insert)
import Perspectives.DomeinFile (DomeinFileRecord)
import Perspectives.Parsing.Arc.AST (ContextE(..), ContextPart(..), RoleE(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.Role (Role(..))
import Perspectives.Representation.Context (Context(..), defaultContext)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..), defaultEnumeratedRole)
import Perspectives.Representation.TypeIdentifiers (ContextType(..), RoleKind(..), RoleType(..))
import Prelude (($), (<>))

-- | Traverse the members of the ContextE AST type to construct a new Context type
-- | and insert it into a DomeinFileRecord.
traverseContextE :: ContextE -> DomeinFileRecord -> String -> Tuple DomeinFileRecord Context
traverseContextE (ContextE {id, kindOfContext, contextParts}) df ns = let
  context = defaultContext (ns <> "$" <> id) id kindOfContext
  r@(Tuple domeinFile context') = foldl handleParts (Tuple df context) contextParts
  in Tuple (addContextToDomeinFile context' domeinFile) context'
  where
    -- Construct members for the new Context type according to the type of
    -- parts found in the ContextE AST type. Insert these members into the new Context.
    handleParts :: Tuple DomeinFileRecord Context -> ContextPart -> Tuple DomeinFileRecord Context
    -- Construct a nested Context.
    handleParts (Tuple domeinFile contextUnderConstruction) (CE c) = let
      (Tuple domeinFile' subContext) = traverseContextE c domeinFile (ns <> "$" <> id)
      in Tuple domeinFile' (subContext `insertInto` contextUnderConstruction)
    -- Construct a Role
    handleParts (Tuple domeinFile contextUnderConstruction) (RE r) = let
      (Tuple domeinFile' role) = traverseRoleE r domeinFile (ns <> "$" <> id)
      in Tuple domeinFile' (role `insertRoleInto` contextUnderConstruction)

    addContextToDomeinFile :: Context -> DomeinFileRecord -> DomeinFileRecord
    addContextToDomeinFile c@(Context{_id: (ContextType ident)}) domeinFile = over
      (prop (SProxy :: SProxy "contexts"))
      (insert ident c)
      domeinFile

    -- Insert a sub-Context type into a Context type.
    insertInto :: Context -> Context -> Context
    insertInto (Context{_id}) (Context cr@{nestedContexts}) = Context $ cr {nestedContexts = cons _id nestedContexts}

    -- Insert a Role type into a Context type.
    insertRoleInto :: Role -> Context -> Context
    insertRoleInto (E (EnumeratedRole {_id, kindOfRole})) c = case kindOfRole, c of
      RoleInContext, (Context cr@{rolInContext}) -> Context $ cr {rolInContext = cons (ENR _id) rolInContext}
      ContextRole, (Context cr@{contextRol}) -> Context $ cr {contextRol = cons (ENR _id) contextRol}
      ExternalRole, (Context cr) -> Context $ cr {externeRol = _id}
      UserRole, (Context cr@{gebruikerRol}) -> Context $ cr {gebruikerRol = cons _id gebruikerRol}
      BotRole, (Context cr@{botRol}) -> Context $ cr {botRol = cons _id botRol}

    insertRoleInto (C (CalculatedRole {_id, kindOfRole})) c = case kindOfRole, c of
      RoleInContext, (Context cr@{rolInContext}) -> Context $ cr {rolInContext = cons (CR _id) rolInContext}
      ContextRole, (Context cr@{contextRol}) -> Context $ cr {contextRol = cons (CR _id) contextRol}
      -- A catchall case that just returns the context. Calculated roles for ExternalRole,
      -- UserRole and BotRole should be ignored.
      _, _ -> c

-- | Traverse the members of the RoleE AST type to construct a new Role type
-- | and insert it into a DomeinFileRecord.
traverseRoleE :: RoleE -> DomeinFileRecord -> String -> Tuple DomeinFileRecord Role
traverseRoleE (RoleE {id, kindOfRole, roleParts}) df ns = Tuple df (E (defaultEnumeratedRole (ns <> "$" <> id) id))
