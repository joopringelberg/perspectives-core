module Perspectives.Query.Compiler where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Exception (error)
import Perspectives.ContextAndRole (context_rolInContext)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Identifiers (deconstructNamespace)
import Perspectives.Instances (getPerspectEntiteit)
import Perspectives.Query.DescriptionCompiler (compileElementaryStep)
import Perspectives.Query.QueryTypes (QueryFunctionDescription(..))
import Perspectives.QueryAST (ElementaryQueryStep(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole, calculation)
import Perspectives.Representation.Class.Persistent (getPerspectType)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Perspectives.StringTripleGetterConstructors (StringTypedTripleGetter)
import Perspectives.TripleGetters.TrackedAs (trackedAs)

compileQuery :: QueryFunctionDescription -> MonadPerspectives StringTypedTripleGetter

-- ROLGETTER
compileQuery (QD _ (RolGetter (ENR (EnumeratedRoleType r))) _) = pure $ (getPerspectEntiteit >=> pure <<< (flip context_rolInContext r)) `trackedAs` r
compileQuery (QD _ (RolGetter (CR cr)) _) = do
  (ct :: CalculatedRole) <- getPerspectType cr
  compileQuery (calculation ct)


-- The last case
compileQuery _ = throwError (error "Unknown QueryFunction expression")

{-
getPropertyFunction ::
  String ->
  MonadPerspectives StringTypedTripleGetter
getPropertyFunction = constructGetter QualifiedProperty

getInternalPropertyFunction ::
  String ->
  MonadPerspectives StringTypedTripleGetter
getInternalPropertyFunction = constructGetter QualifiedInternalProperty

getRolFunction ::
  String ->
  MonadPerspectives StringTypedTripleGetter
getRolFunction = constructGetter QualifiedRol

-- getUnqualifiedRolFunction ::
--   String ->
--   String ->
--   MonadPerspectives StringTypedTripleGetter
-- getUnqualifiedRolFunction = constructUnqualifiedGetter UnqualifiedRol

-- | Returns a getter, lookup function or compiled query.
constructGetter ::
  (String -> ElementaryQueryStep) ->
  String ->
  MonadPerspectives StringTypedTripleGetter
constructGetter queryAstConstructor pn = do
  -- Is the property Enumerated, or is it Calculated? We don't know.
  -- We must try. What we get will be a PropertyType.
  -- Best approach is probably to deconstruct the rolename,
  -- then try to get it either as an EnumeratedRole or a CalculatedRole.
  -- Either way, provide it to the compiler as such. Or maybe we need
  -- to take into account the as yet unrepresented result type of a Calculated Role.
  prop <- getPerspectType (PropertyType pn)
  -- Now get the role (but notice: the property can be enumerated or calculated)
  -- Then use that as domain for the QueryCompiler.
  mrn <- pure $ deconstructNamespace pn
  case mrn of
    Nothing -> throwError (error $ "invalid name: " <> pn)
    (Just rn) -> do
      -- The QueryDescriptionCompiler checks if the Rol defines the Property.
      -- That check is of no importance here, as it leads to the same code.
      -- Hence we use the namespace of the Property as the name of the Rol.
      r <- runMonadPerspectivesQueryCompiler rn (compileElementaryStep (queryAstConstructor pn) (pn <> "_getterDescription"))
      case r of
        (Left m) -> throwError $ error $ show m
        (Right descriptionId) -> compileQuery descriptionId
-}
