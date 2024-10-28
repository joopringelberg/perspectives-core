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

module Perspectives.Representation.Class.Role where

import Perspectives.Representation.ADT

import Control.Monad.Error.Class (throwError)
import Control.Plus (empty, map, (<|>))
import Data.Array (catMaybes, cons, nub, null)
import Data.Map (Map)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception (error)
import Foreign.Object (Object, toArrayWithKey)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MP, MonadPerspectives)
import Perspectives.Identifiers (buitenRol)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), RoleInContext(..), domain2roleInContext, range, roleInContext2Context, roleInContext2Role, roleRange)
import Perspectives.Query.QueryTypes (functional, mandatory) as QT
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.CNF (CNF, distribute, flattenProducts)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.Context (contextADT, roles)
import Perspectives.Representation.Class.Identifiable (class Identifiable, identifier, identifier_)
import Perspectives.Representation.Class.PersistentType (class PersistentType, ContextType, getCalculatedRole, getContext, getEnumeratedRole, getPerspectType)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.ExpandedADT (ExpandedADT(..))
import Perspectives.Representation.Perspective (Perspective, StateSpec)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.ThreeValuedLogic (bool2threeValued, pessimistic)
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), EnumeratedPropertyType, EnumeratedRoleType(..), PropertyType, RoleKind, RoleType(..), ViewType)
import Prelude (class Ord, class Show, bind, identity, pure, ($), (&&), (<$>), (<<<), (<>), (>=>), (>>=))

-----------------------------------------------------------
-- ROLE TYPE CLASS
-----------------------------------------------------------
class (Show r, Identifiable r i, PersistentType r i) <= RoleClass r i | r -> i, i -> r where
  typeOfRole :: r -> RoleType
  -- | Reliabley an ST or a UET.
  roleADT :: r -> MP (ADT RoleInContext)
  kindOfRole :: r -> RoleKind
  context :: r -> ADT ContextType
  contextOfRepresentation :: r -> ContextType
  binding :: r -> MonadPerspectives (Maybe (ADT RoleInContext))
  functional :: r -> MonadPerspectives Boolean
  mandatory :: r -> MonadPerspectives Boolean
  calculation :: r -> MonadPerspectives QueryFunctionDescription
  perspectives :: r -> Array Perspective
  contextActions :: r -> Map StateSpec (Object Action)
  displayName :: r -> String
  pos :: r -> ArcPosition
  
-----------------------------------------------------------
-- CALCULATED ROLE INSTANCE
-----------------------------------------------------------
instance calculatedRoleRoleClass :: RoleClass CalculatedRole CalculatedRoleType where
  typeOfRole r = CR (unwrap r).id
  roleADT r = calculation r >>= pure <<< unsafePartial domain2roleInContext <<< range
  kindOfRole r = (unwrap r).kindOfRole
  context r = unsafePartial case (unwrap r).calculation of
    Q calc -> contextOfADT (unsafePartial roleRange calc)
  contextOfRepresentation r = (unwrap r).context
  binding r = pure $ Nothing
  functional r = calculation r >>= pure <<< pessimistic <<< QT.functional
  mandatory r = calculation r >>= pure <<< pessimistic <<< QT.mandatory
  calculation r = case (unwrap r).calculation of
    Q qd -> pure qd
    otherwise -> throwError (error ("Attempt to acces QueryFunctionDescription of a CalculatedRole before the expression has been compiled. This counts as a system programming error." <> (unwrap $ (identifier r :: CalculatedRoleType))))
  perspectives r = (unwrap r).perspectives
  contextActions r = unwrap (unwrap r).actions
  displayName r = (unwrap r).displayName
  pos r = (unwrap r).pos

-----------------------------------------------------------
-- ENUMERATED ROLE INSTANCE
-----------------------------------------------------------
instance enumeratedRoleRoleClass :: RoleClass EnumeratedRole EnumeratedRoleType where
  typeOfRole r = ENR (unwrap r).id
  roleADT = pure <<< declaredType
  kindOfRole r = (unwrap r).kindOfRole
  context r = UET $ contextOfRepresentation r
  contextOfRepresentation r = (unwrap r).context
  binding (EnumeratedRole r) = pure r.binding
  functional r = pure (unwrap r).functional
  mandatory r = pure (unwrap r).mandatory
  calculation r = if (unwrap r).unlinked
    then pure $ SQD
      (CDOM $ context r)
      (DataTypeGetterWithParameter GetRoleInstancesForContextFromDatabaseF (identifier_ r))
      (RDOM $ declaredType r)
      (bool2threeValued (unwrap r).functional)
      (bool2threeValued (unwrap r).mandatory)
    else pure $ SQD
      (CDOM $ ST $ contextOfRepresentation r)
      (RolGetter (ENR (identifier r)))
      (RDOM $ declaredType r)
      (bool2threeValued (unwrap r).functional)
      (bool2threeValued (unwrap r).mandatory)
  perspectives r = (unwrap r).perspectives
  contextActions r = unwrap (unwrap r).actions
  displayName r = (unwrap r).displayName
  pos r = (unwrap r).pos

-----------------------------------------------------------
-- VARIOUS DECLARED TYPES OF ENUMERATEDROLE
-----------------------------------------------------------
-- | The simple type of the role as it has been declared, expressed as an Abstract Data Type of role in context.
-- | Only if the type has no aspects and no filler restriction do we construct it as a Simple Type (ST); 
-- | otherwise we construct an UnExpanded Type (UET)
declaredType :: EnumeratedRole -> (ADT RoleInContext)
declaredType (EnumeratedRole r) = if null r.roleAspects && isNothing r.binding
  then ST $ RoleInContext {context: r.context, role: r.id}
  else UET $ RoleInContext {context: r.context, role: r.id}

-- | The (possibly compound) restriction on fillers of the role as it has been declared, expressed as an Abstract Data Type of role in context.
declaredFillerRestriction :: EnumeratedRole -> Maybe (ADT RoleInContext)
declaredFillerRestriction (EnumeratedRole r) = r.binding

-- | The sum of the declared types of the aspects of the role, if any, expressed as an Abstract Data Type of role in context.
declaredAspects :: EnumeratedRole -> MP (ADT RoleInContext)
declaredAspects (EnumeratedRole r) = PROD <$> (for r.roleAspects (map declaredType <<< getEnumeratedRole <<< roleInContext2Role))

-- | The product of the declared filler restrictions of the declared aspects of the role, expressed as an Abstract Data Type of role in context.
-- | Notice that the product may be empty!
declaredAspectFillerRestrictions :: EnumeratedRole -> MP (ADT RoleInContext)
declaredAspectFillerRestrictions (EnumeratedRole r) = PROD <<< catMaybes <$> (for r.roleAspects (map declaredFillerRestriction <<< getEnumeratedRole <<< roleInContext2Role))

-- | The product of the declared filler restrictions of the role and of the declared aspect filler restrictions, possibly Nothing.
completeDeclaredFillerRestriction :: EnumeratedRole -> MP (Maybe (ADT RoleInContext))
completeDeclaredFillerRestriction role@(EnumeratedRole r) = do 
  dafr <- declaredAspectFillerRestrictions role
  asr <- unsafePartial case dafr of 
    PROD x -> pure x
  case declaredFillerRestriction role of
    Nothing -> if null asr 
      then pure $ Nothing 
      else pure $ Just $ PROD asr
    Just (dfr :: ADT (RoleInContext)) -> if null asr
      then pure $ Just dfr
      else pure $ Just $ PROD $ cons dfr asr

-- | The sum of the declared aspects and the declared type.
declaredTypeWithoutFiller :: EnumeratedRole -> MP (ADT RoleInContext)
declaredTypeWithoutFiller role@(EnumeratedRole r) = do
  aspectTypes <- declaredAspects role
  pure $ PROD [(declaredType role), aspectTypes]

-- | The sum of the declared aspect types, the complete declared filler restriction and the aspect types.
completeDeclaredType :: EnumeratedRole -> MP (ADT RoleInContext)
completeDeclaredType role@(EnumeratedRole r) = do
  -- Will be a PROD.
  aspectTypes <- declaredAspects role
  case declaredFillerRestriction role, aspectTypes of 
    Nothing, PROD [] -> pure $ declaredType role
    Nothing, _ -> pure $ PROD [(declaredType role), aspectTypes]
    Just fr, PROD [] -> pure $ PROD [(declaredType role), fr]
    Just fr, _ -> pure $ PROD [(declaredType role), fr, aspectTypes]

-----------------------------------------------------------
-- TYPE EXPANSION
-----------------------------------------------------------
-- | The recursive ('complete') expansion of the ADT. This takes care of aspects and fillers.
expandUnexpandedLeaves :: ADT RoleInContext -> MP (ExpandedADT RoleInContext)
expandUnexpandedLeaves = expand $ unsafePartial 
  -- the function below is intentionally and responsibly Partial, because 
  -- by construction `expand` only applies it to the two cases below.
  case _ of 
    (ST a) -> pure $ EST a
    UET a -> do 
      role@(EnumeratedRole{roleAspects, binding}) <- getEnumeratedRole (roleInContext2Role a)
      -- Expand filler restriction
      (mexpandedFiller :: Maybe (ExpandedADT RoleInContext)) <- traverse expandUnexpandedLeaves binding
      -- Expand aspects
      expandedAspects' <- for roleAspects (getEnumeratedRole <<< roleInContext2Role >=> completeExpandedType)
      -- the function below is responsibly Partial, because
      -- we know by construction that the declared type of a role 
      -- is either ST or UET.
      case declaredType role of 
        ST ric -> case mexpandedFiller of 
          Nothing -> pure $ ECT (EST ric) (EPROD $ nub $ cons (EST a) expandedAspects')
          Just expandedFiller -> pure $ ECT (EST ric) (EPROD $ nub $ cons (EST a) (cons expandedFiller expandedAspects'))
        UET ric -> case mexpandedFiller of 
          Nothing -> pure $ ECT (EST ric) (EPROD $ nub $ cons (EST a) expandedAspects')
          Just expandedFiller -> pure $ ECT (EST ric) (EPROD $ nub $ cons (EST a) (cons expandedFiller expandedAspects'))

-- | The recursive expansion of all aspects of the ADT. NOT INCLUDING FILLERS!
expandAspects :: ADT RoleInContext -> MP (ExpandedADT RoleInContext)
expandAspects = expand $ unsafePartial 
  -- the function below is intentionally and responsibly Partial, because 
  -- by construction `expand` only applies it to the two cases below.
  case _ of 
    (ST a) -> pure $ EST a
    UET a -> do 
      role@(EnumeratedRole{roleAspects}) <- getEnumeratedRole (roleInContext2Role a)
      expandedAspects' <- for roleAspects (getEnumeratedRole <<< roleInContext2Role >=> declaredTypeWithoutFiller >=> expandAspects)
      -- the function below is responsibly Partial, because
      -- we know by construction that the declared type of a role 
      -- is either ST or UET.
      case declaredType role of 
        ST ric -> pure $ ECT (EST ric) (EPROD $ nub $ cons (EST a) expandedAspects')
        UET ric -> pure $ ECT (EST ric) (EPROD $ nub $ cons (EST a) expandedAspects')

typeToRole :: ExpandedADT RoleInContext -> MP (ExpandedADT EnumeratedRole)
typeToRole = traverse (getEnumeratedRole <<< roleInContext2Role)

-----------------------------------------------------------
-- VARIOUS EXPANDED TYPES OF ENUMERATEDROLE
-----------------------------------------------------------
-- | The sum of the declared types of the aspects of the role, if any, expressed as an Abstract Data Type of role in context.
expandedAspects :: EnumeratedRole -> MP (ExpandedADT RoleInContext)
expandedAspects = declaredAspects >=> expandAspects

completeExpandedType :: EnumeratedRole -> MP (ExpandedADT RoleInContext)
completeExpandedType r = completeDeclaredType r >>= expandUnexpandedLeaves

-- | The recursive ('complete') expansion of the declared filler restriction. This includes all aspects.
completeExpandedFillerRestriction :: EnumeratedRole -> MP (Maybe (ExpandedADT RoleInContext))
completeExpandedFillerRestriction = completeDeclaredFillerRestriction >=> traverse expandUnexpandedLeaves

-----------------------------------------------------------
-- GETROLEADT
-----------------------------------------------------------
getRoleADT :: RoleType -> MP (ADT RoleInContext)
getRoleADT (ENR enr) = getEnumeratedRole enr >>= roleADT
getRoleADT (CR cr) = getCalculatedRole cr >>= roleADT 

-----------------------------------------------------------
-- EXPAND ROLETYPE
-----------------------------------------------------------
completeExpandedRoleType :: RoleType -> MP (ExpandedADT RoleInContext)
completeExpandedRoleType (ENR enr) = getEnumeratedRole enr >>= completeExpandedType
completeExpandedRoleType (CR cr) = getCalculatedRole cr >>= 
  roleADT >>= 
  expandUnexpandedLeaves

-----------------------------------------------------------
-- ADT RoleInContext TO DNF
-- NOTICE: this function can be applied only when member `completeType` of EnumeratedRoleType has been constructed in Perspectives.Parsing.Arc.PhaseThree.
-----------------------------------------------------------
toConjunctiveNormalForm_ :: ADT RoleInContext -> MP (CNF RoleInContext)
toConjunctiveNormalForm_ adt = case adt of 
  ST (RoleInContext{role}) -> getEnumeratedRole role >>= pure <<< _.completeType <<< unwrap
  -- In the disjunctive normal form we have no UET.
  -- We have a tree built from EST, ECT, ESUM and EPROD.
  UET (RoleInContext{role}) -> getEnumeratedRole role >>= pure <<< _.completeType <<< unwrap
  SUM as -> for as toConjunctiveNormalForm_ >>= pure <<< unsafePartial distribute
  PROD as -> for as toConjunctiveNormalForm_ >>= pure <<< unsafePartial flattenProducts


-----------------------------------------------------------
-- CONTEXTOFADT
-----------------------------------------------------------
-- | Context 'mapped' over ADT. Note that we cannot just use the Functor instance, as we have to 
-- | transform the label as well.
contextOfADT :: ADT RoleInContext -> (ADT ContextType)
contextOfADT = map roleInContext2Context

-----------------------------------------------------------
-- FUNCTIONS ON ROLE
-----------------------------------------------------------
data Role = E EnumeratedRole | C CalculatedRole

id :: forall r i. RoleClass r i => r -> i
id = identifier

getCalculation :: Role -> MonadPerspectives QueryFunctionDescription
getCalculation (E r) = calculation r
getCalculation (C r) = calculation r

adtOfRole :: Role -> MP (ADT RoleInContext)
adtOfRole (E e) = pure $ declaredType e
adtOfRole (C c) = rangeOfCalculatedRole c

contextOfRole :: Role -> ADT ContextType
contextOfRole (E e) = context e
contextOfRole (C c) = context c

perspectivesOfRole :: Role -> Array Perspective
perspectivesOfRole (E e) = perspectives e
perspectivesOfRole (C e) = perspectives e

roleIsFunctional :: Role -> MonadPerspectives Boolean
roleIsFunctional (E e) = functional e
roleIsFunctional (C e) = functional e

identifierOfRole :: Role -> String
identifierOfRole (E e) = identifier_ e
identifierOfRole (C e) = identifier_ e

posOfRole :: Role -> ArcPosition
posOfRole (E e) = pos e
posOfRole (C e) = pos e
-----------------------------------------------------------
-- FUNCTIONS ON ROLETYPE
-----------------------------------------------------------
getRole :: RoleType -> MonadPerspectives Role
getRole (ENR e) = getPerspectType e >>= pure <<< E
getRole (CR c) = getPerspectType c >>= pure <<< C

-- | The range of the computation of the RoleType.
-- | Does not include the binding, for (ENR (EnumeratedRoleType e)).
rangeOfRoleCalculation :: RoleType -> MonadPerspectives (ADT EnumeratedRoleType)
rangeOfRoleCalculation = getRole >=> getCalculation >=> case _ of
    SQD _ _ (RDOM adt) _ _ -> pure $ roleInContext2Role <$> adt
    UQD _ _ _ (RDOM adt) _ _ -> pure $ roleInContext2Role <$> adt
    BQD _ _ _ _ (RDOM adt) _ _ -> pure $ roleInContext2Role <$> adt
    MQD _ _ _ (RDOM adt) _ _ -> pure $ roleInContext2Role <$> adt
    otherwise -> empty -- NB: The Alt instance of Aff throws an error on empty!

roleTypeIsFunctional :: RoleType -> MonadPerspectives Boolean
roleTypeIsFunctional = getRole >=> (case _ of
  E r -> functional r
  C r -> functional r)

roleTypeIsMandatory :: RoleType -> MonadPerspectives Boolean
roleTypeIsMandatory = getRole >=> (case _ of
  E r -> mandatory r
  C r -> mandatory r)

bindingOfRole :: RoleType -> MonadPerspectives (Maybe (ADT RoleInContext))
bindingOfRole = getRole >=> binding'
  where
    binding' :: Role -> MonadPerspectives (Maybe (ADT RoleInContext))
    binding' (E r) = binding r
    binding' (C r) = binding r

contextOfRoleType :: RoleType -> MonadPerspectives (ADT ContextType)
contextOfRoleType (ENR e) = getPerspectType e >>= pure <<< context
contextOfRoleType (CR c) = getPerspectType c >>= pure <<< context

contextOfRepresentationOfRole :: RoleType -> MonadPerspectives (ADT ContextType)
contextOfRepresentationOfRole (ENR e) = getPerspectType e >>= pure <<< contextOfRepresentation >>= getPerspectType >>= pure <<< contextADT
contextOfRepresentationOfRole (CR c) = getPerspectType c >>= pure <<< contextOfRepresentation >>= getPerspectType >>= pure <<< contextADT

perspectivesOfRoleType :: RoleType -> MonadPerspectives (Array Perspective)
perspectivesOfRoleType (ENR e) = getPerspectType e >>= pure <<< perspectives
perspectivesOfRoleType (CR c) = getPerspectType c >>= pure <<< perspectives

roleKindOfRoleType :: RoleType -> MonadPerspectives RoleKind
roleKindOfRoleType (ENR e) = getPerspectType e >>= pure <<< kindOfRole
roleKindOfRoleType (CR c) = getPerspectType c >>= pure <<< kindOfRole

roleTypeIsEnumerated :: RoleType -> Boolean
roleTypeIsEnumerated (ENR _) = true
roleTypeIsEnumerated _ = false

actionsOfRoleType :: RoleType -> MonadPerspectives (Map StateSpec (Object Action))
actionsOfRoleType (ENR r) = getPerspectType r >>= pure <<< contextActions
actionsOfRoleType (CR r) = getPerspectType r >>= pure <<< contextActions

roleADTOfRoleType :: RoleType -> MonadPerspectives (ADT RoleInContext)
roleADTOfRoleType (ENR r) = getPerspectType r >>= pure <<< declaredType
roleADTOfRoleType (CR r) = getPerspectType r >>= rangeOfCalculatedRole

rangeOfCalculatedRole :: CalculatedRole -> MonadPerspectives (ADT RoleInContext)
rangeOfCalculatedRole cr = calculation cr >>= roleCalculationRange
  where
    roleCalculationRange :: QueryFunctionDescription -> MonadPerspectives (ADT RoleInContext)
    roleCalculationRange qfd = case range qfd of
      (RDOM x) -> pure x
      otherwise -> throwError (error ("range of calculation of a calculated role is not a role Domain."))

displayNameOfRoleType :: RoleType -> MonadPerspectives String
displayNameOfRoleType (ENR e) = getEnumeratedRole e >>= pure <<< displayName
displayNameOfRoleType (CR e) = getCalculatedRole e >>= pure <<< displayName

calculationOfRoleType :: RoleType -> MonadPerspectives QueryFunctionDescription
calculationOfRoleType (ENR e) = getEnumeratedRole e >>= calculation
calculationOfRoleType (CR e) = getCalculatedRole e >>= calculation

-----------------------------------------------------------
-- FUNCTIONS ON CALCULATEDROLETYPE
-----------------------------------------------------------
getRoleType :: String -> MonadPerspectives RoleType
getRoleType s = ((getEnumeratedRole $ EnumeratedRoleType s) >>= pure <<< ENR <<< identifier)
  <|> ((getCalculatedRole $ CalculatedRoleType s) >>= pure <<< CR <<< identifier)

getRoleADTFromString :: CalculatedRoleType -> MonadPerspectives (ADT RoleInContext)
getRoleADTFromString s = getCalculatedRole s >>= calculation >>= pure <<< unsafePartial roleRange

--------------------------------------------------------------------------------------------------
---- TWO RECURRING PATTERNS: COLLECTING PART OF AN ENUMERATED ROLE OVER AN ADT ROLEINCONTEXT
--------------------------------------------------------------------------------------------------
-- | From an EnumeratedRole, extract or compute an Array of a.
type ExtractorOnEnumeratedRole a = EnumeratedRole -> Array a

-- | From an EnumeratedRole, collect a's over its expansion.
type CollectorOverEnumeratedRoleExpansion a = EnumeratedRole -> MP (Array a)

-- | From an EnumeratedRole, compute over its complete expansion a set by applying the extractor to each leaf in the expansion.
collectOverEnumeratedRoleExpansion :: forall a. Ord a => ExtractorOnEnumeratedRole a -> CollectorOverEnumeratedRoleExpansion a
collectOverEnumeratedRoleExpansion f role = 
  completeExpandedType role >>= 
  typeToRole >>= 
  pure <<< computeExpandedCollection f

-- | From an ADT RoleInContext, compute over its entire structure a set by applying the collector to each leaf.
collectOverRoleInContextADT :: forall a. Ord a => CollectorOverEnumeratedRoleExpansion a -> ADT EnumeratedRoleType -> MP (Array a)
collectOverRoleInContextADT f = traverse (getEnumeratedRole >=> f) >=> pure <<< computeCollection identity

-- | Expand an EnumeratedRole over Aspects, then apply a monadic function to all leaves and collect the results.
collectOverEnumeratedRoleExpansionWithoutFillers :: forall a. Ord a => ExtractorOnEnumeratedRole a -> CollectorOverEnumeratedRoleExpansion a
collectOverEnumeratedRoleExpansionWithoutFillers f role = 
  declaredTypeWithoutFiller role >>= 
  expandAspects >>= 
  typeToRole >>= 
  pure <<< computeExpandedCollection f

--------------------------------------------------------------------------------------------------
---- PROPERTY SETS
--------------------------------------------------------------------------------------------------
-- | All properties, computed recursively over every role type and its Aspects - but in every case excluding the binding.
allLocallyRepresentedProperties :: ADT EnumeratedRoleType -> MP (Array PropertyType)
allLocallyRepresentedProperties = collectOverRoleInContextADT allLocallyOnRoleRepresentedProperties

-- | All properties, computed recursively over the role type and its Aspects - but in every case excluding the binding.
-- | Notice how this function diverges from the CollectorOverEnumeratedRoleExpansion pattern in that it does not 
-- | include restrictions on fillers (bindings).
allLocallyOnRoleRepresentedProperties :: EnumeratedRole -> MP (Array PropertyType)
allLocallyOnRoleRepresentedProperties = collectOverEnumeratedRoleExpansionWithoutFillers (_.properties <<< unwrap)

-- | All properties, computed recursively over binding and Aspects, of the Role ADT.
allProperties :: ADT EnumeratedRoleType -> MP (Array PropertyType)
allProperties = collectOverRoleInContextADT allRoleProperties

allRoleProperties :: EnumeratedRole -> MP (Array PropertyType)
allRoleProperties = collectOverEnumeratedRoleExpansion (_.properties <<< unwrap)

allLocalAliases :: ADT EnumeratedRoleType -> MP (Array (Tuple String EnumeratedPropertyType))
allLocalAliases = collectOverRoleInContextADT allRoleAliases

allRoleAliases :: EnumeratedRole -> MP (Array (Tuple String EnumeratedPropertyType))
allRoleAliases = collectOverEnumeratedRoleExpansionWithoutFillers ((toArrayWithKey Tuple) <<< _.propertyAliases <<< unwrap)

--------------------------------------------------------------------------------------------------
---- ROLE SETS
--------------------------------------------------------------------------------------------------
-- | The collection of roles of every context type occurring in the ADT as is (NOT expanded!)
-- | The computation folds with intersection over PROD and with union over SUM.
allRoles :: ADT ContextType -> MP (Array RoleType)
allRoles = traverse getContext >=> pure <<< computeCollection roles

-----------------------------------------------------------
-- VIEW SETS
-----------------------------------------------------------
-- | All views, computed recursively over binding and Aspects, of the Role ADT.
allViews :: ADT EnumeratedRoleType -> MP (Array ViewType)
allViews = collectOverRoleInContextADT allViewsOfRole
 
allViewsOfRole :: EnumeratedRole -> MP (Array ViewType)
allViewsOfRole = collectOverEnumeratedRoleExpansion (_.views <<< unwrap)

-----------------------------------------------------------
-- FILLEROFADT
-----------------------------------------------------------
-- | The notion of a 'filler' of an abstract data type is contentious, as only
-- | instances are actually filled and in compile time we deal with restrictions on fillers
-- | rather than fillers themselves. 
-- | Nevertheless, we can imagine that we leave the structure of an ADT mostly intact 
-- | but replace terminals (RoleInContexts) with the binding restriction of the EnumeratedRole.
-- | As such a binding restriction can be a compound ADT, this means the structure can grow.
-- | However, a binding restriction is optional; hence the structure can shrink, too.
-- | NOTE that we do not expand UET nodes (we do not replace them with their Complete Type).
-- | This is because we are interested in the fillers of the actual types.
-- TODO: hernoem naar fillerOfADT
bindingOfADT :: ADT RoleInContext -> MP (Maybe (ADT RoleInContext))
bindingOfADT = expandAndReduce (unsafePartial fillerADT)
  where 
    fillerADT :: Partial => ADT RoleInContext -> MP (Maybe (ADT RoleInContext))
    fillerADT adt = case adt of 
      ST a -> getEnumeratedRole (roleInContext2Role a) >>= pure <<< declaredFillerRestriction
      UET a -> getEnumeratedRole (roleInContext2Role a) >>= pure <<< declaredFillerRestriction

-----------------------------------------------------------
-- BOOLEANS
-----------------------------------------------------------
adtIsFunctional :: ADT RoleInContext -> MonadPerspectives Boolean
adtIsFunctional adt = 
  expandUnexpandedLeaves adt >>=
  typeToRole >>=
  pure <<< computeExpandedBoolean \(EnumeratedRole{functional}) -> functional

-----------------------------------------------------------
-- EXTERNALROLEOFADT
-----------------------------------------------------------
-- | The external role of an ADT ContextType
externalRoleOfADT :: ADT ContextType -> MP (ADT RoleInContext)
-- TODO: handle CalculatedRole.
externalRoleOfADT = pure <<< map \ctype -> RoleInContext {context: ctype, role: EnumeratedRoleType $ buitenRol (unwrap ctype)}
