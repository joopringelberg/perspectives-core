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

module Perspectives.Query.Kinked where

import Control.Alternative (guard)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, elemIndex, foldr, head, intercalate, last, snoc, unsnoc)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (for, traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.InvertedQuery (QueryWithAKink(..), backwards)
import Perspectives.ModelDependencies (mySystem)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, addBinding, lift2, lookupVariableBinding, throwError, withFrame)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.Inversion (invertFunction, queryFunctionIsFunctional, queryFunctionIsMandatory)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), RoleInContext, composeOverMaybe, domain, makeComposition, range, replaceDomain, roleInContext2Role)
import Perspectives.Representation.ADT (ADT, allLeavesInADT)
import Perspectives.Representation.Class.PersistentType (getCalculatedProperty)
import Perspectives.Representation.Class.Property (calculation)
import Perspectives.Representation.Class.Role (allLocallyRepresentedProperties, bindingOfADT, getCalculation, getRole)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (PropertyType(..), RoleType(..))
import Perspectives.Utilities (class PrettyPrint, prettyPrint, prettyPrint')
import Prelude (class Show, append, bind, discard, eq, join, map, pure, show, ($), (<$>), (<*>), (<<<), (<>), (==), (>=>), (>>=))

--------------------------------------------------------------------------------------------------------------
---- QUERYWITHAKINK
--------------------------------------------------------------------------------------------------------------
-- This we use in the invert_ function. The first part is backwards-facing (inverted). It is an array of separate steps,
-- each one inverted, in inverse order of the original query.
-- The second part is forwards-facing (not inverted).
data QueryWithAKink_ = ZQ_ (Array QueryFunctionDescription) (Maybe QueryFunctionDescription)

derive instance genericQueryWithAKink_ :: Generic QueryWithAKink_ _

instance showQueryWithAKink_ :: Show QueryWithAKink_ where
  show = genericShow

instance prettyPrintQueryWithAKink_ :: PrettyPrint QueryWithAKink_ where
  prettyPrint' tab (ZQ_ qfds mqfd) = "QueryWithAKink_\n[" <> (intercalate (",\n" <> tab) (prettyPrint' (tab <> "  ") <$> qfds) <> "]\n" <> prettyPrint' (tab <> "  ") mqfd)

--------------------------------------------------------------------------------------------------------------
---- COMPLETEINVERSIONS
--------------------------------------------------------------------------------------------------------------
-- | Inverted queries without a backwards part contribute nothing to the result of this function.
completeInversions :: QueryFunctionDescription -> PhaseThree (Array QueryFunctionDescription)
completeInversions = invert >=> pure <<< catMaybes <<< map f
  where
    f :: QueryWithAKink -> Maybe QueryFunctionDescription
    f qwk = if isCompleteInverse qwk then backwards qwk else Nothing

    -- | Is the original query completely inversed? (as opposed to: is this a partial inversion).
    -- | By definition this is true if the forwards part is Nothing.
    isCompleteInverse :: QueryWithAKink -> Boolean
    isCompleteInverse (ZQ _ Nothing) = true
    isCompleteInverse _ = false


--------------------------------------------------------------------------------------------------------------
---- INVERT
--------------------------------------------------------------------------------------------------------------
-- | From a query, return a collection of paths that run from some station in the original direction to the query end
-- | result and in reverse direction to the query start.
-- | The original query is 'kinked' at the station.
-- | The result is a collection of all possible inverse paths, kinked at all possible stations.
-- | As the domain of the original query can be either a ContextType, or an EnumeratedRoleType, the range of the
-- | inverted parts can be a ContextType or an EnumeratedRoleType, too.
-- |
-- | FILTERS and LET (WithFrame)
-- | Filters are completely ignored in the code below. On the type level, this does not matter as unfiltered queries
-- | have the same type as filtered versions. Runtime, things will have impact on performance and sometimes on
-- | semantics. See the text Filtering Inverted Queries.docx.
-- | INVARIANT: All QueryWithAKink instances will have a backwards part.
invert :: QueryFunctionDescription -> PhaseThree (Array QueryWithAKink)
invert = invert_ >=> pure <<< catMaybes <<< map h
  where
    h :: QueryWithAKink_ -> Maybe QueryWithAKink
    h (ZQ_ steps q) = case unsnoc steps of
      -- Remove candidates without a backwards part.
      Nothing -> Nothing
      -- Creates a right-associative composition that preserves the order in steps.
      Just {init, last} -> Just $ ZQ (Just $ foldr makeComposition last init) q

-- | The QueryFunctionDescriptions in the Array of each QueryWithAKink_
-- | are inversed wrt the orinal query.
invert_ :: QueryFunctionDescription -> PhaseThree (Array QueryWithAKink_)
-- NOTE moeten we hier niet iets met de args?
-- The inversion depends on the function f.
invert_ (MQD dom (ExternalCoreRoleGetter f) args ran _ _) = case f of 
  "model://perspectives.domains#Couchdb$PendingInvitations" -> pure [ZQ_ [SQD ran (ContextIndividual (ContextInstance mySystem)) dom True True] Nothing]
  _ -> pure $ [ZQ_ [MQD ran (ExternalCoreContextGetter "model://perspectives.domains#Couchdb$ContextInstances") args dom Unknown Unknown] Nothing]
 
invert_ (MQD _ _ args _ _ _) = join <$> traverse invert_ args

invert_ q@(BQD dom (BinaryCombinator ComposeF) l r _ f m) = case l of
  (BQD _ (BinaryCombinator UnionF) conj1 conj2 _ _ _) -> append <$>
    invert_ (makeComposition conj1 (replaceDomain r (range conj1))) <*>
    invert_ (makeComposition conj2 (replaceDomain r (range conj2)))

  (BQD _ (BinaryCombinator IntersectionF) conj1 conj2 _ _ _) -> append <$>
    invert_ (makeComposition conj1 (replaceDomain r (range conj1))) <*>
    invert_ (makeComposition conj2 (replaceDomain r (range conj2)))

  (BQD _ (BinaryCombinator ComposeF) qfd1 qfd2 ran _ _) -> invert_ (BQD dom (BinaryCombinator ComposeF) qfd1 (BQD (range qfd1) (BinaryCombinator ComposeF) qfd2 r ran f m) ran f m)

  (BQD _ (BinaryCombinator FilledByF) conj1 conj2 _ _ _) -> append <$>
    invert_ (makeComposition conj1 (replaceDomain r (range conj1))) <*>
    invert_ (makeComposition conj2 (replaceDomain r (range conj2)))

  (BQD _ (BinaryCombinator SequenceF) qfd1 qfd2 ran _ _) -> do
    q1 <- invert_ qfd1
    q2 <- invert_ qfd2
    pure $ join $ [q1, q2]

  qq@(SQD _ (VariableLookup varName) _ _ _) -> do
    varExpr <- lookupVariableBinding varName
    case varExpr of
      Nothing -> pure []
      Just qfd | qq == qfd -> pure []
      Just qfd -> invert_ (makeComposition qfd r)
  
  otherwise -> do
    (lefts :: Array QueryWithAKink_) <- invert_ l
    (rights :: Array QueryWithAKink_) <- invert_ r
    case lefts, rights of 
      [], _ -> pure rights
      _, [] -> pure lefts
      _, _ -> do 
        comprehension <- pure (comprehend lefts rights)
        -- If the next step is a filter, just return the comprehension. This is because storeInvertedQueries will 
        -- re-create the lefts, but then with a condition.
        -- TODO. I am not sure of the above.
        if hasFilter r
          then pure comprehension
          else append comprehension <$>  for lefts 
            -- Add the original right part of the composition as the forward part of the qinked query.
            \(ZQ_ bw fw) -> case fw of
              Nothing -> pure $ ZQ_ bw (Just r)
              Just fw' -> pure $ ZQ_ bw (Just $ makeComposition fw' r)
  
  where
    comprehend :: Array QueryWithAKink_ -> Array QueryWithAKink_ -> Array QueryWithAKink_
    comprehend lefts rights = do
      (ZQ_ left_inverted_steps mLeft_forward) <- lefts
      (ZQ_ right_inverted_steps mRight_forward) <- rights
      -- The range of mLeft_forward must equal the domain of mRight_forward.
      -- guard $ case range <$> mLeft_forward, domain <$> mRight_forward of
      --   Just ran, Just domn -> ran == domn
      --   -- If the forward of the left part is Nothing, left has been inverted entirely and 
      --   -- may be combined with any inversion of right.
      --   Nothing, _ -> true
      --   _, _ -> false
      guard $ case range <$> (last right_inverted_steps), domain <$> head left_inverted_steps of
        -- We had `equalsOrGeneralizesDomain` instead of `eq`. This runs into two implementation problems:
        --  a. we need `equalsOrGeneralisesRoleInContext`, which is a Monadic function. That seriously complicates the comprehension.
        --  b. we then need that operation on CDOM (ADT ContextType), which we do not yet have and requires a lot of work.
        -- Reconsidering, `eq` is probably right anyway. This is because for consequtive query steps, the range of f1 equals 
        -- the domain of f2 in f1 >> f2. And here we are just juggling around all these combinations.
        Just ranRight, Just domLeft -> ranRight `eq` domLeft
        -- NOTE that as the backwards part cannot be empty, the next case may not occur.
        _, _ -> false
      -- This is where we invert the order of the steps.
      -- That's obvious if both left and right were single steps.
      -- Remember we have right-associativity: s1 >> (s2 >> s3).
      -- So when right has multiple steps, we receive them from the recursive call in reverse order: [s3, s2].
      -- We must then add the left step to the end of those steps: [s3, s2] <> [s1].
      pure $ ZQ_ (right_inverted_steps <> left_inverted_steps)
                  (mLeft_forward `composeOverMaybe` mRight_forward)
    
    hasFilter :: QueryFunctionDescription -> Boolean
    hasFilter qfd = case qfd of 
      (UQD _ FilterF _ _ _ _) -> true
      (BQD _ (BinaryCombinator ComposeF) (UQD _ FilterF _ _ _ _) _ _ _ _) -> true
      _ -> false

-- invert_ (BQD _ (BinaryCombinator FilterF) source criterium _ _ _) = invert_ $ 
--   makeComposition source $
--     makeComposition (UQD (range source) FilterF criterium (range source) True False)
--       criterium

invert_ (BQD _ (BinaryCombinator f) qfd1 qfd2 _ _ _) = append <$> invert_ qfd1 <*> invert_ qfd2

-- We balance VariableLookup, where we invert the expression we look up,
-- simply by storing the compiled expression under the variable name.
invert_ (UQD _ (BindVariable varName) qfd _ _ _) = do
  addBinding varName qfd
  -- As we invert the expression that the variable is bound to
  -- each time we refer it, we do not need invert it here.
  pure []

-- Push a frame. We will encounter BindVariable instances in the first
-- part of the seqence that is the qfd.
invert_ (UQD _ WithFrame qfd _ _ _) = do
  withFrame (invert_ qfd)

-- The inverted criterium is a function with the type T to be filtered as range.
-- Ultimately, we do not want to prepend the filter to the inverted criterium. A case analysis shows why:
-- If the criterum turns true on an instance of T, it would pass it on.
-- If the criterium turns false on an instance of T, a peer needs to know (but the filter would prevent him from receiving a delta).
-- However, it must be prepended to the inversion of the steps to the left of the filter step in the original query.
-- For that reason, we prepend it here but remove it when we store inverted queries.
-- Notice that the filter never ends up in the forward part.
invert_ filter@(UQD _ FilterF criterium _ _ _) = do
  criteriumInversions :: Array QueryWithAKink_ <- invert_ criterium
  pure $ addFilter <$> criteriumInversions
  -- An inversion of criterium is a query whose range is the type of items to be judged. 
  -- We append the filter to such an inverted query (apply the filter to items of the range type!).
  where
    addFilter :: QueryWithAKink_ -> QueryWithAKink_
    addFilter (ZQ_ steps forward) = ZQ_ (snoc steps filter) forward

invert_ (UQD _ _ qfd _ _ _) = invert_ qfd

invert_ (SQD dom (Constant _ _) ran _ _) = pure []

invert_ (SQD dom (RolGetter rt) ran _ _) = case rt of
  ENR _ -> pure [ZQ_ [(SQD ran (DataTypeGetter ContextF) dom True True)] Nothing]
  CR r -> (lift2 $ (getRole >=> getCalculation) rt) >>= invert_

invert_ (SQD dom (PropertyGetter (CP prop)) ran _ _) = (lift2 $ (getCalculatedProperty >=> calculation) prop) >>= invert_

invert_ (SQD dom (DataTypeGetter CountF) ran _ _) = pure []

  -- Treat a variable by looking up its definition (a QueryFunctionDescription), inverting it and inserting it.
invert_ q@(SQD dom (VariableLookup varName) _ _ _) = do
  varExpr <- lookupVariableBinding varName
  case varExpr of
    Nothing -> pure []
    Just qfd | qfd == q -> pure []
    Just qfd -> invert_ qfd

invert_ qfd@(SQD dom@(RDOM roleAdt) f@(PropertyGetter prop@(ENP _)) ran fun man) = do
  (hasProp :: Boolean) <- roleHasProperty roleAdt
  if hasProp
    then do
      minvertedF <- invertFunction dom f ran
      case minvertedF of
        Nothing -> pure []
        Just invertedF -> pure [ZQ_ [(SQD ran invertedF dom True True)] Nothing]
    else (expandPropertyQuery roleAdt) >>= invert_

  where
    -- Creates a series of nested binding expressions until the property has been reached.
    expandPropertyQuery :: ADT RoleInContext -> PhaseThree QueryFunctionDescription
    expandPropertyQuery adt = do
      hasProp <- roleHasProperty adt
      if hasProp
        then pure (SQD (RDOM adt) (PropertyGetter prop) ran fun man)
        else do 
          mbinding <- lift $ lift $ bindingOfADT adt
          case mbinding of 
            Just binding -> do
              bindingHasProp <- roleHasProperty binding
              if bindingHasProp
                then pure $ makeComposition 
                  (SQD (RDOM adt) (DataTypeGetter FillerF) (RDOM binding) True False)
                  (SQD (RDOM binding) (PropertyGetter prop) ran fun man)
                else makeComposition <$> pure (SQD (RDOM adt) (DataTypeGetterWithParameter FillerF "direct") (RDOM binding) True False) <*> (expandPropertyQuery binding)
            -- No fillers, but we haven't found the property yet. This is an error situation, but the compiler has 
            -- ensured it cannot happen.
            Nothing -> throwError (Custom $ "An impossible situation in module Perspectives.Query.Kinked, invert_.expandPropertyQuery. This property cannot be found: " <> show prop)

    roleHasProperty :: ADT RoleInContext -> PhaseThree Boolean
    roleHasProperty adt = lift $ lift (allLocallyRepresentedProperties (roleInContext2Role <$> adt) >>= pure <<< isJust <<< (elemIndex prop))

-- The individual takes us to an instance of the range (`ran`), whatever the domain (`dom`) is. RoleIndividual is a constant function.
-- Its inversion takes us to all instances of the domain.
invert_ (SQD dom (RoleIndividual rid) ran fun man) = case dom of 
  -- Find all context instances whose type is one of the ContextTypes in CDOM adt. 
  -- NOTE: WE ARBITRARILY TAKE THE FIRST SUCH TYPE.
  
  CDOM adt -> pure [ZQ_ [MQD ran (ExternalCoreContextGetter "model://perspectives.domains#Couchdb$ContextInstances") 
    [SQD dom (Constant PString (unwrap $ unsafePartial $ fromJust $ head $ allLeavesInADT adt)) (VDOM PString Nothing) True True]
    dom Unknown Unknown] Nothing]
  RDOM adt -> pure [ZQ_ [MQD ran (ExternalCoreRoleGetter "model://perspectives.domains#Couchdb$RoleInstances") 
    [SQD dom (Constant PString (unwrap $ roleInContext2Role $ unsafePartial $ fromJust $ head $ allLeavesInADT adt)) (VDOM PString Nothing) True True]
    dom Unknown Unknown] Nothing]
  _ -> pure []

-- Get all instances of the type of the domain. 
invert_ (SQD dom (ContextIndividual rid) ran fun man) = case dom of 
  CDOM adt -> pure [ZQ_ [MQD ran (ExternalCoreContextGetter "model://perspectives.domains#Couchdb$ContextInstances") 
    [SQD dom (Constant PString (unwrap $ unsafePartial $ fromJust $ head $ allLeavesInADT adt)) (VDOM PString Nothing) True True]
    dom Unknown Unknown] Nothing]
  RDOM adt -> pure [ZQ_ [MQD ran (ExternalCoreRoleGetter "model://perspectives.domains#Couchdb$RoleInstances") 
    [SQD dom (Constant PString (unwrap $ roleInContext2Role $ unsafePartial $ fromJust $ head $ allLeavesInADT adt)) (VDOM PString Nothing) True True]
    dom Unknown Unknown] Nothing]
  _ -> pure []

invert_ (SQD dom f ran _ _) = do
  (minvertedF :: Maybe QueryFunction) <- invertFunction dom f ran
  case minvertedF of
    Nothing -> pure []
    Just invertedF -> pure [ZQ_ [(SQD ran invertedF dom (queryFunctionIsFunctional invertedF) (queryFunctionIsMandatory f))] Nothing]

-- Catchall.
invert_ q = throwError (Custom $ "Missing case in invert for: " <> prettyPrint q)

