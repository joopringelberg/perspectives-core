module Test.Query.Inversion where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (catMaybes, cons, find, head, intercalate, intersect, last, length, null, union)
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Foreign.Object (lookup, empty)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.InvertedQuery (QueryWithAKink, backwards)
import Perspectives.Parsing.Arc (domain) as ARC
import Perspectives.Parsing.Arc.AST (ContextE(..))
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (evalPhaseTwo', runPhaseTwo', runPhaseTwo_')
import Perspectives.Persistent (getPerspectEntiteit)
import Perspectives.Query.Kinked (invert)
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), domain, queryFunction)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), PropertyType(..))
import Perspectives.TypePersistence.LoadArc.FS (loadCompileAndCacheArcFile', loadCompileAndSaveArcFile')
import Perspectives.Utilities (prettyPrint)
import Test.Perspectives.Utils (runP, withModel_, withSystem)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip, suiteOnly)
import Test.Unit.Assert (assert)
import Parsing (ParseError)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suite "Test.Query.Inversion" do

  -- testOnly "InverseQueries" $ runP $ withSystem do
  testSkip "InverseQueries" $ runP $ do
    _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
    modelErrors <- loadCompileAndSaveArcFile' "invertedQueryTest" "test"
    -- logShow modelErrors
    -- logShow (length modelErrors)
    liftAff $ assert "There should be no model errors" (null modelErrors)
    when (length modelErrors > 0) (logShow modelErrors)


  -- test "Invert a rule condition on a CalculatedProperty" (runP do
  --     modelErrors <- loadCompileAndCacheArcFile' "inversion" testDirectory
  --     if null modelErrors
  --       then do
  --         (Action{condition}) <- getAction (ActionType "model:Test$TestCase5$Self_bot$ChangeARole")
  --         case condition of
  --           S _ _ -> liftAff $ assert "Condition should have been compiled." false
  --           Q qfd -> do
  --             -- log $ prettyPrint qfd
  --             -- the condition is variable-free.
  --             (DomeinFile (dfr :: DomeinFileRecord)) <- getPerspectEntiteit (DomeinFileId "model:Test")
  --             result <- runPhaseTwo_' (invert qfd) dfr empty empty
  --             case fst result of
  --               Left e -> liftAff $ assert ("Cannot invert query: " <> show e) false
  --               Right (affectedContextQueries :: Array QueryWithAKink) -> do
  --                 -- log $ intercalate "\n" (prettyPrint <$> affectedContextQueries)
  --                 -- log $ "\n" <> intercalate "\n" (show <<< paths2functions <$> (catMaybes $ backwards <$> affectedContextQueries))
  --                 liftAff $ assert "There should be two AffectedContextQueries." ((paths2functions <$> (catMaybes $ backwards <$> affectedContextQueries)) ==
  --                   [ [(DataTypeGetter ContextF)]
  --                   , [(Value2Role (ENP $ EnumeratedPropertyType "model:Test$TestCase5$ARole$Prop6")),(DataTypeGetter ContextF)]
  --                   , [(Value2Role (ENP $ EnumeratedPropertyType "model:Test$TestCase5$ARole$Prop7")),(DataTypeGetter ContextF)]
  --                   ])
  --       else liftAff $ assert ("There are model errors: " <> show modelErrors) false
  --       )

  test "Constant" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain Test\n  case TestCase1\n    thing ARole\n      property Prop1 = true" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                (Left e) -> assert (show e) false
                (Right (Tuple correctedDFR@{calculatedProperties} _)) -> do
                  -- logShow correctedDFR
                  case lookup "model:Test$TestCase1$ARole$Prop1" calculatedProperties of
                    Nothing -> assert "The model should have property 'model:Test$TestCase1$ARole$Prop1'." false
                    (Just (CalculatedProperty{calculation})) -> case calculation of
                      S _ _ -> assert "The calculation should have been compiled!" false
                      Q c -> do
                        inv <- invertFD c
                        assert "The inversion of a Constant should be Nothing" (null inv)

  test "Property of another role in the same context (1)" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain Test\n  case TestCase1\n    thing ARole\n      property Prop1 = context >> AnotherRole >> Prop2\n    thing AnotherRole\n      property Prop2 (mandatory, Boolean)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                (Left e) -> assert (show e) false
                (Right (Tuple correctedDFR@{calculatedProperties} _)) -> do
                  -- logShow correctedDFR
                  case lookup "model:Test$TestCase1$ARole$Prop1" calculatedProperties of
                    Nothing -> assert "The model should have property 'model:Test$TestCase1$ARole$Prop1'." false
                    (Just (CalculatedProperty{calculation})) -> case calculation of
                      S _ _ -> assert "The calculation should have been compiled!" false
                      Q c -> do
                        paths <- invertFD c
                        -- log $ intercalate "\n" (prettyPrint <$> paths)
                        assert "The inversion of a a property of another role in the same context should run \
                        \from that property to the role to the context" (not $null paths)
                        assert "The domain of the inversion should be Prop2."
                          (isJust $ find (\path -> case path of
                            (BQD (VDOM PBool (Just (ENP (EnumeratedPropertyType "model:Test$TestCase1$AnotherRole$Prop2")))) _ _ _ _ _ _) -> true
                            otherwise -> false) paths)
                        assert "The first term of the composition should be Value2Role"
                          (isJust $ find (\path -> case path of
                            (BQD _ (BinaryCombinator ComposeF) (SQD _ (Value2Role _) _ _ _) _ _ _ _) -> true
                            otherwise -> false) paths)

  test "Property of another role in the same context (2)" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain Test\n  case TestCase1\n    thing ARole\n      property Prop1 = context >> NestedContext >> binding >> context >> AnotherRole >> Prop2\n    context NestedContext filledBy SubCase\n    case SubCase\n      thing AnotherRole\n        property Prop2 (mandatory, Boolean)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                (Left e) -> assert (show e) false
                (Right (Tuple correctedDFR@{calculatedProperties} _)) -> do
                  -- logShow correctedDFR
                  case lookup "model:Test$TestCase1$ARole$Prop1" calculatedProperties of
                    Nothing -> assert "The model should have property 'model:Test$TestCase1$ARole$Prop1'." false
                    (Just (CalculatedProperty{calculation})) -> case calculation of
                      S _ _ -> assert "The calculation should have been compiled!" false
                      Q c -> do
                        paths <- invertFD c
                        -- log $ intercalate "\n" (prettyPrint <$> paths)
                        assert "The inversion of a a property of another role in the same context should run \
                        \from that property to the role to the context" (not $ null paths)
                        assert "The domain of the inversion should be Prop2."
                          (isJust $ find (\path -> case path of
                            (BQD (VDOM PBool (Just (ENP (EnumeratedPropertyType "model:Test$TestCase1$SubCase$AnotherRole$Prop2")))) _ _ _ _ _ _) -> true
                            otherwise -> false) paths)
                        assert "The first term of the composition should be Value2Role"
                          (isJust $ find (\path -> case path of
                            (BQD _ (BinaryCombinator ComposeF) (SQD _ (Value2Role _) _ _ _) _ _ _ _) -> true
                            otherwise -> false) paths)
                        assert "The third term of the composition should be external"
                          (isJust $ find (\path -> case path of
                            (BQD _ _ _ (BQD _ _ _ (BQD _ _ (SQD _ (DataTypeGetter ExternalRoleF) _ _ _ ) _ _ _ _ ) _ _ _) _ _ _) -> true
                            otherwise -> false) paths)

  test "A filter expression should yield five inverse queries." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-} runIndentParser "domain Test\n  case TestCase1\n    thing ARole\n      property Prop1 = filter context >> AnotherRole with Prop2 >> Prop3\n    thing AnotherRole\n      property Prop2 (mandatory, Boolean)\n      property Prop3 (mandatory, Boolean)\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE{id})) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                (Left e) -> assert (show e) false
                (Right (Tuple correctedDFR@{calculatedProperties} _)) -> do
                  -- logShow correctedDFR
                  case lookup "model:Test$TestCase1$ARole$Prop1" calculatedProperties of
                    Nothing -> assert "The model should have property 'model:Test$TestCase1$ARole$Prop1'." false
                    (Just (CalculatedProperty{calculation})) -> case calculation of
                      S _ _ -> assert "The calculation should have been compiled!" false
                      Q c -> do
                        -- log (prettyPrint c)
                        paths <- invertFD c
                        -- log $ intercalate "\n" (prettyPrint <$> paths)
                        assert "The inversion of an expression with a filter should yield two inverse queries" (length paths == 5)

  -- test "Invert a rule condition" (runP do
  --     modelErrors <- loadCompileAndCacheArcFile' "inversion" testDirectory
  --     if null modelErrors
  --       then do
  --         (Action{condition}) <- getAction (ActionType "model:Test$TestCase1$Self_bot$ChangeARole")
  --         -- logShow condition
  --         case condition of
  --           S _ _ -> liftAff $ assert "Condition should have been compiled." false
  --           Q qfd -> do
  --             affectedContextQueries <- liftAff $ invertFD qfd
  --             -- log $ intercalate "\n" (prettyPrint <$> affectedContextQueries)
  --             -- logShow $ paths2functions <$> affectedContextQueries
  --             liftAff $ assert "There should be four AffectedContextQueries." ((paths2functions <$> affectedContextQueries) ==
  --               [ [(DataTypeGetter ContextF)]
  --               , [Value2Role $ ENP $ EnumeratedPropertyType "model:Test$TestCase1$AnotherRole$Prop2", (DataTypeGetter ContextF)]
  --               , [(DataTypeGetter ContextF)]
  --               , [Value2Role $ ENP $ EnumeratedPropertyType "model:Test$TestCase1$AnotherRole$Prop3",(DataTypeGetter ContextF)]
  --               ])
  --       else liftAff $ assert ("There are model errors: " <> show modelErrors) false
  --       )

  -- test "Invert a rule condition that reaches into a subcontext 1" (runP do
  --     modelErrors <- loadCompileAndCacheArcFile' "inversion" testDirectory
  --     if null modelErrors
  --       then do
  --         (Action{condition}) <- getAction (ActionType "model:Test$TestCase2$Self_bot$ChangeARole")
  --         -- logShow condition
  --         case condition of
  --           S _ _ -> liftAff $ assert "Condition should have been compiled." false
  --           Q qfd -> do
  --             affectedContextQueries <- liftAff $ invertFD qfd
  --             -- log $ intercalate "\n" (prettyPrint <$> affectedContextQueries)
  --             -- log $ "\n" <> intercalate "\n" (show <<< paths2functions <$> affectedContextQueries)
  --             liftAff $ assert "There should be seven AffectedContextQueries." ((paths2functions <$> affectedContextQueries) ==
  --               [ [ (DataTypeGetter ContextF)]
  --               , [ Value2Role $ ENP $ EnumeratedPropertyType "model:Test$TestCase2$ARole$Prop1" , (DataTypeGetter ContextF)]
  --               , [(DataTypeGetter ContextF)]
  --               , [(DataTypeGetterWithParameter GetRoleBindersF "model:Test$TestCase2$ARole"), (DataTypeGetter ContextF)]
  --               , [(DataTypeGetter ExternalRoleF), (DataTypeGetterWithParameter GetRoleBindersF "model:Test$TestCase2$ARole"), (DataTypeGetter ContextF)]
  --               , [(DataTypeGetter ContextF), (DataTypeGetter ExternalRoleF), (DataTypeGetterWithParameter GetRoleBindersF "model:Test$TestCase2$ARole"), (DataTypeGetter ContextF)]
  --               , [(Value2Role $ ENP $ EnumeratedPropertyType "model:Test$TestCase2$NestedCase1$AnotherRole$Prop3"), (DataTypeGetter ContextF), (DataTypeGetter ExternalRoleF), (DataTypeGetterWithParameter GetRoleBindersF "model:Test$TestCase2$ARole"), (DataTypeGetter ContextF)]
  --               ])
  --       else liftAff $ assert ("There are model errors: " <> show modelErrors) false
  --       )

  -- test "Invert a rule condition that reaches into a subcontext 2" (runP do
  --     modelErrors <- loadCompileAndCacheArcFile' "inversion" testDirectory
  --     if null modelErrors
  --       then do
  --         (Action{condition}) <- getAction (ActionType "model:Test$TestCase3$Self_bot$ChangeARole")
  --         -- logShow condition
  --         case condition of
  --           S _ _ -> liftAff $ assert "Condition should have been compiled." false
  --           Q qfd -> do
  --             affectedContextQueries <- liftAff $ invertFD qfd
  --             -- log $ intercalate "\n" (prettyPrint <$> affectedContextQueries)
  --             -- log $ "\n" <> intercalate "\n" (show <<< paths2functions <$> affectedContextQueries)
  --             -- logShow $ length affectedContextQueries
  --             liftAff $ assert "There should be nine AffectedContextQueries." (length affectedContextQueries == 9)
  --       else liftAff $ assert ("There are model errors: " <> show modelErrors) false
  --       )

  -- test "Invert a rule condition with a filter criterium that reaches into a subcontext" (runP do
  --     modelErrors <- loadCompileAndCacheArcFile' "inversion" testDirectory
  --     if null modelErrors
  --       then do
  --         (Action{condition}) <- getAction (ActionType "model:Test$TestCase4$Self_bot$ChangeARole")
  --         -- logShow condition
  --         case condition of
  --           S _ _ -> liftAff $ assert "Condition should have been compiled." false
  --           Q qfd -> do
  --             affectedContextQueries <- liftAff $ invertFD qfd
  --             -- log $ intercalate "\n" (prettyPrint <$> affectedContextQueries)
  --             -- log $ "\n" <> intercalate "\n" (show <<< paths2functions <$> affectedContextQueries)
  --             -- log $ intercalate "\n" (map (show <<< domain) affectedContextQueries)
  --             liftAff $ assert "There should be twelve AffectedContextQueries." (length affectedContextQueries == 12)
  --             liftAff $ assert "The three AffectedContextQueries should start in the properties Prop3, Prop5 and Prop2"
  --               (eq 3
  --                 (length $ (map domain affectedContextQueries) `intersect`
  --                 [ (VDOM PBool (Just $ ENP $ EnumeratedPropertyType "model:Test$TestCase4$NestedCase3$AnotherRole$Prop3"))
  --                 , (VDOM PBool (Just $ ENP $ EnumeratedPropertyType "model:Test$TestCase4$NestedCase3$YetAnotherRole$Prop5"))
  --                 , (VDOM PBool (Just $ ENP $ EnumeratedPropertyType "model:Test$TestCase4$NestedCase3$AnotherRole$Prop2"))]))
  --       else liftAff $ assert ("There are model errors: " <> show modelErrors) false
  --       )

composition2path :: QueryFunctionDescription -> Array QueryFunctionDescription
composition2path (BQD _ (BinaryCombinator ComposeF) left right _ _ _) = cons left (composition2path right)
composition2path qfd = [qfd]

paths2functions :: QueryFunctionDescription -> Array QueryFunction
paths2functions = (map queryFunction <<< composition2path)

invertFD :: QueryFunctionDescription -> Aff (Array QueryFunctionDescription)
invertFD qfd = do
  result <- runP $ evalPhaseTwo' (invert qfd)
  case result of
    Left e -> assert ("Cannot invert query: " <> show e) false *> pure []
    Right qfds -> pure $ catMaybes (backwards <$> qfds)
