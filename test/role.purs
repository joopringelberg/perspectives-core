module Test.Class.Role where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (concat, null)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.ModelDependencies (perspectivesUsers, socialEnvironmentPersons, sysUser)
import Perspectives.Query.QueryTypes (RoleInContext(..))
import Perspectives.Representation.ADT (ADT(..), equalsOrSpecialises, equalsOrSpecialises_, specialises)
import Perspectives.Representation.CNF (CNF, DPROD(..), DSUM(..), toConjunctiveNormalForm)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.Class.Role (allRoleProperties, completeDeclaredType, completeExpandedFillerRestriction, completeExpandedType, declaredAspects, declaredType, roleADTOfRoleType, typeToRole)
import Perspectives.Representation.ExpandedADT (ExpandedADT(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Perspectives.TypePersistence.LoadArc.FS (loadCompileAndCacheArcFile')
import Perspectives.Types.ObjectGetters (equalsOrSpecialisesRoleInContext, generalisesRoleType_)
import Perspectives.Utilities (class PrettyPrint, prettyPrint)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, test, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

-- Edit persistenceAPI.js first by declaring Pouchdb like this:
-- var PouchDB = require('pouchdb');

loadModels :: String -> Array String ->  MonadPerspectives Unit
loadModels dir models = do
  addAllExternalFunctions
  errs <- concat <$> for models
    (flip loadCompileAndCacheArcFile' dir)
  if null errs
    then log "All models loaded\n\n"
    else liftAff $ assert 
      ("There are instance- or model errors: " <> show errs) false

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Representation.Class.Role" do

  testSkip "allProperties" $ runP do
    loadModels "test" ["roleTest"]
    getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test1$Role1") >>= pure <<< declaredType >>= 
      \dnf -> liftAff $ assert "declaredType" (eq dnf $
        (UET (RoleInContext { context: ContextType "model://joopringelberg.nl#TestModel$Test1", role: EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test1$Role1" })))
    getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test1$Role1") >>= declaredAspects >>=
      \dnf -> liftAff $ assert "declaredAspects" (eq dnf $
        (PROD [
        (ST (RoleInContext { context: ContextType "model://joopringelberg.nl#TestModel", role: EnumeratedRoleType "model://joopringelberg.nl#TestModel$Aspect1" }))
        ]))
    getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test1$Role1") >>= completeDeclaredType >>=
      \dnf -> liftAff $ assert "completeDeclaredType" (eq dnf $
        (PROD [
          (UET (RoleInContext { context: ContextType "model://joopringelberg.nl#TestModel$Test1", role: EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test1$Role1" })),
          (SUM [
            (UET (RoleInContext { context: ContextType "model://joopringelberg.nl#TestModel", role: EnumeratedRoleType "model://joopringelberg.nl#TestModel$Filler1" }))
            ]),
          (PROD [
            (ST (RoleInContext { context: ContextType "model://joopringelberg.nl#TestModel", role: EnumeratedRoleType "model://joopringelberg.nl#TestModel$Aspect1" }))
            ])
          ]))
    getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test1$Role1") >>= completeExpandedType >>=
      \dnf -> liftAff $ assert "completeExpandedType" (eq dnf $
         (EPROD [
          (ECT (EST (RoleInContext { context: ContextType "model://joopringelberg.nl#TestModel$Test1", role: EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test1$Role1" }))
            (EPROD [
              (EST (RoleInContext { context: ContextType "model://joopringelberg.nl#TestModel$Test1", role: EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test1$Role1" })),
              (ESUM [
                (ECT (EST (RoleInContext { context: ContextType "model://joopringelberg.nl#TestModel", role: EnumeratedRoleType "model://joopringelberg.nl#TestModel$Filler1" }))
                  (EPROD [
                    (EST (RoleInContext { context: ContextType "model://joopringelberg.nl#TestModel", role: EnumeratedRoleType "model://joopringelberg.nl#TestModel$Filler1" })),
                    (ESUM [
                      (EST (RoleInContext { context: ContextType "model://joopringelberg.nl#TestModel", role: EnumeratedRoleType "model://joopringelberg.nl#TestModel$Filler2" }))
                      ])
                    ]))
                ]),
              (EST (RoleInContext { context: ContextType "model://joopringelberg.nl#TestModel", role: EnumeratedRoleType "model://joopringelberg.nl#TestModel$Aspect1" }))
              ])),
          (ESUM [
            (ECT (EST (RoleInContext { context: ContextType "model://joopringelberg.nl#TestModel", role: EnumeratedRoleType "model://joopringelberg.nl#TestModel$Filler1" }))
              (EPROD [
                (EST (RoleInContext { context: ContextType "model://joopringelberg.nl#TestModel", role: EnumeratedRoleType "model://joopringelberg.nl#TestModel$Filler1" })),
                (ESUM [
                  (EST (RoleInContext { context: ContextType "model://joopringelberg.nl#TestModel", role: EnumeratedRoleType "model://joopringelberg.nl#TestModel$Filler2" }))
                  ])
                ]))
            ]),
          (EPROD [
            (EST (RoleInContext { context: ContextType "model://joopringelberg.nl#TestModel", role: EnumeratedRoleType "model://joopringelberg.nl#TestModel$Aspect1" }))
            ])
          ])
        )
    getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test1$Role1") >>= completeExpandedType >>= pure <<< toConjunctiveNormalForm >>=
      \dnf -> liftAff $ assert "completeExpandedType in DNF" (eq dnf
        (DPROD [
          DSUM [
            ( (RoleInContext { context: ContextType "model://joopringelberg.nl#TestModel$Test1", role: EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test1$Role1" }))
            ],
          DSUM [
            ( (RoleInContext { context: ContextType "model://joopringelberg.nl#TestModel", role: EnumeratedRoleType "model://joopringelberg.nl#TestModel$Filler1" }))
            ],
          DSUM [
            ( (RoleInContext { context: ContextType "model://joopringelberg.nl#TestModel", role: EnumeratedRoleType "model://joopringelberg.nl#TestModel$Filler2" }))
            ],
          DSUM [
            ( (RoleInContext { context: ContextType "model://joopringelberg.nl#TestModel", role: EnumeratedRoleType "model://joopringelberg.nl#TestModel$Aspect1" }))
            ]
          ]))
    -- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test1$Role1") >>= completeExpandedType >>= pure <<< toConjunctiveNormalForm >>= showDNF "expanded type"
    getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test1$Role1") >>= allRoleProperties >>= 
      -- \a -> log ("allRoleProperties " <> show a)
      \a -> liftAff $ assert "allRoleProperties" (eq a
        [ ENP (EnumeratedPropertyType "model://joopringelberg.nl#TestModel$Test1$Role1$Prop1")
        , ENP (EnumeratedPropertyType "model://joopringelberg.nl#TestModel$Filler1$Filler1Prop1")
        , ENP (EnumeratedPropertyType "model://joopringelberg.nl#TestModel$Filler2$Prop1")
        , ENP (EnumeratedPropertyType "model://joopringelberg.nl#TestModel$Aspect1$Prop1")]
        )
    -- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Aspect1") >>= completeExpandedType >>= pure <<< toConjunctiveNormalForm >>= showDNF "aspect1DNF"
    -- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test1$Role1") >>= completeExpandedType >>= pure <<< toConjunctiveNormalForm >>= showDNF "role1DNF"

    do 
      aspect1DNF <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Aspect1") >>= completeExpandedType >>= pure <<< toConjunctiveNormalForm
      role1DNF <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test1$Role1") >>= completeExpandedType >>= pure <<< toConjunctiveNormalForm
      -- showDNF "Aspect1" aspect1DNF
      -- showDNF "Role1" role1DNF
      liftAff $ assert "aspect1 equalsOrSpecialises_ role1" (role1DNF `equalsOrSpecialises_` aspect1DNF)

    do 
      aspect1ExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Aspect1") >>= completeExpandedType
      role1ExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test1$Role1") >>= completeExpandedType
      liftAff $ assert "aspect1 equalsOrSpecialises role11" (role1ExpandedADT `equalsOrSpecialises` aspect1ExpandedADT)

    do 
      role1ExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test1$Role1") >>= completeExpandedType
      liftAff $ assert "role1 equalsOrSpecialises role1" (role1ExpandedADT `equalsOrSpecialises` role1ExpandedADT)

    do 
      aspect1ExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Aspect1") >>= completeExpandedType
      role1ExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test1$Role1") >>= completeExpandedType
      liftAff $ assert "aspect1 specialises role1" (role1ExpandedADT `specialises` aspect1ExpandedADT)


    ((ENR $ EnumeratedRoleType "model://joopringelberg.nl#TestModel$Aspect1") `generalisesRoleType_` (ENR $ EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test1$Role1")
      ) >>= 
      \a -> liftAff $ assert "Aspect1 generalisesRoleType_ Role1" a
    
    ((ENR $ EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test1$Role1") `generalisesRoleType_` (ENR $ EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test1$Role1") 
      ) >>= 
      \a -> liftAff $ assert "not (Role1 generalisesRoleType_ Role1)" (not a)

    do 
      aspect1Declared <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Aspect1") >>= pure <<< declaredType
      role1Declared <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test1$Role1") >>= pure <<< declaredType
      (role1Declared `equalsOrSpecialisesRoleInContext` aspect1Declared) >>= liftAff <<< assert "aspect1 equalsOrSpecialisesRoleInContext role1" 

    do
      filler2ExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Filler2") >>= completeExpandedType
      mrole1RestrictionExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test1$Role1") >>= completeExpandedFillerRestriction
      case mrole1RestrictionExpandedADT of 
        Nothing -> liftAff $ assert "Test1Role1 should have a filledBy clause!" false
        Just role1RestrictionExpandedADT -> do
          showExpandedADT "Filler1 expanded" role1RestrictionExpandedADT
          showDNF "Filler1 DNF" (toConjunctiveNormalForm role1RestrictionExpandedADT)
          liftAff $ assert "Filler2 cannot fill Test1$Role1" (not (filler2ExpandedADT `equalsOrSpecialises` role1RestrictionExpandedADT))
      
    do
      filler2ExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Filler2") >>= completeExpandedType
      mrole1RestrictionExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test2$Role1") >>= completeExpandedFillerRestriction
      case mrole1RestrictionExpandedADT of 
        Nothing -> liftAff $ assert "Test2Role1 should have a filledBy clause!" false
        Just role1RestrictionExpandedADT -> liftAff $ assert "Filler2 may fill Test2$Role1" (filler2ExpandedADT `equalsOrSpecialises` role1RestrictionExpandedADT)
      
    do
      filler1ExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Filler1") >>= completeExpandedType
      mrole1RestrictionExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test2$Role1") >>= completeExpandedFillerRestriction
      case mrole1RestrictionExpandedADT of 
        Nothing -> liftAff $ assert "Test2Role1 should have a filledBy clause!" false
        Just role1RestrictionExpandedADT -> liftAff $ assert "Filler1 may fill Test2$Role1" (filler1ExpandedADT `equalsOrSpecialises` role1RestrictionExpandedADT)

    do
      aspect1ExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Aspect1") >>= completeExpandedType
      mrole1RestrictionExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test2$Role1") >>= completeExpandedFillerRestriction
      case mrole1RestrictionExpandedADT of 
        Nothing -> liftAff $ assert "Test2Role1 should have a filledBy clause!" false
        Just role1RestrictionExpandedADT -> liftAff $ assert "Aspect1 may not fill Test2$Role1" (not (role1RestrictionExpandedADT `equalsOrSpecialises` aspect1ExpandedADT))

    do
      role1ExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test1$Role1") >>= completeExpandedType
      mrole2RestrictionExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test2$Role2") >>= completeExpandedFillerRestriction
      case mrole2RestrictionExpandedADT of 
        Nothing -> liftAff $ assert "Test2Role1 should have a filledBy clause!" false
        Just role2RestrictionExpandedADT -> liftAff $ assert "Test1$Role may fill Test2$Role2" (role1ExpandedADT `equalsOrSpecialises` role2RestrictionExpandedADT)

    do
      test3ExternalExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test3$External") >>= completeExpandedType
      mtest4RootsRestrictionExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test4$Roots") >>= completeExpandedFillerRestriction
      case mtest4RootsRestrictionExpandedADT of 
        Nothing -> liftAff $ assert "Test2Role1 should have a filledBy clause!" false
        Just test4RootsRestrictionExpandedADT -> liftAff $ assert "Test$External may fill Test4$Roots" (test3ExternalExpandedADT `equalsOrSpecialises` test4RootsRestrictionExpandedADT)

    do
      filler3ExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Filler3") >>= completeExpandedType
      mtest5Role1RestrictionExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test5$Role1") >>= completeExpandedFillerRestriction
      case mtest5Role1RestrictionExpandedADT of 
        Nothing -> liftAff $ assert "Test5Role1 should have a filledBy clause!" false
        Just test5Role1RestrictionExpandedADT -> do 
          showDNF "Test5$Role1 restriction" (toConjunctiveNormalForm test5Role1RestrictionExpandedADT)
          log "equalsOrSpecialises"
          showDNF "filler3" (toConjunctiveNormalForm filler3ExpandedADT)
          liftAff $ assert "Filler3 may fill Test5$Role1" (filler3ExpandedADT `equalsOrSpecialises` test5Role1RestrictionExpandedADT)

    do
      filler1ExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Filler1") >>= completeExpandedType
      showDNF "filler1" (toConjunctiveNormalForm filler1ExpandedADT)
      mtest5Role1RestrictionExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test5$Role1") >>= completeExpandedFillerRestriction
      case mtest5Role1RestrictionExpandedADT of 
        Nothing -> liftAff $ assert "Test5Role1 should have a filledBy clause!" false
        Just test5Role1RestrictionExpandedADT -> do 
          showDNF "Test5$Role1 restriction" (toConjunctiveNormalForm test5Role1RestrictionExpandedADT)
          liftAff $ assert "Filler1 may fill Test5$Role1" (filler1ExpandedADT `equalsOrSpecialises` test5Role1RestrictionExpandedADT)

  testSkip "bindingOfADT" $ runP do
    loadModels "src/model" 
      [ "couchdb"
      , "serialise"
      , "sensor"
      , "utilities"
      , "perspectivesSysteem"
      ] 

    -- void $ bindingOfADT (PROD 
    --   [ (ST $ RoleInContext{role: EnumeratedRoleType sysUser, context: ContextType theSystem})
    --   , (ST $ RoleInContext{role: EnumeratedRoleType socialEnvironmentMe, context: ContextType socialEnvironment})])
    --     >>= (traverse (showADT ""))
    
    generalisesRoleType_ 
      (ENR (EnumeratedRoleType socialEnvironmentPersons))
      (ENR (EnumeratedRoleType sysUser))
       >>=
        (liftAff <<< assert "Me generalisesRoleType Persons")
  
    generalisesRoleType_ 
      (ENR (EnumeratedRoleType sysUser))
      (ENR (EnumeratedRoleType socialEnvironmentPersons)) 
      >>=
        (liftAff <<< assert "not (Persons generalisesRoleType Me)") <<< not

    generalisesRoleType_ 
      (ENR (EnumeratedRoleType sysUser))
      (ENR (EnumeratedRoleType sysUser))
      >>=
        (liftAff <<< assert "not (Me generalisesRoleType Me)") <<< not

  testSkip "Aspect filling" $ runP do
    loadModels "test" ["roleTest"]

    do
      mtest6Role1_RestrictionExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test6$Role1") >>= completeExpandedFillerRestriction
      mfiller1_RestrictionExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Filler1") >>= completeExpandedFillerRestriction
      case mtest6Role1_RestrictionExpandedADT, mfiller1_RestrictionExpandedADT of 
        Just test6Role1_RestrictionExpandedADT, Just filler1_RestrictionExpandedADT -> liftAff $ assert "fill restriction Filler1 `equalsOrSpecialises` fill restriction Test6$Role1" (filler1_RestrictionExpandedADT `equalsOrSpecialises` test6Role1_RestrictionExpandedADT)
        _, _ -> liftAff $ assert "Test6Role1 and Filler2 should each have a filledBy clause!" false

    do
      mtest6Role2_RestrictionExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Test6$Role2") >>= completeExpandedFillerRestriction
      mfiller1_RestrictionExpandedADT <- getEnumeratedRole (EnumeratedRoleType "model://joopringelberg.nl#TestModel$Filler1") >>= completeExpandedFillerRestriction
      case mtest6Role2_RestrictionExpandedADT, mfiller1_RestrictionExpandedADT of 
        Just test6Role2_RestrictionExpandedADT, Just filler1_RestrictionExpandedADT -> liftAff $ assert "not (fill restriction Filler1 `equalsOrSpecialises` fill restriction Test6$Role2)" (not (filler1_RestrictionExpandedADT `equalsOrSpecialises` test6Role2_RestrictionExpandedADT))
        _, _ -> liftAff $ assert "Test6Role1 and Filler2 should each have a filledBy clause!" false

  testSkip "all models" $ runP do
    loadModels "src/model" 
      [ "couchdb"
      , "serialise"
      , "sensor"
      , "utilities"
      , "perspectivesSysteem"
      , "BodiesWithAccounts"
      , "parsing"
      , "files"
      , "rabbitMQ"
      , "BrokerServices"
      , "couchdbManagement_new"
      , "disconnect"
      , "introduction"
      , "simpleChat"
      ] 
  testSkip "DNF" $ runP do
    loadModels "src/model" 
      [ "couchdb"
      , "serialise"
      , "sensor"
      , "utilities"
      , "perspectivesSysteem"
      ] 
    
    do
      getEnumeratedRole (EnumeratedRoleType sysUser) >>= pure <<< declaredType >>= showADT "declaredType User"
      getEnumeratedRole (EnumeratedRoleType sysUser) >>= completeExpandedType >>= showExpandedADT "completeExpandedType User"
      getEnumeratedRole (EnumeratedRoleType sysUser) >>= completeExpandedType >>= pure <<< toConjunctiveNormalForm >>= showDNF "DNF User"

      getEnumeratedRole (EnumeratedRoleType perspectivesUsers) >>= pure <<< declaredType >>= showADT "declaredType perspectivesUsers"
      getEnumeratedRole (EnumeratedRoleType perspectivesUsers) >>= completeExpandedType >>= showExpandedADT "completeExpandedType perspectivesUsers"
      getEnumeratedRole (EnumeratedRoleType perspectivesUsers) >>= completeExpandedType >>= pure <<< toConjunctiveNormalForm >>= showDNF "DNF perspectivesUsers"

      getEnumeratedRole (EnumeratedRoleType socialEnvironmentPersons) >>= pure <<< declaredType >>= showADT "declaredType Persons"
      getEnumeratedRole (EnumeratedRoleType socialEnvironmentPersons) >>= completeExpandedType >>= showExpandedADT "completeExpandedType Persons"
      getEnumeratedRole (EnumeratedRoleType socialEnvironmentPersons) >>= completeExpandedType >>= pure <<< toConjunctiveNormalForm >>= showDNF "DNF Persons"

  test "Type comparison" $ runP do
    loadModels "src/model" 
      [ "couchdb"
      , "serialise"
      , "sensor"
      , "utilities"
      , "perspectivesSysteem"
      , "rabbitMQ"
      , "BrokerServices"
      ] 
    
    inviter <- pure (ENR (EnumeratedRoleType "model://perspectives.domains#System$Invitation$Inviter"))
    administrator <- pure (ENR (EnumeratedRoleType "model://perspectives.domains#BrokerServices$BrokerContract$Administrator"))

    -- roleADTOfRoleType inviter >>= showADT "roleADTOfRoleType inviter = "

    -- roleADTOfRoleType administrator >>= showADT "roleADTOfRoleType administrator = " 

    generalisesRoleType_ inviter administrator >>=
        (liftAff <<< assert "Invitation$Inviter generalisesRoleType BrokerContract$Administrator")
    


showADT :: forall a. Show a => String -> ADT a -> MonadPerspectives Unit
showADT m adt = do 
  log (m <> ": ")
  log $ prettyPrint adt
  log "\n"

showExpandedADT :: forall a. Show a => String -> ExpandedADT a -> MonadPerspectives Unit
showExpandedADT m adt = do 
  log (m <> ": ")
  log $ prettyPrint adt
  log "\n"

showDNF :: forall a. Show a => PrettyPrint a => String -> CNF a -> MonadPerspectives Unit
showDNF m dnf = do 
  log (m <> ": ")
  log $ prettyPrint dnf
  log "\n"

