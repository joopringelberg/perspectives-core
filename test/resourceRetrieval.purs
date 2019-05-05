module Test.Perspectives.ResourceRetrieval (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Trans.Class (lift)
import Data.Array (singleton)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (fromFoldable)
import Perspectives.BasicConstructors (constructContext')
import Perspectives.CoreTypes (MonadPerspectives, MP)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.ResourceRetrieval (fetchEntiteit, removeEntiteit, saveEntiteit, saveUnversionedEntiteit, saveVersionedEntiteit)
import Perspectives.Syntax (Comments(..), PerspectContext(..))
import Test.Perspectives.Utils (addTestContext, assertEqual, string2ContextSerialization, u)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

t1 :: String
t1 = """{ "id": "u:myContext"
  , "prototype": "psp:ContextPrototype"
  , "ctype": "psp:Context"
  , "rollen": { "psp:Context$rolInContext":  [ { "properties": {}, "binding": "psp:Context_buitenRol" }]}
  , "interneProperties": {}
  , "externeProperties": {}
  }"""

theSuite :: Free TestF Unit
theSuite = suite "ResourceRetrieval" do

  testSkip "addTestContext" do
    assertEqual "It should be possible to add a test context."
      (lift $ addTestContext t1)
      unit

  test "fetchEntiteit" do
    assertEqual "It should be possible to retrieve usr:MijnSysteem from the database."
      (((fetchEntiteit $ u "MijnSysteem") >>= (pure <<< singleton)) :: MonadPerspectives (Array PerspectContext))
      [(PerspectContext { _id: "model:User$MijnSysteem", _rev: (Just "113-517f876dc7bd939bec8f3387154c9f06"), binnenRol: "model:User$MijnSysteem_binnenRol", buitenRol: "model:User$MijnSysteem_buitenRol", comments: (Comments { commentAfter: [], commentBefore: [" NB: Deze file wordt nooit van deze lokatie geladen. Wel b.v. in het project perspectives-react-integrated-client"] }), displayName: "MijnSysteem", pspType: "model:Perspectives$PerspectivesSysteem", rolInContext: (fromFoldable [(Tuple "model:Perspectives$PerspectivesSysteem$trustedCluster" ["model:User$MijnSysteem$trustedCluster_1"]),(Tuple "model:Perspectives$PerspectivesSysteem$gebruiker" ["model:User$MijnSysteem$gebruiker_1"])]) })]

  test "saveVersionedEntiteit" do
    assertEqual "It should be possible to save an versioned, locally cached resource."
      do
        (ms :: PerspectContext) <- getPerspectEntiteit (u "MijnSysteem")
        void $ (saveEntiteit (u "MijnSysteem") :: MP PerspectContext)
      unit

  test "saveUnversionedEntiteit" do
    assertEqual "It should be possible to save an unversioned, locally cached resource."
      do
        (c :: String) <- (string2ContextSerialization >=> constructContext') t1
        (void $ (saveUnversionedEntiteit c :: MonadPerspectives PerspectContext))
      unit

  test "saveEntiteit" do
    assertEqual "It should be possible to save a versioned, locally cached resource."
      do
        (ms :: PerspectContext) <- getPerspectEntiteit (u "MijnSysteem")
        void $ (saveVersionedEntiteit (u "MijnSysteem") ms :: MP PerspectContext)
      unit

  -- testOnly "" do
  --   loadTestModel "TestOGC.crl"
  --
  --   unLoadTestModel "model:TestOGC"

  test "removeEntiteit" do
    assertEqual "It should be possible to remove to remove an entiteit."
      do
        (ms :: PerspectContext) <- getPerspectEntiteit (u "myContext")
        void $ removeEntiteit (u "myContext") ms
      unit
