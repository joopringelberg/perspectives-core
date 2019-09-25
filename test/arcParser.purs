module Test.Parsing.Arc where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Lens (Traversal', preview, traversed)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust)
import Data.Symbol (SProxy(..))
import Effect.Class.Console (logShow)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.Parsing.Arc (domain, parseAndCache)
import Perspectives.Parsing.Arc.IndentParser (parseToDomeinFile)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..))
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert as Assert

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Parsing.Arc" do
  testSkip "Representing the Domain" do
    -- parse to a DomeinFile.
    (domeinFile :: DomeinFile) <- runP $ parseToDomeinFile "Context : Domain : MyTestDomain\n" domain
    -- The first context should represent the Domein.
    (dom :: Maybe Context) <- pure $ preview (_context "model:MyTestDomain") domeinFile
    Assert.assert "The DomeinFile should have a context for the Domain." (isJust dom)
    Assert.assert "The context that represents the Domain should have id 'model:MyTestDomain'"
      case dom of
        Nothing -> false
        (Just (Context {_id})) -> _id == ContextType "model:MyTestDomain"

  test "Representing a Role in the Domain" do
    r <- runP $ parseAndCache "Context : Domain : MyTestDomain\n  Role:Role:MyRole\n    Functional : False\n    Mandatory : True\n"
    case r of
      (Left p) -> Assert.assert (show p) false
      (Right domeinFile) -> do
        (role :: Maybe EnumeratedRole) <- pure $ preview (_enumeratedRoles "model:MyTestDomain$MyRole") domeinFile
        logShow domeinFile
        Assert.assert "The DomeinFile should have an EnumeratedRole 'model:MyTestDomain$MyRole'." (isJust role)
        -- Assert.assert "The context that represents the Domain should have id 'model:MyTestDomain'"
        --   case dom of
        --     Nothing -> false
        --     (Just (Context {_id})) -> _id == ContextType "model:MyTestDomain"

  where
    _context :: String -> Traversal' DomeinFile Context
    _context s = _Newtype <<< (prop (SProxy :: SProxy "contexts")) <<< (at s) <<< traversed

    _enumeratedRoles :: String -> Traversal' DomeinFile EnumeratedRole
    _enumeratedRoles s = _Newtype <<< (prop (SProxy :: SProxy "enumeratedRoles")) <<< (at s) <<< traversed
