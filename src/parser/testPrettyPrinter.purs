module Test.PrettyPrinter where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (empty, lookup)
import Perspectives.ContextRoleParser (context) as CRP
import Perspectives.IndentParser (runIndentParser)
import Perspectives.PrettyPrinter (prettyPrint, prettyPrintContext)
import Perspectives.Syntax2 (EntityCollection(..), NamedEntityCollection(..), PerspectEntity(..))
import Prelude (($))

test1 :: String
test1 = prettyPrint $ EntityCollection empty

test2 :: String
test2 = case runIndentParser ":Aangifte :A1" CRP.context of
  (Right (NamedEntityCollection ident (EntityCollection j))) -> case lookup ident j of
    Nothing -> "onbekende fout"
    (Just (Context c)) -> prettyPrintContext c
    (Just (Rol _)) -> "er is geen rol"
  otherwise -> "fout in expressie"
