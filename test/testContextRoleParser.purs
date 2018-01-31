module Test.ContextRoleParser where

import Prelude
import Control.Monad.Aff (Aff, runAff_)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe)
import Perspectives.Resource (PROPDEFS, getPerspectEntiteit)
import Perspectives.ResourceTypes (DomeinFileEffects)
import Perspectives.Syntax (PerspectContext)
-----------------------------------------------------------
-- Tests
-----------------------------------------------------------

-- runAff_ (\_->pure unit) ((runIndentParser test1 context) >>= (\r -> log (show r)))

runTest :: forall e a. Show a => Aff (console :: CONSOLE | e) a -> Eff (console :: CONSOLE | e) Unit
runTest t =
  runAff_ (\_->pure unit) (t >>= (\r -> log (show r)))

getContextDef :: forall e. String -> Aff (DomeinFileEffects (prd :: PROPDEFS | e)) (Maybe PerspectContext)
getContextDef id = do
  getPerspectEntiteit id

-- runAndShowContext :: forall e. String -> Eff (DomeinFileEffects (prd :: PROPDEFS, console :: CONSOLE | e)) Unit
-- runAndShowContext text = runTest ((runIndentParser text context) >>= (\x -> getContextDef (unsafePartial (fromRight x))) >>= (\r -> log (show r)))
-- x = runTest (runIndentParser test1 context)
-- x = runTest ((runIndentParser test22 context) >>= (\x -> getContextDef (unsafePartial (fromRight x))) >>= (\r -> log (show r)))

test1 :: String
test1 = """:Aangifte :Aangifte1
"""
-- runIndentParser test1 contextDeclaration

test2 :: String
test2 = """:Aangifte :Aangifte1 -- Commentaar bij :Aangifte1
  public :status = "voltooid\n" -- Commentaar bij status
"""
-- runIndentParser test2 context

-- runIndentParser "public :propname = 1" publicContextPropertyAssignment
-- runIndentParser "private :propname = 1" publicContextPropertyAssignment
-- runIndentParser "private :propname = 1" privateContextPropertyAssignment
-- runIndentParser "public :status = \"voltooid\"" publicContextPropertyAssignment

-- import Text.Parsing.Parser.String
-- runIndentParser "  public status = \"voltooid\"" (withPos (whiteSpace *> indented *> publicContextPropertyAssignment))

test3 :: String
test3 = """$Aangifte $Aangifte1
  $aangever (0) => $Jansen\n"""
-- runIndentParser test3 context

test3a :: String
test3a = """:Aangifte :Aangifte1
  :aangever (0) => :Jansen\n
  :aangever (1) => :Pietersen\n"""

-- runIndentParser test3 context
-- runIndentParser ":aangever => Jansen" (roleBinding "")
-- runIndentParser ":aangever => Jansen\n  prop = 1" (roleBinding "")

test4 :: String
test4 = """:Aangifte :Aangifte1 -- Commentaar bij :Aangifte1
  public :status = "voltooid" -- Commentaar bij :status
  :aangever => :Jansen -- commentaar bij :aangever"""  -- zodra een newline volgt komt de binding niet mee, geindenteerd
-- runIndentParser test4 context

test5 :: String
test5 = """--Commentaar voor :Aangifte1
:Aangifte :Aangifte1 --Commentaar achter :Aangifte1
  public :status = "voltooid"
  private :aantekening = "bla die bla"
  :aangever => :Jansen"""
-- runIndentParser test5 context

test6 :: String
test6 = """:Aangifte :Aangifte1
  public :status = "voltooid"
  private :aantekening = "bla die bla"
  -- Commentaar voor :aangever
  :aangever => :Jansen -- Commentaar bij :aangever
    :betrouwbaarheid = 6 -- Commentaar bij :betrouwbaarheid"""

-- runIndentParser test6 context

test6a :: String
test6a = """-- Commentaar voor :aangever
:aangever => :Jansen -- Commentaar bij :aangever
  :betrouwbaarheid = 6 -- Commentaar bij :betrouwbaarheid
"""
-- runIndentParser test6a (roleBinding ":Aangifte1")

test6b :: String
test6b = """private :aantekening = "bla die bla"
-- Commentaar voor :aangever
"""
-- runIndentParser test6b privateContextPropertyAssignment
-- Het commentaar wordt geparseerd en genegeerd!

test7 :: String
test7 = """:aangever => :Jansen
    :betrouwbaarheid = 6"""
-- runIndentParser test7 (roleBinding "")

test8 :: String
test8 = """:aangever => :Jansen
    :betrouwbaarheid = 6
"""
-- runIndentParser test8 (roleBinding "")

test9 :: String
test9 = """:ContextType :Aangifte -- Commentaar op de regel.
    public :isFunctioneel = true --Commentaar 1
    -- commentaar boven :isVerplicht
    public :isVerplicht = true --Commentaar 1
"""
-- runIndentParser test9 context

test10 :: String
test10 = """:ContextType :Aangifte
	-- Commentaar voor rolProperty
  :rolProperty =>
		:Property :Betrouwbaarheid
			public :isFunctioneel = true
"""
-- runIndentParser test10 context

test10a :: String
test10a = """-- Commentaar voor rolProperty
:rolProperty =>
	:Property :Betrouwbaarheid
		public :isFunctioneel = true
"""
-- runIndentParser test10a (roleBinding ":Aangifte")

test10b :: String
test10b = """:rolProperty =>
	:Property :Betrouwbaarheid
		public :isFunctioneel = true
"""
-- runIndentParser test10b (roleBinding ":Aangifte")

test10c :: String
test10c = """:ContextType :Aangifte
	:rolProperty =>
		:Property :Betrouwbaarheid
			public :isFunctioneel = true
"""
-- runIndentParser test10c context

test11 :: String
test11 = """:Aangifte :A
	--Commentaar voor de properties
	public :urgentie = 1 -- Commentaar achter de property
  """

test12 :: String
test12 = """Text :Mysource
:Aangifte :A1
  :aangever => :Jansen
:Aangifte :A2"""

test13 :: String
test13 = """:Aangifte :A
	:aangever =>
		:RolDef :R"""

test14 :: String
test14 = """:Aangifte :A
	:aangever =>
		:RolDef :Pietersen
		:prop = 1"""

test15 :: String
test15 = """:Aangifte :A
	:aangever =>
		:RolDef :Pietersen
		  :prop = 1"""

test16 :: String
test16 = """Text :Mytext
:Aangifte :A
	:aangever =>
		:RolDef :Pietersen
			public :betrouwbaarheid = 1"""

test17 :: String
test17 = """Text :A
:Aangifte :B12"""
-- runTest (runIndentParser test17 sourceText)

test18 :: String
test18 = """Text :T
:Aangifte :A1
	:aangever (0) => :Jansen
	:aangever (1) => :Pietersen
"""

test19 :: String
test19 = """:ContextType :Aangifte
	:publicProperty =>
		:Property $Urgentie
			public :prop = 3"""

test20 :: String
test20 = """:publicProperty =>
	:Property $Urgentie
		public :prop = 3"""

test21 :: String
test21 = """:ContextType :Aangifte
	:publicProperty => $Urgentie
		:rolprop = 3"""

test22 :: String
test22 = """$Property $Urgentie
	public $isFunctioneel = true
	-- Commentaar boven $isVerplicht
	public $isVerplicht = false"""

test23 :: String
test23 = """$ContextType $Aangifte
	-- Commentaar boven $aantekening
	public $aantekening = 1 -- Commentaar achter $aantekening
	-- Commentaar boven de binding van $Urgentie
	$publicProperty => $Jansen"""

test24 :: String
test24 = """$ContextType $Aangifte
	-- Commentaar boven $aantekening
	public $aantekening = 1 -- Commentaar achter $aantekening"""

test25 :: String
test25 = """-- Dit bestand bevat elke denkbare Perspectives CLR expressie.
Text $Mijntekst
$ContextType $Aangifte
	-- Commentaar boven $aantekening
	public $aantekening = 1 -- Commentaar achter $aantekening"""
