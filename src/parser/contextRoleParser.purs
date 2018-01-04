module Perspectives.ContextRoleParser where

import Perspectives.IndentParser
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, runAff_)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.State (get, gets)
import Control.Monad.Trans.Class (lift)
import Data.Array (dropEnd, intercalate)
import Data.Array (cons, many, snoc, length) as AR
import Data.Char.Unicode (isLower)
import Data.Foldable (elem, fold)
import Data.List.Types (List(..))
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (StrMap, empty, fromFoldable, insert, lookup, singleton)
import Data.String (Pattern(..), fromCharArray, split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Perspectives.Resource (PROPDEFS, getContext, storeContextInResourceDefinitions, storeRoleInResourceDefinitions)
import Perspectives.ResourceTypes (DomeinFileEffects)
import Perspectives.Syntax (BinnenRol(..), Comment, Comments(..), ContextDeclaration(..), DomeinName, Expanded(..), ID, PerspectContext(..), PerspectRol(..), Prefix, PropertyName, PropertyValueWithComments, RoleName, SimpleValue(..), TextDeclaration(..))
import Perspectives.Token (token)
import Prelude (class Show, Unit, bind, discard, id, pure, show, unit, ($), ($>), (*>), (+), (-), (/=), (<$>), (<*), (<*>), (<>), (==), (>), (>>=))
import Text.Parsing.Indent (block, checkIndent, indented, sameLine, withPos)
import Text.Parsing.Parser (ParseState(..), fail)
import Text.Parsing.Parser.Combinators (choice, option, optionMaybe, try, (<?>), (<??>))
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String (char, satisfy, whiteSpace)
import Text.Parsing.Parser.String (anyChar, oneOf, string) as STRING
import Text.Parsing.Parser.Token (alphaNum, upper)

-----------------------------------------------------------
-- Comments
-----------------------------------------------------------

inLineComment :: forall e. IP (Array Comment) e
inLineComment = option [] do
  sameLine
  _ <- (STRING.string "--")
  chars <- AR.many (satisfy (_ /= '\n'))
  _ <- whiteSpace
  pure [fromCharArray chars]

manyOneLineComments :: forall e. IP (Array Comment) e
manyOneLineComments = AR.many
    (fromCharArray <$> ((STRING.string "--") *> (AR.many (satisfy (_ /= '\n')) <* whiteSpace)))

-----------------------------------------------------------
-- Identifiers
-----------------------------------------------------------

reservedOp :: forall e. String -> IP Unit e
reservedOp = token.reservedOp

reserved :: forall e. String -> IP Unit e
reserved = token.reserved

identifier :: forall e. IP String e
identifier = token.identifier

lexeme :: forall a e. IP a e -> IP a e
lexeme = token.lexeme

int :: forall e. IP SimpleValue e
int = Int <$> token.integer

bool :: forall e. IP SimpleValue e
bool = reserved "true" $> Bool true <|> reserved "false" $> Bool false

string :: forall e. IP SimpleValue e
string = String <$> token.stringLiteral

simpleValue :: forall e. IP SimpleValue e
simpleValue = string <|> int <|> bool

identLetter :: forall e. IP Char e
identLetter = alphaNum <|> STRING.oneOf ['_', '\'']

identLetterString :: forall e. IP String e
identLetterString = fromCharArray <$> AR.many identLetter

-- /([A-Z]\w*\b)/
-- /(\p{Uppercase}[\p{Alphabetic}\p{Mark}\p{Decimal_Number}\p{Connector_Punctuation}\p{Join_Control}]+)/gu
capitalizedString :: forall e. IP String e
capitalizedString = f <$> upper <*> AR.many identLetter where
  f c ca = fromCharArray $ AR.cons c ca

lower :: forall e. IP Char e
lower = satisfy isLower <?> "uppercase letter"

-- /([a-z]\w*\b)/
-- /(\b\p{Lowercase}[\p{Alphabetic}\p{Mark}\p{Decimal_Number}\p{Connector_Punctuation}\p{Join_Control}]+\b)/gu
uncapitalizedString :: forall e. IP String e
uncapitalizedString = f <$> lower <*> AR.many identLetter where
  f c ca = fromCharArray $ AR.cons c ca

-- domeinName = 'model:' upper alphaNum* '$'
domeinName :: forall e. IP String e
domeinName = do
  _ <- STRING.string "model:"
  domein <- capitalizedString
  _ <- char '$'
  pure $ "model:" <> domein <> "$"

-- localContextName = upper alphaNum*
localContextName :: forall e. IP String e
localContextName = f <$> capitalizedString <*> AR.many (defaultEmbedded capitalizedString) where
  f first rest = fold $ AR.cons first rest

defaultEmbedded :: forall e. IP String e -> IP String e
defaultEmbedded p = (<>) <$> STRING.string "$" <*> p

-- localPropertyName = lower alphaNum*
localPropertyName :: forall e. IP String e
localPropertyName = uncapitalizedString

-- prefix = lower+ ':'
prefix :: forall e. IP String e
prefix = (f <$> lower <*> AR.many lower <*> char ':') where
  f fst ca c = fromCharArray $ AR.cons fst (AR.snoc ca c)

prefixedName :: forall e. IP String e -> IP Expanded e
prefixedName localName = lexeme do
  pre <- prefix
  ln <- localName
  namespace <- prefix2namespace pre
  pure $ Expanded namespace ln

-- prefixedContextName = prefix localContextName
prefixedContextName :: forall e. IP Expanded e
prefixedContextName = prefixedName localContextName

-- prefixedPropertyName = prefix localPropertyName
prefixedPropertyName :: forall e. IP Expanded e
prefixedPropertyName = prefixedName localPropertyName

-- qualifiedResourceName = domeinName localContextName
expandedContextName :: forall e. IP Expanded e
expandedContextName = lexeme (Expanded <$> domeinName <*> localContextName)

-- expandedPropertyName = domeinName localPropertyName
expandedPropertyName :: forall e. IP Expanded e
expandedPropertyName = lexeme (Expanded <$> domeinName <*> localPropertyName)

defaultNamespacedContextName :: forall e. IP Expanded e
defaultNamespacedContextName = lexeme do
  namespace <- getNamespace
  namespaceLevels <- AR.length <$> AR.many (STRING.string "$")
  localName <- localContextName
  namespace' <- (butLastNNamespaceLevels namespace (namespaceLevels - 1))
  pure $ Expanded namespace' ("$" <> localName)
  where
    butLastNNamespaceLevels :: String -> Int -> IP String e
    butLastNNamespaceLevels _ -1 = fail "local name starting with '$'."
    butLastNNamespaceLevels ns 0 = pure ns
    butLastNNamespaceLevels ns n = do
      segments <- pure (split (Pattern "$") ns)
      if (n - 1) > (AR.length segments)
        then fail "too many levels up"
        else pure $ intercalate "$" (dropEnd n segments)

-- cname = prefixedContextName | expandedContextName
contextName :: forall e. IP Expanded e
contextName = (expandedContextName <|> prefixedContextName <|> defaultNamespacedContextName) <?> "the name of a resource (Context or Role)."

-- propertyName = prefixedPropertyName | expandedPropertyName
propertyName :: forall e. IP Expanded e
propertyName = (expandedPropertyName <|> prefixedPropertyName) <?> "a property or role name."

roleName :: forall e. IP Expanded e
roleName = propertyName

-----------------------------------------------------------
-- Datatypes
-----------------------------------------------------------
dataTypes :: Array String
dataTypes = ["Number", "String", "Bool", "Date"]

dataType :: forall e. IP SimpleValue e
dataType = try do
  s <- identifier
  if elem s dataTypes then pure $ String s else fail "one of 'Number', 'String', 'Bool' or 'Date'."

-----------------------------------------------------------
-- Handling position
-----------------------------------------------------------

-- | @ getPosition @ returns current position
-- | should probably be added to Text.Parsing.Parser.Pos
getPosition :: forall e. IP Position e
getPosition = gets \(ParseState _ pos _) -> pos

sourceColumn :: Position -> Int
sourceColumn (Position {line: _, column: c}) = c

sourceLine :: Position -> Int
sourceLine (Position {line: l, column: _}) = l

-- | Parses only on the next line as the reference
nextLine :: forall e. IP Unit e
nextLine = do
    pos <- getPosition
    s   <- lift get
    if sourceLine s + 1 == sourceLine pos then pure unit else fail "not on next line"

-----------------------------------------------------------
-- Elementary expression types
-----------------------------------------------------------

-- | contextDeclaration = contextName contextName
textDeclaration :: forall e. IP TextDeclaration e
textDeclaration = (TextDeclaration <$> (reserved "Text" *> contextName) <*> inLineComment) <?> "the text declaration: Text <name>."

-- | contextDeclaration = contextName contextName
contextDeclaration :: forall e. IP ContextDeclaration e
contextDeclaration = (ContextDeclaration <$> contextName <*> contextName <*> inLineComment) <?> "a type declaration, e.g. :ContextType :ContextName."

-- | Apply to a single line parser. Will parse a block of contiguous line comments before the line and
-- | the comment after the expression on the line.
withComments :: forall e a. IP a e -> IP (Tuple (Comments ()) a) e
withComments p = do
  before <- manyOneLineComments
  withPos do
    a <- p
    after <- inLineComment
    pure $ Tuple (Comments{ commentBefore: before, commentAfter: after}) a

typedPropertyAssignment :: forall e. IP Unit e -> IP (Tuple ID PropertyValueWithComments) e
typedPropertyAssignment scope = go (try (withComments
  (withPos
    (Tuple
      <$> (scope *> (propertyName <* (sameLine *> reservedOp "=")))
      <*> (sameLine *> (simpleValue <|> dataType))))))
  where
    go x = do
      (Tuple (Comments {commentBefore, commentAfter}) (Tuple pname value)) <- x
      pure $ Tuple (show pname) (Comments {value: [show value], commentBefore: commentBefore, commentAfter: commentAfter})

-- | publicContextPropertyAssignment = 'public' propertyName '=' simpleValue
publicContextPropertyAssignment :: forall e. IP (Tuple ID PropertyValueWithComments) e
publicContextPropertyAssignment = (typedPropertyAssignment (reserved "public")) <?> "public propertyname = value"

-- | privateContextPropertyAssignment = 'private' propertyName '=' simpleValue
privateContextPropertyAssignment :: forall e. IP (Tuple ID PropertyValueWithComments) e
privateContextPropertyAssignment = (typedPropertyAssignment (reserved "private")) <?> "private propertyname = value"

-- | rolePropertyAssignment = propertyName '=' simpleValue
rolePropertyAssignment :: forall e. IP (Tuple ID PropertyValueWithComments) e
rolePropertyAssignment = (typedPropertyAssignment (pure unit)) <?> "private propertyname = value"

isRoleDeclaration :: forall e. IP Unit (DomeinFileEffects e)
isRoleDeclaration = withPos (roleName *> (sameLine *> optionMaybe roleOccurrence) *> (sameLine *> reservedOp "=>") *> pure unit)

roleOccurrence :: forall e. IP Int e
roleOccurrence = token.parens token.integer

roleBinding' :: forall e.
  Expanded
  -> IP (Tuple (Array Comment) ID) (DomeinFileEffects e)
  -> IP (Tuple RoleName ID) (DomeinFileEffects e)
roleBinding' cname p = ("rolename => contextName" <??>
  (try do
    -- Parsing
    cmtBefore <- manyOneLineComments
    withPos do
      rname@(Expanded _ localRoleName) <- roleName
      occurrence <- sameLine *> optionMaybe roleOccurrence -- The sequence number in text
      _ <- (sameLine *> reservedOp "=>")
      (Tuple cmt binding) <- p
      props <- option Nil (indented *> (block (checkIndent *> rolePropertyAssignment)))
      _ <- incrementRoleInstances localRoleName

      -- Naming
      nrOfRoleOccurrences <- getRoleOccurrences localRoleName -- The position in the sequence.
      rolId <- pure ((show cname) <> localRoleName <> "_" <> (show (roleIndex occurrence nrOfRoleOccurrences)))

      -- Storing
      liftAffToIP $ storeRoleInResourceDefinitions rolId
        (PerspectRol
          { id: rolId
          , occurrence: (roleIndex occurrence nrOfRoleOccurrences)
          , pspType: show rname
          , binding: Just binding
          , context: show cname
          , properties: fromFoldable ((\(Tuple en cm) -> Tuple (show en) cm) <$> props)
          , gevuldeRollen: empty
          , comments: Comments { commentBefore: cmtBefore, commentAfter: cmt }
          })
      pure $ Tuple (show rname) rolId))
  where
    -- If there is an index in the text, it prevails.
    roleIndex :: Maybe Int -> Maybe Int -> Int
    roleIndex nrInText nrInSequence = case nrInText of
      (Just n) -> n
      Nothing -> case nrInSequence of
        (Just n) -> n
        Nothing -> 0

-- | The inline context may itself use a defaultNamespacedContextName name. However,
-- | what is returned from the context parser is an ExpandedQN.
roleBindingWithInlineContext :: forall e. Expanded
  -> IP (Tuple RoleName ID) (DomeinFileEffects e)
roleBindingWithInlineContext cName = roleBinding' cName do
  cmt <- inLineComment
  _ <- nextLine
  (contextBuitenRol :: ID) <- indented *> context
  pure $ Tuple cmt contextBuitenRol

-- | The reference may be defaultNamespacedContextName.
roleBindingWithReference :: forall e. Expanded
  -> IP (Tuple RoleName ID) (DomeinFileEffects e)
roleBindingWithReference cName = roleBinding' cName do
  (ident :: Expanded) <- (sameLine *> contextName)
  cmt <- inLineComment
  pure $ Tuple cmt ((show ident) <> "_buitenRol")

-- | roleBinding = roleName '=>' (contextName | context) rolePropertyAssignment*
roleBinding :: forall e. Expanded
  -> IP (Tuple RoleName ID) (DomeinFileEffects e)
roleBinding cname = roleBindingWithInlineContext cname <|> roleBindingWithReference cname -- TODO: query

withRoleCounting :: forall a e. IP a e -> IP a e
withRoleCounting p = do
  roleInstances <- getRoleInstances
  setRoleInstances empty
  r <- p
  setRoleInstances roleInstances
  pure r

-----------------------------------------------------------
-- Context
-----------------------------------------------------------
-- | context = contextDeclaration
-- | publicContextPropertyAssignment*
-- | privateContextPropertyAssignment*
-- | roleBinding*
-- | The parser never backtracks over a Context. This means we can safely perform the side
-- | effect of storing its constituent roles and contexts.
context :: forall e. IP ID (DomeinFileEffects e)
context = withRoleCounting context' where
  context' = do
    -- Parsing
    cmtBefore <- manyOneLineComments
    withPos do
      (ContextDeclaration typeName instanceName@(Expanded dname localName) cmt) <- contextDeclaration

      -- Naming
      extendNamespace localName
        do
          -- Parsing the body
          (publicProps :: List (Tuple ID PropertyValueWithComments)) <- option Nil (indented *> (block publicContextPropertyAssignment))
          (privateProps :: List (Tuple ID PropertyValueWithComments)) <- option Nil (indented *> (block privateContextPropertyAssignment))
          (rolebindings :: List (Tuple RoleName ID)) <- option Nil (indented *> (block $ roleBinding instanceName))

          -- Storing
          liftAffToIP $ storeContextInResourceDefinitions (show instanceName)
            (PerspectContext
              { id: (show instanceName)
              , displayName : localName
              , pspType: show typeName
              , binnenRol:
                BinnenRol
                  { id: (show instanceName) <> "_binnenRol"
                  , pspType: "model:Perspectives$BinnenRol"
                  , binding: Just $ (show instanceName) <> "_buitenRol"
                  , properties: fromFoldable privateProps
                  }
              , buitenRol: (show instanceName) <> "_buitenRol"
              , rolInContext: collect rolebindings
              , comments: Comments { commentBefore: cmtBefore, commentAfter: cmt}
            })
          liftAffToIP $ storeRoleInResourceDefinitions ((show instanceName) <> "_buitenRol")
            (PerspectRol
              { id: (show instanceName) <> "_buitenRol"
              , occurrence: 0
              , pspType: "model:Perspectives$BuitenRol"
              , binding: Nothing
              , context: (show instanceName)
              , properties: fromFoldable publicProps
              , gevuldeRollen: empty
              , comments: Comments { commentBefore: [], commentAfter: []}
              })
          pure $ (show instanceName) <> "_buitenRol"
  collect :: List (Tuple RoleName ID) -> StrMap (Array ID)
  collect Nil = empty
  collect (Cons (Tuple rname id) r) = let map = collect r in
    case lookup rname map of
      Nothing -> insert rname [id] map
      (Just ids) -> insert rname (AR.cons id ids) map

-- Helper functions for development.
allTheRest :: forall e. IP String e
allTheRest = fromCharArray <$> (AR.many STRING.anyChar)

-----------------------------------------------------------
-- ContextName and QualifiedName
-----------------------------------------------------------

-- TODO: look up the prefix in the state!
prefix2namespace :: forall e. Prefix -> IP DomeinName e
prefix2namespace prefx = pure prefx


-----------------------------------------------------------
-- Expression
-----------------------------------------------------------

expression :: forall e. IP String (DomeinFileEffects e)
expression = choice
  [ try (textDeclaration *> (pure "textDeclaration"))
  , try (contextDeclaration *> (pure "contextDeclaration"))
  , try (publicContextPropertyAssignment *> (pure "publicContextPropertyAssignment"))
  , try (privateContextPropertyAssignment *> (pure "privateContextPropertyAssignment"))
  , try (rolePropertyAssignment *> (pure "rolePropertyAssignment"))
  , try (isRoleDeclaration *> (pure "isRoleDeclaration" ))
  , try ((STRING.string "--") *> (pure "oneLineComment"))
  -- query
  ]

-----------------------------------------------------------
-- Text
-----------------------------------------------------------
sourceText :: forall e. IP ID (DomeinFileEffects e)
sourceText = withRoleCounting sourceText' where
  sourceText' = do
    cmtBefore <- manyOneLineComments
    withPos do
      (TextDeclaration textName@(Expanded _ localName) cmt) <- textDeclaration
      (publicProps :: List (Tuple PropertyName PropertyValueWithComments)) <- (block publicContextPropertyAssignment)
      (privateProps :: List (Tuple PropertyName PropertyValueWithComments)) <- (block privateContextPropertyAssignment)
      -- TODO: introduceer Section hier.
      defs <- AR.many context
      (roleBindings :: Array String) <- (traverse (\buitenRolId ->
        do
          _ <- incrementRoleInstances "psp:text_Item"
          nrOfRoleOccurrences <- getRoleOccurrences "psp:text_Item"
          rolId <- pure $ (show textName) <> "_psp:text_Item_" <> maybe "0" show nrOfRoleOccurrences
          -- let roleId = guid unit
          liftAffToIP $ storeRoleInResourceDefinitions rolId
            (PerspectRol
              { id: rolId
              , occurrence: maybe 0 id nrOfRoleOccurrences
              , pspType: "psp:text_Item"
              , binding: Just buitenRolId
              , context: show textName
              , properties: empty
              , gevuldeRollen: empty
              , comments: Comments { commentBefore: cmtBefore, commentAfter: cmt}
              })
          pure rolId) defs)
      liftAffToIP $ storeContextInResourceDefinitions (show textName)
        (PerspectContext
          { id: show textName
          , displayName : localName
          , pspType: "model:Perspectives$SourceText"
          , binnenRol:
            BinnenRol
              { id: (show textName) <> "_binnenRol"
              , pspType: "model:Perspectives$BinnenRol"
              , binding: Just $ (show textName) <> "_buitenRol"
              , properties: fromFoldable privateProps
              }
          , buitenRol: (show textName) <> "_buitenRol"
          , rolInContext: singleton "psp:text_Item" roleBindings
          , comments: Comments { commentBefore: cmtBefore, commentAfter: cmt}
          })

      liftAffToIP $ storeRoleInResourceDefinitions ((show textName) <> "_buitenRol")
        (PerspectRol
          { id: show textName <> "_buitenRol"
          , occurrence: 0
          , pspType: "model:Perspectives$BuitenRol"
          , binding: Nothing
          , context: show textName
          , properties: fromFoldable publicProps
          , gevuldeRollen: empty
          , comments: Comments { commentBefore: [], commentAfter: []}
          })
      pure $ show textName

-----------------------------------------------------------
-- Tests
-----------------------------------------------------------

-- runAff_ (\_->pure unit) ((runIndentParser test1 context) >>= (\r -> log (show r)))

runTest :: forall e a. Show a => Aff (console :: CONSOLE | e) a -> Eff (console :: CONSOLE | e) Unit
runTest t =
  runAff_ (\_->pure unit) (t >>= (\r -> log (show r)))

getContextDef :: forall e. String -> Aff (DomeinFileEffects (prd :: PROPDEFS | e)) (Maybe PerspectContext)
getContextDef id = do
  getContext id

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
