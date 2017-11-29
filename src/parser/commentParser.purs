module Perspectives.CommentParser where

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Monad (class Monad)
import Control.Monad.State (gets)
import Data.Array (many, null, replicate, snoc, union)
import Data.Char (toCharCode)
import Data.Char.Unicode.Internal (uIswspace)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.List (foldr, foldl)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, insert, lookup, singleton, empty, mapWithKey, union) as SM
import Data.String (Pattern(..), contains, fromCharArray, singleton)
import Perspectives.IndentParser (IP)
import Prelude (class Show, Unit, bind, otherwise, pure, show, ($), (*>), (-), (<$>), (<*), (<*>), (<<<), (<=), (<>), (==), (>>=), (||))
import Text.Parsing.Indent (checkIndent, sameLine, withPos)
import Text.Parsing.Parser (ParseError, ParseState(..), ParserT, fail)
import Text.Parsing.Parser.Combinators (lookAhead, many1Till, manyTill, optionMaybe, skipMany, skipMany1, try, (<?>))
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String (anyChar, string, char, satisfy) as SP

-----------------------------------------------------------
-- TODO
-----------------------------------------------------------
{-
  * multiLineComments.
-}

-----------------------------------------------------------
-- CommentIndex: administration of comments relative to lines of code
-----------------------------------------------------------

type CommentIndex = SM.StrMap Comments

type Comments = SM.StrMap (Array String)

type CommentCategory = String

commentIndex :: CommentIndex
commentIndex = SM.empty

addToCommentCategory :: CommentCategory -> CommentIndex -> Code -> Comment -> CommentIndex
addToCommentCategory cat i s c = case SM.lookup cat i of
  Nothing -> SM.insert cat (SM.singleton s [c]) i
  (Just bl) -> SM.insert cat (addComment s c bl) i

beforeLine :: CommentIndex -> Code -> Comment -> CommentIndex
beforeLine = addToCommentCategory "beforeLine"

afterLine :: CommentIndex -> Code -> Comment -> CommentIndex
afterLine = addToCommentCategory "afterLine"

followingLine ::CommentIndex ->  Code -> Comment -> CommentIndex
followingLine = addToCommentCategory "followingLine"

lookupInCommentCategory :: CommentCategory -> Code -> CommentIndex -> Maybe (Array Comment)
lookupInCommentCategory cat cod i = case SM.lookup cat i of
  Nothing -> Nothing
  (Just x) -> SM.lookup cod x

lookupCommentBeforeLine :: Code -> CommentIndex -> Maybe (Array Comment)
lookupCommentBeforeLine = lookupInCommentCategory "beforeLine"

lookupCommentAfterLine :: Code -> CommentIndex -> Maybe (Array Comment)
lookupCommentAfterLine = lookupInCommentCategory "afterLine"

lookupCommentFollowingLine :: Code -> CommentIndex -> Maybe (Array Comment)
lookupCommentFollowingLine = lookupInCommentCategory "followingLine"

mergeCommentIndices :: CommentIndex -> CommentIndex -> CommentIndex
mergeCommentIndices c1 c2 = SM.mapWithKey f (SM.union c1 c2) where
  f cat comments = case SM.lookup cat c2 of
    Nothing -> comments
    (Just cm) -> mergeComments comments cm

mergeComments :: Comments -> Comments -> Comments
mergeComments c1 c2 = SM.mapWithKey f (SM.union c1 c2) where
  f code comments = case SM.lookup code c2 of
    Nothing -> comments
    (Just ac) -> union comments ac

addComment :: Code -> Comment -> Comments -> Comments
addComment cod com coms = case SM.lookup cod coms of
  Nothing -> SM.insert cod [com] coms
  (Just cs) -> SM.insert cod (snoc cs com) coms

-----------------------------------------------------------
-- CommentAdmin: combination of CommentIndex, the last line of code parsed and all code parsed.
-----------------------------------------------------------

type LastCodeLine = String
type AllCodeLines = String

-- The first Code element represents the last line of code in the text; the second represents the text itself.
data CommentAdmin = CommentAdmin CommentIndex LastCodeLine AllCodeLines

instance showCommentAdmin :: Show CommentAdmin where
  show (CommentAdmin ind lastCode allcode) = show ind <> "\nLast code: " <> lastCode <> "\nAll code:\n" <> allcode

commentAdmin :: CommentAdmin
commentAdmin = CommentAdmin commentIndex "" ""

-- The second argument will provide the last code.
mergeCommentAdmins :: CommentAdmin -> CommentAdmin -> CommentAdmin
mergeCommentAdmins (CommentAdmin i1 _ allc1) (CommentAdmin i2 lc2 allc2) =
  CommentAdmin (mergeCommentIndices i1 i2) lc2 (allc1 <> allc2)

-----------------------------------------------------------
-- PositionedCode
-----------------------------------------------------------
type Line = Int
data PositionedCode = PositionedCode CommentIndex Code Line

instance showPositionedCode :: Show PositionedCode where
  show (PositionedCode ci c l) =
    "From line " <> show l <> ":\n" <>
    show ci <> "\n" <>
    show c

-----------------------------------------------------------
-- Handling position
-----------------------------------------------------------

-- | @ getPosition @ returns current position
-- | should probably be added to Text.Parsing.Parser.Pos
getPosition :: IP Position
getPosition = gets \(ParseState _ pos _) -> pos

sourceColumn :: Position -> Int
sourceColumn (Position {line: _, column: c}) = c

sourceLine :: Position -> Int
sourceLine (Position {line: l, column: _}) = l

getColumn :: IP Int
getColumn = getPosition >>= (\pos -> pure $ sourceColumn pos)

getLine :: IP Int
getLine = getPosition >>= (\pos -> pure $ sourceLine pos)

-----------------------------------------------------------
-- Parsers that parse a single line. They consume the newline and all following inlineSpace,
-- leaving the parser at the start of the next expression or the end of the next line.
-- All produce a newline terminated string.
-----------------------------------------------------------

type Code = String
type Comment = String

-- Consumes any character up to and including the newline.
untilNewline :: IP String
untilNewline = withPos (many1Till (sameLine *> SP.anyChar) (SP.char '\n')) <* inlineSpace >>= pure <<< charsToLine

-- Concatenate all characters.
charsToLine :: forall f. Foldable f => f Char -> String
charsToLine lc = foldr (\c s -> singleton c <> s) "\n" lc

-- An empty line starts in the leftmost column, contains just whiteSpace characters and is terminated by a newline.
emptyLine :: IP String
emptyLine = try $ manyTill (SP.satisfy isInlineSpace) (SP.char '\n') <* inlineSpace >>= pure <<< charsToLine

-- | This is a variant of isSpace.
-- | Returns `True` for any Unicode space character, and the control
-- | character `\t`.
-- |
-- | `isInlineSpace` includes non-breaking space.
isInlineSpace :: Char -> Boolean
-- The magic 0x377 used in the code below isn't really that magical. As of
-- 2014, all the codepoints at or below 0x377 have been assigned, so we
-- shouldn't have to worry about any new spaces appearing below there.
isInlineSpace c = if uc <= 0x337
               then uc == 32 || uc == 0xa0
               else uIswspace $ toCharCode c
  where
    uc :: Int
    uc = toCharCode c

inlineSpace :: IP Unit
inlineSpace = skipMany (simpleSpace <?> "") where
  simpleSpace :: forall m . Monad m => ParserT String m Unit
  simpleSpace = skipMany1 (SP.satisfy isInlineSpace)

-- | A comment starts with --.
comment :: IP Comment
comment = ((<>) <$> (SP.string "--") <*> untilNewline)

commentOrEmptyLine :: IP Comment
commentOrEmptyLine = checkIndent *> comment <|> emptyLine

-- | Matches code that is by itself on a single line.
codeLine :: IP CommentAdmin
codeLine = do
  (col :: Int) <- getColumn
  expr <- untilNewline
  case contains (Pattern "--") expr of
      true -> fail "Comment found in code"
      false -> pure $ CommentAdmin commentIndex expr (fromCharArray (replicate (col - 1) ' ') <> expr)

-- | Matches code that is followed by a comment on the same line. Matches up to the "--", leaving the parser just
-- | before them.
codeLineUpToComment :: IP String
codeLineUpToComment = (withPos $ many1Till (sameLine *> SP.anyChar) (lookAhead (SP.string "--")) >>= (pure <<< charsToLine))

-- | Matches some code followed by a comment on the same line.
codeLineWithInlineComment :: IP CommentAdmin
codeLineWithInlineComment = try do
  (col :: Int) <- getColumn
  (code :: String) <- codeLineUpToComment
  comnt <- comment
  pure $ CommentAdmin (afterLine commentIndex code comnt)
          code
          (fromCharArray(replicate (col - 1) ' ') <> code)

anyCodeLine :: IP CommentAdmin
anyCodeLine = codeLineWithInlineComment <|> codeLine

-- Helper functions for development.
allTheRest :: IP String
allTheRest = (many SP.anyChar) >>= pure <<< charsToLine

allTheRest' :: IP CommentAdmin
allTheRest' = allTheRest >>= (\s -> pure $ CommentAdmin commentIndex "" s)

-----------------------------------------------------------
-- Combinations of lines
-----------------------------------------------------------

-- | Parses any number of commentlines followed by a single line of code (with or without commment).
lines :: IP CommentAdmin
lines = try $ withPos $ f <$> (many commentOrEmptyLine) <*> (checkIndent *> anyCodeLine) where
  f comments ca | null comments = ca
  f comments ca@(CommentAdmin i c all) | otherwise = CommentAdmin (SM.insert "beforeLine" (SM.singleton c comments) i) c all

-- | Parses lines and a blok of content.
blok :: IP CommentAdmin -> IP CommentAdmin
blok blokContent = do
  commentAndCode <- lines
  withPos do
    -- content <- (block blokContent)
    content <- many (checkIndent *> blokContent)
    optionalComment <- optionMaybe (try commentOrEmptyLine)
    let cadmin@(CommentAdmin i last allcode) = foldl mergeCommentAdmins commentAndCode content
    case optionalComment of
      Nothing -> pure cadmin
      (Just c) -> pure $ CommentAdmin (followingLine i last c) last allcode

-- | Parses a blok, otherwise a single line of code optionally preceded by comments, recursively.
blokOrLines :: IP CommentAdmin
blokOrLines = fix f where
  f p = (blok p <|> lines)

-- Parses an entire file.
file :: IP (Array PositionedCode)
file = many (fixPosition blokOrLines) where
  fixPosition :: IP CommentAdmin -> IP PositionedCode
  fixPosition p = do
    l <- getLine
    (CommentAdmin ci _ code ) <- p
    pure $ PositionedCode ci code l

-- Helper functions.
getCode :: Either ParseError CommentAdmin -> Either ParseError Code
getCode (Right (CommentAdmin _ _ code)) = Right code
getCode (Left e) = Left e

getCommentIndex :: Either ParseError CommentAdmin -> Either ParseError CommentIndex
getCommentIndex (Right (CommentAdmin i _ _)) = Right i
getCommentIndex (Left e) = Left e

-----------------------------------------------------------
-- Tests.
-----------------------------------------------------------

test4 :: String
test4 = ":Aangifte :aangifte1 -- Hiermee definiëren we een instantie van :Aangifte\n"

test5 :: String
test5 = "-- Commentaar vooraf\n:Aangifte :aangifte1 -- Hiermee definiëren we een instantie van :Aangifte\n"

test6 :: String
test6 = """-- Commentaar vooraf-- Nog een keer commentaar vooraf\n:Aangifte :aangifte1 -- Hiermee definiëren we een instantie van :Aangifte\n"""

test7 :: String
test7 = """-- Eerste commentaar

Eerste codeRegel.
    -- Geïndenteerd commentaar

    Geïndenteerde code -- Commentaar achter geïndenteerde code.

    Laatste code
    -- Laatste commentaar

allerlaatste code
"""
-- runIndentParser test7 blokOrLines

t1 :: String
t1 = """-- Eerste commentaar
Eerste codeRegel.
"""

t2 :: String
t2 = """-- Geïndenteerd commentaar
Geïndenteerde code -- Commentaar achter geïndenteerde code
"""

t3 :: String
t3 = """Eerste codeRegel.
    -- Geïndenteerd commentaar
    laaste regel
"""
-- runIndentParser t3 (codeLine *> untilNewline)
-- (Right "laaste regel\n")
-- Hoogstwaarschijnlijk omdat lexeme óók commentaarregels opeet!

test8 :: String
test8 = """dit is code
    dit is ook code
en de laatste code
    """

test9 :: String
test9 = """-- Eerste commentaar
Eerste codeRegel.
    -- Geïndenteerd commentaar
    Geïndenteerde code -- Commentaar achter geïndenteerde code
    Laatste code
allerlaatste code
"""

test10 :: String
test10 = """-- Eerste commentaar
Eerste codeRegel.
    -- Geïndenteerd commentaar
    Geïndenteerde code
    Laatste code
allerlaatste code
"""

test11 :: String
test11 = """-- Eerste commentaar

Eerste codeRegel.
    -- Geïndenteerd commentaar
    Geïndenteerde code -- Commentaar achter geïndenteerde code
      --dubbel geindenteerd commentaar
      dubbel geindenteerde code
    Laatste code
    -- Laatste commentaar

-- Allerlaatste commentaar
allerlaatste code
"""

test12 :: String
test12 = """
"""
