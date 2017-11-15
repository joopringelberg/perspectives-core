module Perspectives.CommentParser where

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Monad (class Monad)
import Control.Monad.State (gets)
import Data.Array (cons, many, null, replicate, union)
import Data.Char.Unicode (isSpace)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.List (foldr, foldl)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, insert, lookup, singleton, empty, mapWithKey, union) as SM
import Data.String (Pattern(..), contains, fromCharArray, singleton)
import Text.Parsing.Indent (block, checkIndent, sameLine, withPos)
import Text.Parsing.Parser (ParseError, ParseState(..), ParserT, fail)
import Text.Parsing.Parser.Combinators (lookAhead, many1Till, optionMaybe, skipMany, skipMany1, try, (<?>))
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String (anyChar, string, char, satisfy) as SP
import Perspectives.IndentParser (IP)
import Prelude (class Show, Unit, bind, otherwise, pure, show, ($), (*>), (-), (<$>), (<*), (<*>), (<<<), (<>), (>>=))

-----------------------------------------------------------
-- TODO
-----------------------------------------------------------
{-
  * multiLineComments.
  * ofwel de resulterende code zonder commentaar indenteren, ofwel die parseren.
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
  (Just cs) -> SM.insert cod (cons com cs) coms

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
-- Parsers that parse a single line
-----------------------------------------------------------

type Code = String
type Comment = String

-- Consumes any character up to and including the newline and produces a newline terminated string.
untilNewline :: IP String
untilNewline = withPos (many1Till (sameLine *> SP.anyChar) (SP.char '\n')) >>= pure <<< charsToLine

-- Concatenate all characters.
charsToLine :: forall f. Foldable f => f Char -> String
charsToLine lc = foldr (\c s -> singleton c <> s) "" lc

whiteSpace :: IP Unit
whiteSpace = skipMany (simpleSpace <?> "") where
  simpleSpace :: forall m . Monad m => ParserT String m Unit
  simpleSpace = skipMany1 (SP.satisfy isSpace)

-- returns a comment and leaves the parser at the start of the next expression (having eaten all whiteSpace).
comment :: IP Comment
comment = (<>) <$> (SP.string "--") <*> (untilNewline <* whiteSpace)

-- Only matches code that is by itself on a single line and leaves the parser at the start of the next expression (having eaten all whiteSpace).
codeLine :: IP CommentAdmin
codeLine = try do
  (col :: Int) <- getColumn
  expr <- untilNewline <* whiteSpace
  case contains (Pattern "--") expr of
      true -> fail "Comment found in code"
      false -> pure $ CommentAdmin commentIndex expr (fromCharArray (replicate (col - 1) ' ') <> expr <> "\n")

-- Only matches code that is followed by a comment on the same line, leaving the parser at the start of that comment.
codeLineUpToComment :: IP String
codeLineUpToComment = withPos $ many1Till (sameLine *> SP.anyChar) (lookAhead (SP.string "--")) >>= (pure <<< charsToLine)
-- codeLineUpToComment = withPos do
--   (col :: Int) <- getColumn
--   chars <- many1Till (sameLine *> SP.anyChar) (lookAhead (SP.string "--"))
--   pure (fromCharArray (replicate (col - 1) ' ') <> charsToLine chars <> "\n")

-- Only matches some code followed by a comment on the same line, leaving the parser at the start of the next expression.
codeLineWithInlineComment :: IP CommentAdmin
-- codeLineWithInlineComment = try (f <$> codeLineUpToComment <*> comment) where
  -- f code com = CommentAdmin (afterLine commentIndex code com) code code

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
lines = try $ withPos $ f <$> (many (checkIndent *> comment)) <*> (checkIndent *> anyCodeLine) where
  f comments ca | null comments = ca
  f comments ca@(CommentAdmin i c all) | otherwise = CommentAdmin (SM.insert "beforeLine" (SM.singleton c comments) i) c all

-- | Parses lines and a blok of content.
blok :: IP CommentAdmin -> IP CommentAdmin
blok blokContent = do
  commentAndCode <- lines
  withPos do
    content <- (block blokContent)
    optionalComment <- optionMaybe (try (checkIndent *> comment))
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
    Geïndenteerde code -- Commentaar achter geïndenteerde code
    Laatste code
    -- Laatste commentaar
allerlaatste code
"""

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
allerlaatste code
"""
