module Perspectives.ContextRoleParser where

import Perspectives.IndentParser
import Control.Alt ((<|>))
import Data.Array (cons, fromFoldable, many) as AR
import Data.Foldable (elem)
import Data.List.Types (List)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, empty, fromFoldable, insert, singleton, unions)
import Data.String (fromCharArray)
import Data.Tuple (Tuple(..))
import Perspectives.Guid (guid)
import Perspectives.Syntax2 (BinnenRol(..), EntityCollection(..), NamedEntityCollection(..), PerspectContext(..), PerspectEntity(..), PerspectRol(..), PropertyName, SimpleValue(..), TypeDeclaration(..), PerspectName)
import Perspectives.Token (token)
import Prelude (Unit, bind, pure, show, unit, ($), ($>), (*>), (<$>), (<*), (<*>), (<>))
import Text.Parsing.Indent (block, indented, sameOrIndented, withPos)
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.Combinators (notFollowedBy, try)
import Text.Parsing.Parser.Token (letter, upper)

-----------------------------------------------------------
-- Lexemes
-----------------------------------------------------------

reservedOp :: String -> IP Unit
reservedOp = token.reservedOp

reserved :: String -> IP Unit
reserved = token.reserved

identifier :: IP String
identifier = token.identifier

lexeme :: forall a. IP a -> IP a
lexeme = token.lexeme

int :: IP SimpleValue
int = Int <$> token.integer

bool :: IP SimpleValue
bool = reserved "true" $> Bool true <|> reserved "false" $> Bool false

string :: IP SimpleValue
string = String <$> token.stringLiteral

simpleValue :: IP SimpleValue
simpleValue = string <|> int <|> bool

-- | roleName = lowerCaseChar anyChar*
roleName :: IP String
roleName = fromCharArray <$> (notFollowedBy upper *> AR.many letter)

-- | propertyName = lowerCaseChar anyChar*
propertyName :: IP String
propertyName = roleName

-- | perspectName = upperCaseChar anyChar*
perspectName :: IP String
perspectName = f <$> upper <*> AR.many letter where
  f c ca = fromCharArray $ AR.cons c ca

-----------------------------------------------------------
-- Datatypes
-----------------------------------------------------------
dataTypes :: Array String
dataTypes = ["Number", "String", "Bool", "Date"]

dataType :: IP SimpleValue
dataType = try do
  s <- identifier
  if elem s dataTypes then pure $ String s else fail "Expected one of 'Number', 'String', 'Bool' or 'Date'."
-----------------------------------------------------------
-- Elementary expression types
-----------------------------------------------------------

-- | typeDeclaration = perspectName perspectName
typeDeclaration :: IP TypeDeclaration
typeDeclaration = TypeDeclaration <$> perspectName <*> perspectName

typedPropertyAssignment :: String -> IP (Tuple PropertyName (Array String))
typedPropertyAssignment scope = withPos $ (\pn pv -> Tuple pn [show pv])
    <$> identifier <* (sameOrIndented *> reservedOp "=")
    <*> (sameOrIndented *> (simpleValue <|> dataType))

-- | publicContextPropertyAssignment = 'public' propertyName '=' simpleValue
publicContextPropertyAssignment :: IP (Tuple PropertyName (Array String))
publicContextPropertyAssignment = reserved "public" *> typedPropertyAssignment "public"

-- | privateContextPropertyAssignment = 'private' propertyName '=' simpleValue
privateContextPropertyAssignment :: IP (Tuple PropertyName (Array String))
privateContextPropertyAssignment =reserved "private" *> typedPropertyAssignment "private"

-- | rolePropertyAssignment = propertyName '=' simpleValue
rolePropertyAssignment :: IP (Tuple PropertyName (Array String))
rolePropertyAssignment = withPos $ (\pn pv -> Tuple pn [show pv])
    <$> propertyName <* (sameOrIndented *> reservedOp "=")
    <*> (sameOrIndented *> (simpleValue <|> dataType))

-- | roleBinding = roleName '=>' (perspectName | expression) rolePropertyAssignment*
roleBinding :: PerspectName -> IP NamedEntityCollection
roleBinding contextID =
  withPos do
    rname <- (roleName <* (sameOrIndented *> reservedOp "=>"))
    ident <- (sameOrIndented *> perspectName)
    props <- indented *> withPos (block rolePropertyAssignment)
    -- pure $ RolBinding rname ident (EntityCollection empty) props
    pure $ (NamedEntityCollection ident
      (EntityCollection
        (singleton
          ident
          (Rol (PerspectRol
            { id: guid unit
            , pspType: rname
            , binding: Just ident
            , context: contextID
            , properties: fromFoldable props
            , gevuldeRollen: empty
            })))))
  <|>
  withPos do
    rname <- (roleName <* (sameOrIndented *> reservedOp "=>"))
    (NamedEntityCollection pname (EntityCollection entities)) <- context <|> role
    props <- indented *> withPos (block rolePropertyAssignment)
    -- pure $ RolBinding rname pname entities props
    rolId <- pure (guid unit)
    pure $ (NamedEntityCollection rolId
      (EntityCollection
        (insert rolId
          (Rol (PerspectRol
            { id: rolId
            , pspType: rname
            , binding: Just pname
            , context: contextID
            , properties: fromFoldable props
            , gevuldeRollen: empty
            }))
            entities)))

-- TODO: query

-----------------------------------------------------------
-- Context, Role and Expression
-----------------------------------------------------------

-- | context = typeDeclaration
-- | publicContextPropertyAssignment*
-- | privateContextPropertyAssignment*
-- | roleBinding*
context :: IP NamedEntityCollection
context = withPos do
  (TypeDeclaration typeName instanceName) <- typeDeclaration
  indented *> do
    (publicProps :: List (Tuple PropertyName (Array String))) <- (block publicContextPropertyAssignment)
    (privateProps :: List (Tuple PropertyName (Array String))) <- (block privateContextPropertyAssignment)
    (rolebindings :: List (NamedEntityCollection)) <- (block $ roleBinding instanceName)
    let
      ctxt = PerspectContext
        { id: instanceName
        , pspType: typeName
        , binnenRol: binnenRol
        , buitenRol: instanceName <> "_buitenRol"
        , rolInContext: AR.fromFoldable $ (\(NamedEntityCollection id _) -> id) <$> rolebindings }

      binnenRol = BinnenRol
        { id: instanceName <> "_binnenRol"
        , pspType: ":BinnenRol"
        , binding: Just $ instanceName <> "_buitenRol"
        , properties: fromFoldable privateProps
        }

      buitenRol = PerspectRol
        { id: instanceName <> "_buitenRol"
        , pspType: ":BuitenRol"
        , binding: Nothing
        , context: instanceName
        , properties: fromFoldable publicProps
        , gevuldeRollen: empty
        }

      (entities :: StrMap PerspectEntity) =
        unions $ (\(NamedEntityCollection _ (EntityCollection m)) -> m) <$> rolebindings

    pure
      (NamedEntityCollection instanceName
        (EntityCollection (insert instanceName (Context ctxt)
          (insert (instanceName <> "_buitenRol") (Rol buitenRol) entities))))

-- | role = typeDeclaration
-- | rolePropertyAssignment*
-- | query?
-- TODO: query
-- TODO: moet er geen binding bij?
role :: IP NamedEntityCollection
role = withPos do
  (TypeDeclaration typeName instanceName) <- typeDeclaration
  indented *> do
    props <- (block rolePropertyAssignment)
    pure
      (NamedEntityCollection instanceName
        (EntityCollection
          (singleton instanceName
            (Rol (PerspectRol
              { id: instanceName
              , pspType: typeName
              , binding: Nothing
              , context: instanceName
              , properties: fromFoldable props
              , gevuldeRollen: empty
              })))))

-- | expression = context | role
-- expression :: IP NamedEntityCollection
-- expression = NamedEntityCollection <$> pure "dummy" <*> (pure $ EntityCollection empty)
-- expression = context <|> role
