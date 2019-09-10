module Perspectives.Representation.Assignment where

import Data.Array.Partial (head, tail)
import Data.Array (cons)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Foreign (ForeignError(..), fail, unsafeFromForeign, unsafeToForeign, F)
import Partial.Unsafe (unsafePartial)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, EnumeratedRoleType(..))
import Simple.JSON (class ReadForeign, class WriteForeign, writeJSON, readJSON)
import Prelude (class Eq, class Show, pure, ($), (<>), show, (<$>), (<*>))

type FunctionName = String

data AssignmentStatement
  = SetRol EnumeratedRoleType QueryFunctionDescription
  | AddToRol EnumeratedRoleType QueryFunctionDescription
  | RemoveFromRol EnumeratedRoleType QueryFunctionDescription
  | SetProperty EnumeratedPropertyType QueryFunctionDescription
  | AddToProperty EnumeratedPropertyType QueryFunctionDescription
  | RemoveFromProperty EnumeratedPropertyType QueryFunctionDescription
  | EffectFullFunction FunctionName (Array String)

derive instance genericRepAssignmentStatement :: Generic AssignmentStatement _

instance showAssignmentStatement :: Show AssignmentStatement where
  show x = genericShow x

instance eqAssignmentStatement :: Eq AssignmentStatement where
  eq x = genericEq x

type ConstructorRep = {tag :: String, dat :: Array String}

instance writeForeignAssignmentStatement :: WriteForeign AssignmentStatement where
  writeImpl (SetRol r v) = unsafeToForeign $ writeJSON {tag: "SetRol", dat: [writeJSON r, writeJSON v]}
  writeImpl (AddToRol r v) = unsafeToForeign $ writeJSON {tag: "AddToRol", dat: [writeJSON r, writeJSON v]}
  writeImpl (RemoveFromRol r v) = unsafeToForeign $ writeJSON {tag: "RemoveFromRol", dat: [writeJSON r, writeJSON v]}
  writeImpl (SetProperty r v) = unsafeToForeign $ writeJSON {tag: "SetProperty", dat: [writeJSON r, writeJSON v]}
  writeImpl (AddToProperty r v) = unsafeToForeign $ writeJSON {tag: "AddToProperty", dat: [writeJSON r, writeJSON v]}
  writeImpl (RemoveFromProperty r v) = unsafeToForeign $ writeJSON {tag: "RemoveFromProperty", dat: [writeJSON r, writeJSON v]}
  writeImpl (EffectFullFunction f as) = unsafeToForeign $ writeJSON {tag: "EffectFullFunction", dat: cons f as}


instance readForeignAssignmentStatement :: ReadForeign AssignmentStatement where
  readImpl q = case readJSON (unsafeFromForeign q) of
    (Left e) -> fail (ForeignError "Could not parse AssignmentStatement data")
    (Right ({tag, dat} :: ConstructorRep)) -> case tag of
      "SetRol" -> unsafePartial (SetRol <$> getRol dat <*> getValueQuery dat)
      "AddToRol" -> unsafePartial (AddToRol <$> getRol dat <*> getValueQuery dat)
      "RemoveFromRol" -> unsafePartial (RemoveFromRol <$> getRol dat <*> getValueQuery dat)
      "SetProperty" -> unsafePartial (SetProperty <$> getProperty dat <*> getValueQuery dat)
      "AddToProperty" -> unsafePartial (AddToProperty <$> getProperty dat <*> getValueQuery dat)
      "RemoveFromProperty" -> unsafePartial (RemoveFromProperty <$> getProperty dat <*> getValueQuery dat)
      "EffectFullFunction" -> pure $ unsafePartial $ EffectFullFunction (head dat) (tail dat)
      x -> fail $ ForeignError ("Unknown case in ReadForeign QueryFunction: " <> show x)

getValueQuery :: Array String -> F QueryFunctionDescription
getValueQuery as = case (readJSON (unsafePartial (head as))) of
    (Left e) -> fail $ ForeignError $ show e
    (Right p) -> pure $ unsafePartial $ p

getProperty :: Array String -> F EnumeratedPropertyType
getProperty as = case (readJSON (unsafePartial (head $ tail as))) of
    (Left e) -> fail $ ForeignError $ show e
    (Right p) -> pure $ unsafePartial $ p

getRol :: Array String -> F EnumeratedRoleType
getRol as = case (readJSON (unsafePartial (head $ tail as))) of
    (Left e) -> fail $ ForeignError $ show e
    (Right p) -> pure $ unsafePartial $ EnumeratedRoleType $ p
