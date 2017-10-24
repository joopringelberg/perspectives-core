module Perspectives.Triples where

import Data.Maybe (Maybe)
import Perspectives.Property (PropertyName)
import Perspectives.TripleAdministration (NamedFunction, TripleGetter, constructTripleGetter)

type NamedSingleTripleGetter e = NamedFunction (TripleGetter e Maybe)

type NamedPluralTripleGetter e = NamedFunction (TripleGetter e Array)

getString :: forall e. PropertyName -> NamedSingleTripleGetter e
getString name = constructTripleGetter name

-- | in AsyncDomeinFile, retrieve either an Array of Strings or an error message.
getStrings :: forall e. PropertyName -> NamedPluralTripleGetter e
getStrings name = constructTripleGetter name

-- | in AsyncDomeinFile, retrieve either a Number or an error message.
getNumber :: forall e. PropertyName -> NamedSingleTripleGetter e
getNumber name = constructTripleGetter name

-- | in AsyncDomeinFile, retrieve either an Array of Numbers or an error message.
getNumbers :: forall e. PropertyName -> NamedPluralTripleGetter e
getNumbers name = constructTripleGetter name

-- | in AsyncDomeinFile, retrieve either a Boolean value or an error message.
getBoolean :: forall e. PropertyName -> NamedSingleTripleGetter e
getBoolean name = constructTripleGetter name
