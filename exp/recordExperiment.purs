module RecordExperiment where

import Prelude

import Data.Symbol (SProxy(..))
import Data.Record as Record

x_ = SProxy :: SProxy "x"

-- we can get a value out of a field
-- Dus: met een term-level veldnaam kun je een record uitlezen.
gotX :: Int
gotX = Record.get x_ { x: 1 }

-- we can insert a value into a record that does not have a field at that label yet
-- Dus: je kunt insertedX.x evalueren en dat heeft type Int.
-- En: je kunt met een term level veldnaam een veld toevoegen aan een Record type.
-- Als je het type van insertedX niet geeft, leidt de compiler het zelf af.
insertedX :: { x :: Int }
insertedX = Record.insert x_ 1 {}
