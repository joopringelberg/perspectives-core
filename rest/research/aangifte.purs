module Research.Aangifte where

-- data Aangifte = Aangifte Aangever Verbalisant Getuige

-- record is product type met namen.
-- data Aangever =

type Name = String
type IsFunctional = Boolean

-- Rollen als telescopen. Maar waar blijven de properties?
data Aangever = Aangever Persoon Gebruiker

-- Een sum type in de vorm van een record. Je wil dan een annotatie dat deze rol niet functioneel is. Maar hoe?
newtype Aangever' = Aangever' { vuller :: Persoon, betrouwbaarheid :: String }

data Verbalisant = Verbalisant Medewerker Persoon Gebruiker

-- instance level definition of properties.
data Range = PBool | PString | PNumber
data Property = Property Name IsFunctional Range

-- types as annotations? Vb Partial
-- data Betrouwbaarheid = IsFunctional <= Betrouwbaarheid
-- newtype IsFunctional = IsFunctional
-- newtype IsNotFunctional = IsNotFunctional
