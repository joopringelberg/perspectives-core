module Perspectives.Property where
import Perspectives.Location
{-
Een typeclass Property met vier instanties, één voor elke combinatie van 1 Resource of n Resources voor domein en range:
  - 1 to 1
  - 1 to n
  - n to 1
  - n to n
De verplichte functie van Property is dan:
getProperty :: forall a b. Location a -> Property -> Location b

Mogelijk nog een explosie, voor het verschil Resource - Literal. Dat voegt nog vier mogelijkheden toe, voor de vier verschillende ranges (domain is altijd Resource):
  - 1 to 1 Resource
  - 1 to 1 Literal
  - 1 to n Resources
  - 1 to n Literals
  - n to 1 Resource
  - n to 1 Literal
  - n to n Resources
  - n to n Literals

Meer soorten properties zijn er niet als we alleen letten op cardinaliteit van de bron- en targetlocatie en de mogelijkheid dat de target Literals en Resources kan bevatten.

Het idee is dat elke Property getypeerd wordt met één van deze acht en dat er dan dus altijd een instantie-specifieke functie beschikbaar is om deze property toe te passen op een Location.

Ai. Hier zit een denkfout in. De cardinaliteit van de bron is geen onderdeel van de property-definitie. Dan houdt je maar vier instanties over:
- 1 to 1 Resource
- 1 to 1 Literal
- 1 to n Resources
- 1 to n Literals
Hoe maak je dan het verschil voor Locaties met een enkele Resource en één met een Array van Resources?

Als we het verschil tussen 1 en n niet maken, blijft het verschil tussen Resource en Literal (voor de range) over.
-}

type Domain = Location

type PropertyName = String

newtype Resource = Resource
  { id :: String
  }

class SomeThing a b where
  getIt :: PropertyName -> Location a -> Location b

instance someThingRR :: SomeThing Resource Resource where
  getIt pn loc = loc

instance someThingRB :: SomeThing Resource Boolean where
  getIt pn loc = locate true

r1 :: Resource
r1 = Resource {id: "ri1"}
l1 :: Location Resource
l1 = locate r1

r2 :: Location
r2 = getIt "myProp" l1

{-newtype Range a = Range a

data NetworkLink domain range = NetworkLink (Location domain) range

instance connectionDRRR :: Connection (NetworkLink (Location Resource) Resource) Resource where
  getProperty (NetworkLink l _) domain = locate (Resource { id: "r1"})

class Connection source range where
  getProperty :: source -> range -> Location range

r1 :: Resource
r1 = Resource {id: "ri1"}
l1 :: Location Resource
l1 = locate r1
nl1 :: NetworkLink (Location Resource) Resource
nl1 = NetworkLink l1 r1
p1 = getProperty nl1 1

-}
