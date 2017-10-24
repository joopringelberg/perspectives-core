module Test.Types where


import Prelude
import Data.BooleanAlgebra (not, (&&))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap)
import Partial.Unsafe (unsafePartial)

  {-
  Ik wil rekening houden met Resources en Literals, beiden alleenstaand en in Arrays, en binnen Literals in elk geval Booleans en Integers van elkaar onderscheiden.
  Verder kan een locatie wel of niet een waarde bevatten: Nothing of Maybe Resource, enz.
  Ik heb de functie 'getProperty' nodig die al die gevallen als argument accepteert.
  Bovendien kan getProperty elk van die gevallen opleveren.
  Hoe moet dat?

  Een mogelijkheid lijkt me een record-type. Daarin kan ik het type als een veld representeren.
  Maar dan moeten alle operaties op dit record type worden gedefinieerd. Bijvoorbeeld het optellen van getallen, logische operaties op booleans, enzovoort. Dat is niet fijn. Ik wil Locaties met gewone Integers, Booleans, enz.

  Voorlopige conclusies:
  1. Je kunt niet het type bepalen op basis van informatie die pas runtime beschikbaar is.
  2. Een Class constraint op de output geeft onvoldoende informatie aan de type-checker om als typering van het functie-resultaat te kunnen dienen.
  3. Dit is voor Resources geen probleem.
  4. We kunnen echter niet het verschil tussen functioneel en niet functioneel maken. Ook dat is pas runtime bekend.

  Hoe kunnen we nu het optellen en de logische operaties enz. realiseren?
  -}

  {-  type Id = String
    data Res = Resource Id | Literal
    data Literal = Bool Boolean | Number Int
    data PropertyValue = Array Res | Res

    -- Distinghuish various property types from each other. A property is a resource.
    --newtype LiteralProperty = LiteralProperty Resource


    getProperty :: Resource -> String -> PropertyValue
    getProperty r propName | isLiteralResource =

    r1 :: Res
    r1 = Resource "r1"

    l1 :: Literal
    l1 = Bool true
  -}


{-type Resource = { id :: String }
type Property a = { id :: String
                , isFunctional :: Boolean
                , range :: a }

getProperty :: forall a. Partial => Resource -> Property a -> Maybe a
getProperty r {isFunctional, id, range} | isFunctional = Just range

p1 :: Property String
p1 = { id: "label", isFunctional: true, range: "sample label"}

r1 :: Resource
r1 = { id: "r1" }

v1 :: Maybe String
v1 = unsafePartial getProperty r1 p1
-}

{-newtype Resource = Resource {id :: String}
newtype Resources = Resources (Array Resource)
class PropertyValue a

instance propertyValueResource :: PropertyValue Resource
instance propertyValueResources :: PropertyValue Resources

newtype Property = Property{ id :: String
                , isFunctional :: Boolean }

getPropertyValue :: forall a. PropertyValue a => Resource -> Property -> a
getPropertyValue r (Property {isFunctional: true}) = Resource {id : "aap"}
-}

-- Merk op dat 'anyProperty' natuurlijk vervangen moet worden door werkelijke propertynamen. Dit is meer een sjabloon dan een type. Oftewel, je kunt voor elke instantie van de MetaClasses een type maken.
newtype SerializedResource = SerializedResource
  { id :: String
  , anyProperty :: Array String }

-- We hebben dus een abstracter type nodig om SerializedResource te beschrijven. Hiermee beschrijven we een willekeurige resource (afgezien van geneste Entiteiten). We kunnen code schrijven die uit de database values van dit type oplevert.
newtype SerializedResource1 = SerializedResource1
  { id :: String
  , propertyValuePairs :: StrMap (Array String) }

{-
De grote vraag is: (hoe) kunnen we een overstap maken van een string "true" naar de Boolean waarde true? Concreet in de situatie waarin je een property met een Boolean range opvraagt van een Resource, hoe maak je dan een Location met daarin een Boolean in plaats van "true", of, erger nog: ["true"]?
Het is mogelijk een functie te schrijven die dat doet:
  getBooleanProperty :: PropertyName -> Resource -> Boolean
Maar je kunt dus geen functie Decide schrijven die deze functie inzet op basis van runtime informatie (c.q. de range van de property), want het resultaat van Decide is niet te typeren (aannemend dat je met Decide ook b.v. Ints zou willen kunnen opleveren).
Dit heeft alles te maken met de ondersteuning van de type checker bij het schrijven van queries.
Of liever, met het gebrek aan ondersteuning.
De type checker wijst je namelijk alleen op een type mismatch in een functie over Locaties a, voor types a die hem bekend zijn.
dit kan dus niet:
test n = if ( n == 1 ) then "aap" else 0
-}
