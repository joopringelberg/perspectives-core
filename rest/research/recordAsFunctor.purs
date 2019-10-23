module Research.RecordAsFunctor where

import Data.Array.Partial (head)
import Data.Maybe (Maybe(..))
import Data.Show (class Show, show)
import Data.String (length)
import Prelude (class Apply, class Functor, apply, map, (+), (<*>), (<<<), (<=<), (<>), (>=>), (>>=))

newtype Triple a = Triple
  { subject :: Resource
  , predicate :: String
  , object :: Maybe a
  , supports :: Array TripleRef
  , dependencies :: Array TripleRef}

newtype TripleRef = TripleRef { subject :: String, predicate :: String}

type Resource = String

instance showTriple :: Show a => Show (Triple a) where
  show (Triple t@{}) = "<" <> show t.subject <> ", " <> t.predicate <> ", " <> show t.object <> ">"

-- Hier hoort een specifieke interpretatie bij voor Triple a, namelijk dat het predicate "nameOf" is.
pasToe :: forall a b. Triple (a -> Maybe b) -> Triple a -> Triple b
pasToe (Triple{subject: predicate, object: Just fun}) (Triple{subject, object: Just val}) =
  case fun val of
    Nothing ->
      Triple { subject: subject
              , predicate: predicate
              , object: Nothing
              , supports: []
              , dependencies: []
              }
    (Just v) ->
      Triple { subject: subject
              , predicate: predicate
              , object: Just v
              , supports: []
              , dependencies: []
              }
pasToe (Triple{subject: predicate, object: Nothing}) (Triple{subject}) =
  Triple { subject: subject
          , predicate: "No function value given for " <> predicate
          , object: Nothing
          , supports: []
          , dependencies: []
            }
pasToe (Triple{subject: predicate, object: Just fun}) (Triple{subject, object: Nothing}) =
  Triple { subject: subject
          , predicate: predicate
          , object: Nothing
          , supports: []
          , dependencies: []
            }

compose :: forall a b c. Triple (a -> Maybe b) -> Triple (b -> Maybe c) -> Triple (a -> Maybe c)
compose (Triple{subject: fname, object: Just f}) (Triple{subject: gname, object: Just g}) =
  Triple { subject: fname <> "." <> gname
          , predicate: "nameOf"
          , object: Just (f >=> g)
          , supports: []
          , dependencies: []
          }
compose (Triple{subject: fname, object: _}) (Triple{subject: gname, object: _}) =
  Triple { subject: fname <> "." <> gname
          , predicate: "nameOf"
          , object: Nothing
          , supports: []
          , dependencies: []
          }

-- Wat levert een PropertyGetter op? Triple. Dus property p toegepast op resource r levert op:
-- <r p <een waarde> <supports> <dependencies> >
-- Maar hier zijn beide in een Array verpakt.
-- Het bovenstaande klopt dus niet.

type PropertyGetter a = Resource -> Maybe a

type TripleGetter a = Triple (PropertyGetter a)

type ResourceTriple = Triple Resource

createNamedValue :: forall a. String -> a -> Triple a
createNamedValue name value =
  Triple { subject: name
          , predicate: "nameOf"
          , object: Just value
          , supports: []
          , dependencies: []
          }

-- runquery :: forall a. TripleGetter a -> ResourceTriple -> Triple a
-- runquery = apply
--
resourceNameLength' :: PropertyGetter Int
resourceNameLength' = Just <<< length
--
resourceNameLength :: TripleGetter Int
resourceNameLength = createNamedValue "resourceNameLength" resourceNameLength'
--
aap :: Triple String
aap = createNamedValue "aap1" "aap2"
--
test :: Triple Int
test = pasToe resourceNameLength aap
-- <aap, resourceNameLength, 3>
--
double :: TripleGetter String
double = createNamedValue "double" \s -> Just (s <> s)

noot :: Triple String
noot = createNamedValue "noot" "noot"
-- <noot, nameOf, "noot">

test2 :: Triple String
test2 = pasToe double noot
-- <noot, double, "nootnoot">
--
test3 :: Triple Int
test3 = pasToe resourceNameLength (pasToe double noot)
-- <noot, resourceNameLength, 8>

test4 = pasToe (compose double resourceNameLength) noot
