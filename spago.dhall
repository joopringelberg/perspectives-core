{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "perspectives-core"
, dependencies =
  [ "aff"
  , "aff-coroutines"
  , "aff-promise"
  , "affjax"
  , "apitypes"
  , "argonaut"
  , "arraybuffer"
  , "arraybuffer-builder"
  , "arraybuffer-types"
  , "arrays"
  , "avar"
  , "avar-monadask"
  , "b64"
  , "console"
  , "control"
  , "coroutines"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-generic"
  , "foreign-object"
  , "free"
  , "functions"
  , "http-methods"
  , "identity"
  , "integers"
  , "iterable"
  , "js-date"
  , "lists"
  , "lrucache"
  , "math"
  , "maybe"
  , "media-types"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-fs-aff"
  , "node-path"
  , "node-process"
  , "now"
  , "nullable"
  , "ordered-collections"
  , "parsing"
  , "partial"
  , "perspectives-couchdb"
  , "perspectives-utilities"
  , "prelude"
  , "profunctor-lenses"
  , "random"
  , "record"
  , "safe-coerce"
  , "serializablenonemptyarray"
  , "simple-json"
  , "strings"
  , "subtlecrypto"
  , "tailrec"
  , "test-unit"
  , "transformers"
  , "tuples"
  , "uint"
  , "unicode"
  , "unsafe-coerce"
  , "web-encoding"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/*.purs" ]
}
