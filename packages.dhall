
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.7-20220404/packages.dhall
        sha256:75d0f0719f32456e6bdc3efd41cfc64785655d2b751e3d080bd849033ed053f2

in  upstream
  -- with aff-sockets =
  --   { dependencies =
  --     [ "prelude"
  --     , "aff"
  --     , "aff-coroutines"
  --     , "foreign-generic"
  --     ]
  --   , repo =
  --      "https://github.com/joopringelberg/purescript-aff-sockets.git"
  --   , version =
  --       "v2.2.1"
  --   }

  with avar-monadask =
    { dependencies =
      [ "prelude"
      , "avar"
      , "aff"
      , "transformers"
      ]
    , repo =
       "https://github.com/joopringelberg/purescript-avar-monadask.git"
    , version =
        "v2.1.1"
    }

  with kishimen =
    { dependencies =
      [ "prelude"
      , "variant"
      , "typelevel-prelude"
      ]
    , repo =
       "https://github.com/joopringelberg/purescript-kishimen.git"
    , version =
        "v1.1.0"
    }

  with apitypes =
    { dependencies =
      [ "console"
      , "effect"
      , "foreign-generic"
      , "prelude"
      , "simple-json"
      , "perspectives-utilities"
      , "serializablenonemptyarray"
      , "either"
      , "foreign"
      , "foreign-object"
      , "maybe"
      , "newtype"
      , "partial"
      , "transformers"
      , "unsafe-coerce"
      ]
    , repo =
       "https://github.com/joopringelberg/perspectives-apitypes.git"
    , version =
        "v2.13.2"
    }

  with perspectives-utilities =
    { dependencies =
      [ "console"
      , "effect"
      , "prelude"
      , "foreign-object"
      , "arrays"
      , "foldable-traversable"
      , "maybe"
      , "transformers"
      , "tuples"
      ]
    , repo =
        "https://github.com/joopringelberg/perspectives-utilities.git"
    , version =
        "v1.0.0"
    }

  with serializablenonemptyarray =
    { dependencies =
      [ "console"
      , "effect"
      , "prelude"
      , "perspectives-utilities"
      , "foreign"
      , "simple-json"
      , "foreign-generic"
      , "arrays"
      , "either"
      , "foldable-traversable"
      , "lists"
      , "maybe"
      , "newtype"
      , "transformers"
      ]
    , repo =
        "https://github.com/joopringelberg/serialisable-nonempty-arrays.git"
    , version =
        "v1.0.1"
    }

  with perspectives-couchdb =
    { dependencies =
      [ "console"
      , "effect"
      , "prelude"
      , "avar-monadask"
      , "affjax"
      , "argonaut"
      , "foreign-generic"
      , "simple-json"
      , "b64"
      , "test-unit"
      , "aff-promise"
      , "aff"
      , "aff-coroutines"
      , "arrays"
      , "avar"
      , "coroutines"
      , "either"
      , "exceptions"
      , "foldable-traversable"
      , "foreign"
      , "foreign-object"
      , "free"
      , "http-methods"
      , "maybe"
      , "media-types"
      , "newtype"
      , "ordered-collections"
      , "partial"
      , "strings"
      , "tailrec"
      , "transformers"
      , "tuples"
      , "unsafe-coerce"
      ]
    , repo =
        "https://github.com/joopringelberg/perspectives-couchdb.git"
    , version =
        "v2.8.5"
    }

  with affjax.repo = "https://github.com/joopringelberg/purescript-affjax.git"
  with affjax.version = "v12.0.0-with-xhr-cookies"

  with parsing.repo = "https://github.com/joopringelberg/purescript-parsing.git"
  with parsing.version = "v7.0.0-transformer-tagged"

  with lrucache =
    { dependencies =
      ["prelude"]
    , repo = "https://github.com/joopringelberg/purescript-lru-cache.git"
    , version = "v1.0.0"}
