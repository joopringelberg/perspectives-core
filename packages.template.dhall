
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
        "AVAR_MONADASK"
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
        "APITYPES"
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
      , "ordered-collections"
      ]
    , repo =
        "https://github.com/joopringelberg/perspectives-utilities.git"
    , version =
        "UTILITIES"
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
        "SERIALIZABLENONEMPTYARRAY"
    }

  with affjax.repo = "https://github.com/joopringelberg/purescript-affjax.git"
  with affjax.version = "AFFJAX"

  with parsing.repo = "https://github.com/joopringelberg/purescript-parsing.git"
  with parsing.version = "PARSING"

  with iterable =
    { dependencies =
      [ "prelude"
      ]
    , repo =
       "https://github.com/Risto-Stevcev/purescript-iterable.git"
    , version =
        "v2.0.0"
    }

  with lrucache =
    { dependencies =
      [ "console"
      , "effect"
      , "foreign"
      , "free"
      , "iterable"
      , "maybe"
      , "prelude"
      , "simple-json"
      , "strings"
      , "test-unit"
      , "tuples"
      , "unsafe-coerce"
      ]
    , repo = "https://github.com/joopringelberg/purescript-lru-cache.git"
    , version = "LRUCACHE"}
