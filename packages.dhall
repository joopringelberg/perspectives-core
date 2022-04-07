{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
  with halogen-vdom.dependencies = [ "extra-dependency" ] # halogen-vdom.dependencies
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.7-20220404/packages.dhall
        sha256:75d0f0719f32456e6bdc3efd41cfc64785655d2b751e3d080bd849033ed053f2

in  upstream
  with aff-sockets =
    { dependencies =
      [ "prelude"
      , "aff"
      , "aff-coroutines"
      , "foreign-generic"
      ]
    , repo =
       "https://github.com/joopringelberg/purescript-aff-sockets.git"
    , version =
        "v2.2.1"
    }

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
        "v2.1.0"
    }

  with kishimen =
    { dependencies =
      [ "prelude"
      , "variant"
      , "typelevel-prelude"
      ]
    , repo =
       "https://github.com/justinwoo/purescript-kishimen.git"
    , version =
        "v2.0.0"
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
